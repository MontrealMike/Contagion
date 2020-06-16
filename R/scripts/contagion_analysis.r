# libraries and includes -----------------------------------------------------------------------------------------
library(tidyverse)
library(tidytidbits)
library(ggplot2)
library(stringi)
library(knitr)
library(kableExtra)
source("../scripts/load_sim_data.R")


# Shiny outputs ----------------------------------------------------------------------------------------------

model_list <- function() {
  names(model_dir_list())
}

scenario_list <- function(model_dir) {
  sd <- scenario_dir_list(model_dir)
  names(sd) <- basename(sd)
  sd
}

model_parameters <- function(model_dir) {
  parms <- read_model_parameters(model_dir)
  model_desc_parms <-
    data.frame(
      Parameter = c("Name", "Description", "Cycles"),
      Value = c(parms$model_name, parms$model_description, parms$cycles)
    )
  world_parms <-
    data.frame(
      Parameter = c("Rows", "Cols", "Population"),
      Value = c(
        parms$world_parms$rows,
        parms$world_parms$cols,
        parms$world_parms$population_size
      )
    )
  sensitivity_parms <-
    data.frame(
      Parameter = c("Variable", "Statistic" , "Values"),
      Value = c(
        parms$scenario_parms$scenario_variable,
        parms$scenario_parms$scenario_stat_type,
        paste(parms$scenario_parms$scenario_values, collapse = ", ")
      )
    )
  beta_dist_parms <- data.frame(
    Variable =
      c("Mobility", "Exposure", "Compliability", "Immunity loss"),
    Mean = c(
      parms$sim_parms$mobility$mean,
      parms$sim_parms$exposure$mean,
      parms$sim_parms$compliability$mean,
      parms$sim_parms$immunity_loss$mean
    ),
    Population = c(
      parms$sim_parms$mobility$sample_population,
      parms$sim_parms$exposure$sample_population,
      parms$sim_parms$compliability$sample_population,
      parms$sim_parms$immunity_loss$sample_population
    )
  )
  pert_dist_parms <-
    data.frame(
      Variable = c("Infected", "Symptomatic", "Hospitalized"),
      Mode = c(
        parms$sim_parms$days_infected$mode,
        parms$sim_parms$days_symptomatic$mode,
        parms$sim_parms$days_hospitalized$mode
      ),
      Min = c(
        parms$sim_parms$days_infected$min,
        parms$sim_parms$days_symptomatic$min,
        parms$sim_parms$days_hospitalized$min
      ),
      Max = c(
        parms$sim_parms$days_infected$max,
        parms$sim_parms$days_symptomatic$max,
        parms$sim_parms$days_hospitalized$mode
      )
    )
  vuln_dist_parms <-
    data.frame(
      Sequence = c("W>I>W", "W>I>S>W", "W>I>S>H>W", "W>I>S>D", "W>I>S>H>D"),
      Weighting = c(
        parms$sim_parms$vulnerability$weightings[[1]],
        parms$sim_parms$vulnerability$weightings[[2]],
        parms$sim_parms$vulnerability$weightings[[3]],
        parms$sim_parms$vulnerability$weightings[[4]],
        parms$sim_parms$vulnerability$weightings[[5]]
      )
    )
  list(
    model_desc_parms = model_desc_parms,
    world_parms = world_parms,
    sensitivity_parms = sensitivity_parms,
    beta_dist_parms = beta_dist_parms,
    pert_dist_parms = pert_dist_parms,
    vuln_dist_parms = vuln_dist_parms
  )
}

scenario_comparison_tables <- function(model_dir) {
  parms <- read_model_parameters(model_dir)
  sensitivity_var_type <-
    paste(
      parms$scenario_parms$scenario_variable,
      parms$scenario_parms$scenario_stat_type,
      sep = "_"
    )
  cct <- load_model_data(model_dir, "cell_cycle_transitions")
  
  scenario_summary <-
    cct %>% group_by(scenario, disease_status) %>%
    summarize(transitions = sum(disease_transitions)) %>%
    ungroup()  %>%
    mutate(transition_pct = transitions / parms$world_parms$population_size) %>% 
    mutate(transitions_fmt = format(transitions, big.mark = ",", nsmall = 0)) %>% 
    mutate(transition_pct_fmt = paste0(format(transition_pct * 100, nsmall=2), "%"))
  scenario_status <- expand.grid(scenario = unique(scenario_summary$scenario),
                                 disease_status = levels(scenario_summary$disease_status))
  scenario_table_abs <-
    scenario_status %>% left_join(scenario_summary, by = c("scenario", "disease_status")) %>% 
    arrange(scenario, disease_status) %>%
    pivot_wider(id_cols = scenario,
                names_from = disease_status,
                values_from = transitions_fmt) %>%
    mutate(!!sensitivity_var_type := parms$scenario_parms$scenario_values) %>%
    select(scenario,!!sensitivity_var_type, everything())
  scenario_table_pct <-
    scenario_status %>% left_join(scenario_summary, by = c("scenario", "disease_status")) %>% 
    arrange(scenario, disease_status) %>%
    pivot_wider(id_cols = scenario,
                names_from = disease_status,
                values_from = transition_pct_fmt) %>%
    mutate(!!sensitivity_var_type := parms$scenario_parms$scenario_values) %>%
    select(scenario,!!sensitivity_var_type, everything())
  list(scenario_table_abs = scenario_table_abs,
       scenario_table_pct = scenario_table_pct)
}

population_reconciliation_table <- function(prt, cycle, cell = NULL){
  
  # select and summarize data
  prt <- if (!is.null(cell)) {
    prt %>% 
      filter(cell == !!cell & cycle == !!cycle) %>% 
      select(-cell, -cycle)
    
  } else {
    prt %>% 
      filter(cycle == !!cycle) %>% 
      select(-cell, -cycle) %>% 
      group_by(status) %>% 
      summarize_all(funs(sum))
  }
  
  # turn rows into columns and tidy up names
  prt <- prt %>% 
    mutate(`Start pop` = pop - delta) %>% 
    rename(`Net change` = delta, `End pop` = pop) %>% 
    pivot_longer(cols = c(`Start pop`, starts_with("from_"), `Net change`, `End pop`), values_to = "num") %>% 
    pivot_wider(id_cols = name, names_from = status, values_from = num) %>% 
    mutate(name = str_replace(name, "from_", ""))
  
  # add a total column
  prt <- prt %>% mutate(Total = rowSums(.[2:ncol(.)])) %>% 
    mutate(across(is.numeric, comma, accuracy = 1))
  return(prt)
  
  # turn into table with exactly 8 rows and 5 columns
  status_levels <- levels(prt$status)
  x_type_levels <- levels(prt$x_type)
  prt <- expand_grid(x_type = x_type_levels,
                     status = status_levels) %>%
    left_join(prt) %>%
    mutate(num=replace_na(num, 0)) %>% 
    pivot_wider(id_cols = x_type, names_from = status, values_from = num) %>% 
    select(-x_type)

    
  # add summary rows and columns - our final matrix will have 10 rows
  prt_init <- as.matrix(prt)
  prt_full <- matrix(0, ncol = 5, nrow = 10)
  new_rows <- c(1,4,10)
  # prt_full[-new_rows,] <- prt_init[-3,]          # copy in all rows except Stays
  # prt_full[10,] <- colSums(prt_init) - prt_init[2,]   # End count is sum of all rows except departures
  # prt_full[4,] <- prt_full[10,] - colSums(prt_init[4:8,])  # Sub total is end count - disease x_types
  # prt_full[1,] <- prt_full[4,] - colSums(prt_init[1:2,])   # Start count is sub total - net arrivals & departures
  # prt_full <- cbind(prt_full, rowSums(prt_full))
  
  prt_full[-new_rows,] <- prt_init[-3,]                    # copy in all rows except Stays
  prt_full[10,] <- colSums(prt_init)                       # End count is sum of all rows except departures
  prt_full[4,] <- prt_full[10,] - colSums(prt_init[4:8,])  # Sub total is end count - disease x_types
  prt_full[1,] <- prt_full[4,] - colSums(prt_init[1:2,])   # Start count is sub total - net arrivals & departures
  prt_full <- cbind(prt_full, rowSums(prt_full))
  
  # format data
  prt_full_format <- format(prt_full, big.mark = ",", )
  
  # add row and column names
  rownames(prt_full_format) <- c("Cycle start pop",
                           "Arrivals",
                           "Departures",
                           "Subtotal",
                           status_levels,
                           "Cycle end pop")
  colnames(prt_full_format) <- c(status_levels, "Total")
  
  prt_full_format
  
}

# Tests  -------------------------------------------------------
test_population_reconciliation_table <- function(prt=NULL, m = 1, s = 1, 
                                                 cycle = 1, cell = 0) {
  if (is.null(prt)) {
    model_dir <- model_dir_list()[[m]]
    scenario_dir <- scenario_dir_list(model_dir)[[s]]
    prt <- load_scenario_data(scenario_dir, "cell_cycle_reconciliation")
  }
  population_reconciliation_table(prt, cycle, cell)
}

test_scenario_comparison_tables <- function() {
  model_dir <- model_dir_list()[[1]]
  sct <- scenario_comparison_tables(model_dir)
  sct
}

test_model_parameters <- function() {
  model_dir <- model_dir_list()[[1]]
  model_parameters(model_dir)
}

test_prepare_data <- function() {
  model_dir <- model_dir_list()[[1]]
  scenario_dir <- scenario_dir_list(model_dir)[[3]]
  prepare_rdata(model_dir, scenario_dir)
}

test_disease_evolution_chart <- function() {
  model_dir <- model_dir_list()[[1]]
  scenario_dir <- scenario_dir_list(model_dir)[[3]]
  disease_evolution_plot(model_dir, scenario_dir)
}

test_cell_heatmap <- function() {
  model_dir <- model_dir_list()[[1]]
  scenario_dir <- scenario_dir_list(model_dir)[[3]]
  heatmap <- cell_heatmap(model_dir, scenario_dir)
}

test_population_distribution_analysis <- function(){
  model_dir <- model_dir_list()[[1]]
  scenario_dir <- scenario_dir_list(model_dir)[[2]]
  model_parms <-read_model_parameters(model_dir)
  population_distribution_analysis(scenario_dir,  
                                   model_parms$world_parms$rows,
                                   model_parms$world_parms$cols)
}

test_person_disease_evolution <- function() {
  model_dir <- model_dir_list()[[2]]
  scenario_dir <- scenario_dir_list(model_dir)[[5]]
  person_disease_evolution(scenario_dir)  
}


# Analysis -----------------------------------------------------
person_disease_evolution <- function(scenario_dir) {
  c_log <- read_create_log(scenario_dir) %>% select(id, start_cell = current_cell, vulnerability)
  d_log <- read_disease_log(scenario_dir) %>% select(id, cycle, current_cell, disease_status)
  cd_log <- c_log %>% inner_join(d_log, by = "id")
  cd_log = rbind(c_log %>% mutate(current_cell = start_cell, cycle = 0, disease_status = "Well"),
                 cd_log)
  cd_log_analysis <- cd_log %>% 
    arrange(id, cycle) %>% 
    mutate(days = if_else(cycle == 0, 0, (cycle - lag(cycle)))) %>% 
    group_by(id, vulnerability) %>% 
    mutate(status_changes = n() - 1, first_status = first(disease_status), last_status = last(disease_status))
  
  cd_log_analysis_2 <- cd_log_analysis %>% 
    group_by(id, vulnerability) %>% 
    summarize(status_changes = n() - 1, 
              first_status = first(disease_status), 
              last_status = last(disease_status),
              last_change_cycle = last(cycle))
  
  v_plot <- ggplot(c_log, aes(x=factor(vulnerability))) +
    geom_bar(stat = "count")
  
  list(detail_log = cd_log, days = cd_log_analysis, fate = cd_log_analysis_2, v_plot = v_plot)
}


population_distribution_analysis <- function(scenario_dir, rows, cols, cycle = 0, disease_status = "Well"){
  
  # non vectorized version
  neighbour_cells_nv <- function(cell, rows, cols) {
    get_index <- function(row, col) {
      row * cols + col
    }
    row <- cell %/% cols
    col <- cell %% cols
    n <- if (row == 0) {                                        #top row
      if (col == 0) {                                           #top left
        c(get_index(row, col+1), 
          get_index(row+1, col), get_index(row+1,col+1))
      } else if (col > 0 & col < cols - 1) {                    #top middle
        c( get_index(row, col-1), get_index(row, col+1), 
           get_index(row+1, col-1), get_index(row+1, col), get_index(row+1,col+1))
      } else {                                                  #top right
        c( get_index(row, col-1), 
           get_index(row+1, col-1), get_index(row+1, col))
      }
    } else if (row > 0 & row < (rows - 1)) {                      #middle row
      if (col == 0) {                                           #middle left
        c( get_index(row-1, col), get_index(row-1, col+1), 
           get_index(row, col+1),
           get_index(row+1, col), get_index(row+1, col+1))
      } else if (col > 0 & col < cols - 1) {                    #middle middle
        c( get_index(row-1, col-1), get_index(row-1, col), get_index(row-1, col+1), 
           get_index(row, col-1), get_index(row, col+1), 
           get_index(row+1, col-1), get_index(row+1, col), get_index(row+1,col+1))
      } else {                                                  #middle right
        c( get_index(row-1, col-1), get_index(row-1, col),
           get_index(row, col-1),
           get_index(row+1, col-1), get_index(row+1, col))
      }
    } else  {                                                   #bottom row
      if (col == 0) {                                           #bottom left
        c(get_index(row-1, col), get_index(row-1, col+1),
          get_index(row, col+1))
      } else if (col > 0 & col < cols - 1) {                    #bottom middle
        c( get_index(row-1, col-1), get_index(row-1, col), get_index(row-1, col+1), 
           get_index(row, col-1), get_index(row, col+1))
      } else {                                                  #bottom right
        c( get_index(row-1, col-1), get_index(row-1, col),
           get_index(row, col-1))
      }
    }
    n
  }
  
  mean_neighbour_pop <- function(pop, rows, cols){
    # vectorized function
    # neighbour_cells_v <- Vectorize(neighbour_cells_nv, "cell")
    n_pop <- NULL
    for (cell in 0:(length(pop) - 1)) {
        n_pop[cell+1] <- mean(pop[neighbour_cells_nv(cell, rows, cols)])
    }
    n_pop
  }
  
  ccs <- load_scenario_data(scenario_dir, "cell_cycle_status")
  cc <- ccs %>% 
    filter(status == disease_status & cycle == !!cycle) %>% 
    group_by(cell) %>% summarize(pop = sum(pop, na.rm = TRUE)) %>% 
    ungroup()
  
  cc$mean_n_pop <- mean_neighbour_pop(cc$pop, rows, cols)
  
  ggplot(cc, aes(x = pop, y = mean_n_pop)) + 
    geom_point(col = "red", size = .05, position = position_jitter()) + 
    geom_smooth()
}
