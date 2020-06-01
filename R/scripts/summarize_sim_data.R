# Contagion
# Converts raw sim data to summary data

# libraries and includes -----------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
source("../scripts/load_sim_data.R")

# data preparation -------------------------------------------------------------------------------
prepare_rdata <- function(model_root_dir, force = FALSE) {
  # person - cycle cartesian join
  person_cycles <- function(parms) {
    pc <- expand.grid(cycle = 0:(parms$cycles - 1), 
                id = 0:(parms$world_parms$population_size - 1))
  }
  
  # world - cycle cartesian join
  world_cycles_status <- function(parms){
    expand.grid(cycle = 0:(parms$cycles - 1), 
                cell = 0:(parms$world_parms$rows * parms$world_parms$cols - 1), 
                status = disease_status_factors())
  }
  
  # personal history
  person_history <- function(scenario_dir, pc) {
    cat("   creating personal histories ... \n")
    creations <-
      read_create_log(scenario_dir) %>%
      rename(create_cell = current_cell, create_status = disease_status) %>%
      select(cycle, id, create_cell, create_status)
    moves <- read_move_log(scenario_dir)
    diseases <-
      read_disease_log(scenario_dir) %>% select(-current_cell)
    
    dsf <- disease_status_factors()
    ph <- merge(data.table(pc), creations, by = c("cycle", "id"), all.x = TRUE)
    ph <- merge(ph, moves, by = c("cycle", "id"), all.x = TRUE)    
    ph <- merge(ph, diseases, by = c("cycle", "id"), all.x = TRUE)
    ph[, cell := ifelse(cycle == 0, create_cell, to_cell)]
    ph[, status := dsf[ifelse(cycle == 0, create_status, disease_status)]]
    ph$status <- factor(ph$status, ordered = TRUE, levels = dsf)
    ph <- ph %>% group_by(id) %>% arrange(cycle) %>% fill(cell, status) %>%
      select(cycle, id, cell, status) %>% ungroup()
    ph    
    
    # ph <-
    #   pc %>% left_join(creations, by = c("cycle", "id")) %>%
    #   left_join(moves, by = c("cycle", "id")) %>%
    #   left_join(diseases, by = c("cycle", "id")) %>%
    #   mutate(cell = ifelse(cycle == 0, create_cell, to_cell)) %>%
    #   mutate(status = dsf[ifelse(cycle == 0, create_status, disease_status)]) %>%
    #   group_by(id) %>% arrange(cycle) %>% fill(cell, status) %>%
    #   select(cycle, id, cell, status) %>% ungroup()
    # 
  }
  
  cell_cycle_reconciliation <- function(s) {
    cat("   cell cycle reconciliation ... ")
    
    # data table are faster and more memory efficient
    ph <- data.table(load_scenario_data(s, "person_history"))
    setkey(ph, id, cycle)
    ph[, next_cycle := cycle + 1]
    ph <- ph[ph, on=.(cycle = next_cycle, id=id)][,next_cycle:=NULL]
    setnames(ph, c("cycle", "cell", "status", "i.cycle", "i.cell", "i.status"),
             c("cycle", "cell", "status", "prev_cycle", "prev_cell", "prev_status"))
    
    cat("ph ... ")
    ph_departures <- ph %>% 
      filter(prev_cell != cell) %>% 
      group_by(prev_cell, status, cycle) %>% 
      summarize(num = -n()) %>% 
      mutate(x_type = "Dep") %>% 
      ungroup() %>% 
      rename(cell = prev_cell) %>% 
      filter(!is.na(cycle))
    cat("dep ... ")
    ph_arrivals <- ph %>% 
      filter(prev_cell != cell) %>% 
      group_by(cell, status, cycle) %>% 
      summarize(num = n()) %>% 
      ungroup() %>% 
      mutate(x_type = "Arr") %>% 
      filter(!is.na(cycle))
    cat("arr ... ")
    ph_stay <- ph %>% 
      filter(prev_cell == cell) %>% 
      group_by(cell, status, cycle) %>% 
      summarize(num = n()) %>% 
      ungroup() %>% 
      mutate(x_type = "Stay")
    cat("stay ... ")
    ph_disease_from <- ph %>%
      group_by(cell, status, cycle, prev_status) %>% 
      summarize(num = n()) %>% 
      ungroup() %>% 
      rename(x_type = status, status = prev_status)
    cat("d_to ... ")
    ph_disease_to <- ph_disease_from %>% 
      mutate(num = num * -1) %>% 
      rename(temp = x_type) %>% 
      mutate(x_type = status) %>% 
      mutate(status = temp) %>% 
      select(-temp)
    cat("d_from ... ")
    rm(ph) # save memory ph can be huge
    ccr <- rbind(ph_arrivals, ph_departures, ph_stay, ph_disease_from, ph_disease_to)
    
    x_type_levels <- c("Arr", "Dep", "Stay", levels(ccr$status))
    ccr$x_type <- factor(ccr$x_type, levels = x_type_levels, ordered = TRUE)
    cat("done\n")
    ccr
  }
  
  cell_cycle_transitions <- function(scenario_dir) {
    cat("   cell cycle transitions ... \n")
    diseases <-
      read_disease_log(scenario_dir) %>% rename(cell = current_cell)
    cct <- diseases %>% group_by(cycle, cell, disease_status, .drop=TRUE) %>% 
      summarize(disease_transitions = n()) %>% ungroup()
    
    # a transition to "Well" status is recoded as a recovery
    cct$disease_status <-
      recode_factor(cct$disease_status, Well = "Recovered")
    cct$disease_status <-
      fct_relevel(cct$disease_status, "Recovered", after = 3)
    
    cct
  }
  
  cell_cycle_status <- function(s, wcs, rows, cols) {
    cat("   cell cycle status ... \n")
    ph <- load_scenario_data(s, "person_history")
    ccs <- ph %>% group_by(cycle, cell, status) %>% summarize(cell_status_pop = n()) %>% 
      ungroup()
    rm(ph) # save memory ph can be huge
    
    ccs <- wcs %>% left_join(ccs, by = c("cycle", "cell", "status")) %>% 
      mutate(row = cell %/% cols, col = cell %% cols) %>% 
      mutate(cell_status_pop = replace_na(cell_status_pop, 0)) %>% 
      group_by(cycle, cell) %>% 
      mutate(cell_pop = sum(cell_status_pop)) %>% 
      group_by(status, add = TRUE) %>% 
      mutate(cell_status_pop_pct = if_else(cell_pop==0, 0, cell_status_pop / cell_pop)) %>% 
      ungroup()
    ccs$row <- factor(ccs$row, ordered = TRUE, levels = 0:(rows-1))
    ccs$col <- factor(ccs$col, ordered = TRUE, levels = 0:(cols-1))
    
    ccs
  }
  
  cell_cycle <- function(ccs) {
    cat("   cell cycle ... \n")
    ccs %>% group_by(cycle, cell, row, col) %>% 
      summarize(cell_pop = sum(cell_status_pop)) %>% 
      group_by(cycle) %>% 
      mutate(cycle_pop = sum(cell_pop)) %>% 
      group_by(cell, row, col, add = TRUE) %>% 
      mutate(cell_pop_pct = cell_pop / cycle_pop) %>% 
      ungroup()
  }
  
  cycle_status <- function(ccs) {
    cat("   cycle status ... \n")
    ccs %>% group_by(cycle, status) %>% 
      summarize(status_pop = sum(cell_status_pop)) %>% ungroup()
  }
  
  # main - loop models scenarios and save intermediate results
  source("../scripts/load_sim_data.R")
  model_dirs <- model_dir_list()
  for (m in model_dirs) {
    if (!rdata_up_to_date(m) || force) {
      model_parms <- read_model_parameters(m)
      pc <- person_cycles(model_parms)
      wcs <- world_cycles_status(model_parms)
      scenario_dirs <- scenario_dir_list(m)
      
      cct_list <- list()
      cs_list <- list()
      for (s in scenario_dirs) {
        cat(paste0(s, "\n"))
        ph <- person_history(s, pc)
        save_scenario_data(s, "person_history", ph)
        rm(ph) # save memory ph can be huge
        
        ccs <- cell_cycle_status(s, wcs, model_parms$world_parms$rows, model_parms$world_parms$cols)
        ccr <- cell_cycle_reconciliation(s)
        
        save_scenario_data(s, "cell_cycle_reconciliation", ccr)
        save_scenario_data(s, "cell_cycle_status", ccs)
        save_scenario_data(s, "cell_cycle", cell_cycle(ccs))
        
        cs <- cycle_status(ccs)
        rm(ccs)
        save_scenario_data(s, "cycle_status", cs)
        cs_list <- c(cs_list, list(cs))
        
        cct <- cell_cycle_transitions(s)
        save_scenario_data(s, "cell_cycle_transitions", cct)
        cct_list <- c(cct_list, list(cct))
      }
      
      # combine the transition files adding scenario column
      if (length(cct_list) > 0) {
        m_cct <- cct_list[[1]] %>% mutate(scenario = 1)
        m_cs <- cs_list[[1]] %>% mutate(scenario = 1)
        for (s_index in 2:length(cct_list)) {
          m_cct <- rbind(m_cct, cct_list[[s_index]] %>% mutate(scenario = s_index))
          m_cs <- rbind(m_cs, cs_list[[s_index]] %>% mutate(scenario = s_index))
        }
        save_model_data(m, "cell_cycle_transitions", m_cct)
        save_model_data(m, "cycle_status", m_cs)
      }
    }
  }
}