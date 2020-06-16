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
    wcs <- expand.grid(cycle = 0:(parms$cycles - 1), 
                cell = 0:(parms$world_parms$rows * parms$world_parms$cols - 1), 
                status = disease_status_factors())
    wcs$status <- factor(wcs$status, ordered = TRUE, disease_status_factors())
    data.table(wcs)
  }
  
  # personal history
  person_history <- function(scenario_dir, pc) {
    cat("   creating personal histories ... ")
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
    ph <- ph %>% 
      group_by(id) %>% 
      arrange(cycle) %>% 
      fill(cell, status) %>%
      select(cycle, id, cell, status) %>% 
      ungroup()
    ph <- data.table(ph)
    setkey(ph, cycle, cell, status)
    
    cat("done\n")
    ph
  }
  
  cell_cycle_reconciliation <- function(m, s) {
    cat("   cell cycle reconciliation ... ")
    
    # data tables are faster and more memory efficient than tibbles
    ph <- data.table(load_scenario_data(s, "person_history"))
    setkey(ph, id, cycle)
    ph[, `:=` (prev_cycle = cycle - 1, next_cycle = cycle + 1)]
    ph <- merge(ph, 
                ph[,.(id, next_cycle, prev_cell = cell, prev_status = status)], 
                by.x = c("id", "cycle"),
                by.y = c("id", "next_cycle"),
                all.x = TRUE)[,next_cycle := NULL]
    
    cat("ph ... ")
    ph_create <- ph[prev_cycle == -1, .(num = .N, x_type = "Create"), by = .(cell, status, cycle)]
    cat("create ... ")
    ph_departures <- ph[cell != prev_cell, .(num = -(.N), x_type = "Dep"), by = .(prev_cell, status, cycle)] %>% 
      rename(cell = prev_cell)
    cat("dep ... ")
    ph_arrivals <- ph[cell != prev_cell, .(num = .N, x_type = "Arr"), by = .(cell, status, cycle)]
    cat("arr ... ")
    ph_disease_from <- ph[status != prev_status & cell == prev_cell, .(num = -.N), by = .(cell, status, cycle, prev_status)] %>% 
      rename(x_type = status, status = prev_status)
    cat("status_from ... ")
    ph_disease_to <- (ph_disease_from[
      ,.(cell, cycle, num = -num, temp = x_type, x_type = status, status = x_type)][,temp:=NULL])
    cat("status_to ... ")
    
    rm(ph) # save memory ph can be huge

    ccr <- rbind(ph_create, ph_arrivals, ph_departures, ph_disease_from, ph_disease_to)
    x_type_levels <- c("Create", "Arr", "Dep", levels(ccr$status))
    ccr$x_type <- factor(ccr$x_type, levels = x_type_levels, ordered = TRUE)
    
    ccr_widened <- 
      data.table(ccr %>% 
                   pivot_wider(id_cols = c(cell, cycle, status), 
                               names_prefix = "from_", 
                               names_from = x_type, 
                               values_from = num, 
                               values_fn =  list(num = sum)) %>% 
                   replace(is.na(.), 0) %>% 
                   mutate(delta = rowSums(.[4:ncol(.)]))
      )
    cat("widen ... ")
    
    ccr_expanded <- merge(data.table(
      expand.grid(cell = cells(m), 
                  status = disease_status_factors(), 
                  cycle = cycles(m))),
      ccr_widened, 
      by = c("cell", "cycle", "status"),
      all.x = TRUE)
    for(j in seq_along(ccr_expanded)){
      set(ccr_expanded, i = which(is.na(ccr_expanded[[j]]) & is.numeric(ccr_expanded[[j]])), j = j, value = 0)
    }
    cat("expand ... ")
    rm(ccr_widened)
    
    setkey(ccr_expanded, cell, status, cycle)
    ccr_expanded[, pop := cumsum(delta), by = .(cell, status)]
    
    cat("done\n")
    ccr_expanded
  }
  
  cell_cycle_transitions <- function(scenario_dir) {
    cat("   cell cycle transitions ... ")
    diseases <- data.table(read_disease_log(scenario_dir))[,`:=` (cell = current_cell, current_cell = NULL)]
    cct <- diseases[, disease_transitions := .N, by = c("cycle", "cell", "disease_status")]
    
    # a transition to "Well" status is recoded as a recovery
    cct$disease_status <-
      recode_factor(cct$disease_status, Well = "Recovered")
    cct$disease_status <-
      fct_relevel(cct$disease_status, "Recovered", after = 3)
    cat("done\n")
    cct    
  }
  
  cell_cycle_status <- function(s, wcs, rows, cols) {
    cat("   cell cycle status ... ")
    ph <- load_scenario_data(s, "person_history")
    # ccs <- ph[, cell_status_pop := .N, by = .(cycle, cell, status)]
    ccs <- ph[, .(cell_status_pop = .N), by = .(cycle, cell, status)][
      , cell_pop := sum(cell_status_pop), by = .(cycle, cell)][
        , cell_status_pop_pct := if_else(cell_pop==0, 0, cell_status_pop / cell_pop)][
          ,`:=` (row = cell %/% cols, col = cell %% cols)]
    rm(ph) # save memory ph can be huge
    
    ccs$row <- factor(ccs$row, ordered = TRUE, levels = 0:(rows-1))
    ccs$col <- factor(ccs$col, ordered = TRUE, levels = 0:(cols-1))
    
    cat("done\n")
    ccs
  }
  
  cell_cycle <- function(ccs) {
    cat("   cell cycle ... ")
    # ccs %>% group_by(cycle, cell, row, col) %>% 
    #   summarize(cell_pop = sum(cell_status_pop)) %>% 
    #   group_by(cycle) %>% 
    #   mutate(cycle_pop = sum(cell_pop)) %>% 
    #   group_by(cell, row, col, .add = TRUE) %>% 
    #   mutate(cell_pop_pct = cell_pop / cycle_pop) %>% 
    #   ungroup()
    cc <- ccs[, .(cell_pop = last(cell_pop)), by = .(cycle, cell, row, col)][
      , cycle_pop := sum(cell_pop), by = .(cycle)][
        , cell_pop_pct :=  cell_pop / cycle_pop]
    
    cat("done\n")
    cc
  }
  
  cycle_status <- function(ccs) {
    cat("   cycle status ... ")
    # ccs %>% group_by(cycle, status) %>% 
    #   summarize(status_pop = sum(cell_status_pop)) %>% ungroup()
    cc <- ccs[, .(status_pop = sum(cell_status_pop)), by = .(cycle, status)]
    cat("done\n")
    cc
  }
  
  # main - loop models scenarios and save intermediate results ---------------
  source("../scripts/load_sim_data.R")
  options(dplyr.summarise.inform = FALSE) # suppress stupid dplyr summarise msg
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
        ccr <- cell_cycle_reconciliation(m, s)
        
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