# Contagion
#
# Load simulation data

# libraries and includes --------------------------------------------------------------
library(yaml)
library(tidyverse)
library(data.table)

# directory structure ----------------------------------------------------------------------
# this is just a convenience for the devlopper --- it should NEVER be called
# by the program code
shiny_directory <- function() {
  setwd("/home/michael/GoogleDrive/Documents/Personal/Development/Contagion/R/shiny")
  print(getwd())
}

# Where to find models
model_root_dir <- function() {
  "/home/michael/GoogleDrive/Documents/Personal/Development/Contagion/data/Model Runs V2"
}

# TODO filter out non valid items and
# check for scenario and parm files
model_dir_list <- function(md = model_root_dir()) {
  path_names <- list.files(md, full.names = TRUE)
  names(path_names) <- basename(path_names)
  path_names
}

read_model_parameters <- function(model_dir) {
  file_name = paste0(model_dir, "/parms.yaml")
  read_yaml(file_name)
}

scenario_dir_list <- function(model_dir) {
  list.files(paste0(model_dir, "/Scenarios"), full.name = TRUE, pattern = "scenario_*")
}

# use the latest date time stamp of the files in the last scenario
model_run_date_time <- function(model_dir) {
  last_scenario = tail(scenario_dir_list(model_dir), 1)
  date_time = max(file.mtime(dir(path = last_scenario, full.names = TRUE)))
  date_time
}

# get model parameter data  -----------------------------------------------
cycles <- function(model_dir) {
  seq(0, read_model_parameters(model_dir)$cycles-1)    
}
people <- function(model_dir) {
  seq(0, read_model_parameters(model_dir)$world_parms$population-1)    
}
cells <- function(model_dir) {
  p <- read_model_parameters(model_dir)
  seq(0, (p$world_parms$rows * p$world_parms$cols)- 1)    
}
disease_status_factors <- function() {
  c("Well", "Infected", "Symptomatic", "Hospitalized", "Dead")
}

# read log files ---------------------------------------------------------
read_create_log <- function(scenario_dir) {
  create_log <-
    read.csv(paste0(scenario_dir, "/person_log_create.csv"))
  # assume people are created "Well"
  create_log$disease_status <- "Well"
  create_log$disease_status <-
    factor(create_log$disease_status,
           ordered = TRUE,
           levels = disease_status_factors())
  create_log
}

read_move_log <- function(scenario_dir) {
  read.csv(paste0(scenario_dir, "/person_log_move.csv"))
}

read_disease_log <- function(scenario_dir) {
  disease_log <-
    read.csv(paste0(scenario_dir, "/person_log_disease.csv"))
  disease_log$new_status <-
    factor(disease_log$new_status,
           ordered = TRUE,
           levels = disease_status_factors())
  disease_log %>% rename(disease_status = new_status)
}

# intermediate data -------------------------------------------------------------------------
# create the directory if need be
model_rdata_directory <- function(model_dir) {
  dir_name <- paste0(model_dir, "/rdata")
  ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
  dir_name
}

model_data_file_names <- function() {
  list(
    cell_cycle_transitions = "cell_cycle_transitions",
    cycle_status = "cycle_status",
    cct = "cell_cycle_transitions",
    cs = "cycle_status"
  )
}

model_data_full_path <- function(model_dir, data_type) {
  paste0(model_rdata_directory(model_dir),"/",model_data_file_names()[data_type], ".rds")
}

save_model_data <- function(model_dir, data_type, data) {
  saveRDS(data, model_data_full_path(model_dir, data_type))
}

load_model_data <- function(model_dir, data_type) {
  data <- readRDS(model_data_full_path(model_dir, data_type))
}

scenario_rdata_directory <- function(scenario_dir) {
  dir_name <- paste0(scenario_dir, "/rdata")
  ifelse(!dir.exists(dir_name), dir.create(dir_name), FALSE)
  dir_name
}

scenario_data_file_names <- function(){
  list(
    person_history = "person_history",
    cell_cycle_status = "cell_cycle_status",
    cell_cycle = "cell_cycle",
    cycle_status = "cycle_status",
    cell_cycle_transitions = "cell_cycle_transitions",
    cell_cycle_reconciliation = "cell_cycle_reconciliation",
    ph = "person_history",
    ccs = "cell_cycle_status",
    cc = "cell_cycle",
    cs = "cycle_status",
    cct = "cell_cycle_transitions",
    ccr = "cell_cycle_reconciliation"
  )
}

scenario_data_full_path <- function(scenario_dir, data_type) {
  paste0(scenario_rdata_directory(scenario_dir),"/",scenario_data_file_names()[data_type], ".rds")
}

save_scenario_data <- function(scenario_dir, data_type, data) {
  saveRDS(data, scenario_data_full_path(scenario_dir, data_type))
}

load_scenario_data <- function(scenario_dir, data_type) {
  data <- readRDS(scenario_data_full_path(scenario_dir, data_type))
}

# check if we have an upto date model level rdata directory
# if not, return false
rdata_up_to_date <- function(model_dir) {
  rdir <- model_rdata_directory(model_dir) 
  if (is_empty(list.files(rdir))) { return(FALSE) }
  min(file.mtime(dir(path = rdir, full.names = TRUE))) > model_run_date_time(model_dir)
}

# tests -------------------------------------------------------------------
test_data <- function() {
  m <- model_dir_list()[1]
  s <- scenario_dir_list(m)[[1]]
  return(list(model = m, scenario = t))
}

test_get_model_parameters <- function(m = test_data()$model) {
  read_model_parameters(m)
}

test_load_scenario_data <- function(data_file_code = "cell_cycle_transitions", 
                                    m = 1, 
                                    s = 1) {
  m = model_dir_list()[[m]]
  s = scenario_dir_list(m)[[s]]
  data.table(load_scenario_data(s, data_file_code))
}

test_read_disease_log <- function() {
  m = model_dir_list()[[1]]
  s = scenario_dir_list(m)[[1]]
  read_disease_log(s)
}

test_model_data_full_path <- function() {
  m = model_dir_list()[[1]]
  dt = "cell_cycle_transitions"
  model_data_full_path(m, dt)
}