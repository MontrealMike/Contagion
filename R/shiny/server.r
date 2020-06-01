# Contagion Model
#
# shiny server

source("../scripts/contagion_analysis.r")
source("../scripts/summarize_sim_data.R")
library(ggplot2)
library(kableExtra)
library(RColorBrewer)
library(scales)

prepare_rdata(model_root_dir())
server <- function(input, output, session) {

  
  # model selection and parms ------------------------------------------------------  
  # update scenario list drop_down based on model selection
  observeEvent (input$m_model_selection, {
    scenario_choices = scenario_list(input$m_model_selection)
    updateSelectizeInput(session, "s_scenario_selection", choices = scenario_choices)
  })
  
  
  model_parms <- reactive({
    print(paste0("Reading model parms ", input$m_model_selection))
    read_model_parameters(input$m_model_selection)
  })
  
  model_parm_tables <- reactive({
    # print(model_parameters(input$m_model_selection))
    model_parameters(input$m_model_selection)
  })
  
  
  output$m_model_desc_parms <- renderTable(model_parm_tables()$model_desc_parms, colnames = F)
  output$m_world_parms <- renderTable(model_parm_tables()$world_parms, colnames = F)
  output$m_sensitivity_parms <- renderTable(model_parm_tables()$sensitivity_parms, colnames = F)
  output$m_beta_dist_parms <- renderTable(model_parm_tables()$beta_dist_parms, colnames = T)
  output$m_pert_dist_parms <- renderTable(model_parm_tables()$pert_dist_parms, colnames = T)
  output$m_vuln_dist_parms <- renderTable(model_parm_tables()$vuln_dist_parms, colnames = T)
  
  # model comparison statistsics -----------------------------------------------------
  model_summary_results <- reactive({
    scenario_comparison <- scenario_comparison_tables(input$m_model_selection)
    msr <- list()
    msr$sct_pct <- kable(scenario_comparison$scenario_table_pct,
                         "html",
                         align= "ccrrrrr") %>%
      kable_styling(full_width = FALSE, position = "center") %>% 
      add_header_above(c(" ", " ", "Percent of population affected" = 5))
    msr$sct_abs <- kable(scenario_comparison$scenario_table_abs,
                         "html",
                         align= "ccrrrrr") %>%
      kable_styling(full_width = FALSE, position = "center") %>% 
      add_header_above(c(" ", " ", "People affected" = 5))
    msr
  })
  
  output$m_scenario_comparison_table_pct <- reactive(model_summary_results()$sct_pct)
  output$m_scenario_comparison_table_abs <- reactive(model_summary_results()$sct_abs)
  
  # model comparison cumulative plots ------------------------------------------------
  output$m_cumulative_disease_plot <- renderPlot({
    plot_data <- load_model_data(input$m_model_selection, "cell_cycle_transitions") %>% 
      group_by(scenario, disease_status, cycle) %>% 
      summarize(disease_transitions = sum(disease_transitions)) %>% 
      mutate(cum_transitions = cumsum(disease_transitions))
    plot_data$scenario <- factor(plot_data$scenario, ordered = TRUE)
    
    ggplot(
      plot_data,
      aes(x = cycle, y = cum_transitions, color = scenario)) + 
      geom_line(size = 1.2) + 
      facet_grid(disease_status ~ .)
  })
  
  # model comparison area plots ------------------------------------------------
  output$m_disease_evolution_plot <- renderPlot({
    plot_data <- load_model_data(input$m_model_selection, "cycle_status")
    ggplot(
      plot_data,
      aes(x = cycle, y = status_pop, fill = status)) +
      geom_area() + 
      facet_grid(scenario ~ .)
  })
  
  
  # scenario data -----------------------------------------------------------------
  scenario_data <- reactive({
    sd <- list()
    sd$cycle_status <-
      load_scenario_data(input$s_scenario_selection, "cycle_status")
    sd$cell_cycle <-
      load_scenario_data(input$s_scenario_selection, "cell_cycle")
    sd$cell_cycle_status <-
      load_scenario_data(input$s_scenario_selection, "cell_cycle_status")
    sd$cell_cycle_transitions <-
      load_scenario_data(input$s_scenario_selection, "cell_cycle_transitions")
    sd$cell_cycle_reconciliation <-
      load_scenario_data(input$s_scenario_selection, "cell_cycle_reconciliation")
    sd
  })
  
  # disease evolution area plot -------------------------------------
  output$s_disease_evolution_plot <- renderPlot({
    plot_data <- scenario_data()$cycle_status
    ggplot(
      plot_data,
      aes(x = cycle, y = status_pop, fill = status)
    ) +
      geom_area()
  })
  
  # cumulative disease plot ----------------------------------------------
  output$s_cumulative_disease_plot <- renderPlot({
    plot_data <- scenario_data()$cell_cycle_transitions %>% 
      group_by(disease_status, cycle) %>% 
      summarize(disease_transitions = sum(disease_transitions)) %>% 
      mutate(cum_transitions = cumsum(disease_transitions))
    ggplot(
      plot_data,
      aes(x = cycle, y = cum_transitions, color = disease_status)) + 
      geom_line(size = 1.5)
  })
  
  # output$s_disease_evolution_table <- renderTable(scenario_data()$cycle_status)
  
  # population heatmap -------------------------------------------------------------
  observe ({
    cycles <- model_parms()$cycles
    updateSliderInput(session, "s_heatmap_cycle", max = model_parms()$cycles)
  })
  observe ({
    updateSliderInput(session, "s_status_row", max = model_parms()$world_parms$rows)
  })
  observe ({
    updateSliderInput(session, "s_status_col", max = model_parms()$world_parms$cols)
  })
  
  heatmap_data <- reactive({
    plot_data <- if (input$s_heatmap_variable == "All") {
      scenario_data()$cell_cycle %>% 
        rename(pop_pct = cell_pop_pct,
               pop = cell_pop)
      
    } else {
      scenario_data()$cell_cycle_status %>%
        filter(status == !!input$s_heatmap_variable) %>% 
        rename(pop_pct = cell_status_pop_pct,
               pop = cell_status_pop)
    }
    plot_data
  })
  
  heatmap_cycle_data <- reactive ({
    heatmap_data() %>% 
            filter(cycle == !!input$s_heatmap_cycle)
  })
  
  output$s_population_heat_map <- renderPlot({
    max_pop_pct <- max(heatmap_data()$pop_pct)
    hm_parms <- switch(input$s_heatmap_variable,
                 "All" = list(midpoint = max_pop_pct / 2, 
                              limits = c(0, max_pop_pct * 1.1)),
                 "Well" = list(midpoint = 0.50,
                               limits = c(0,1)),
                 list(midpoint = 0.10,
                      limits = c(0,1)))
    
    
    ggplot(heatmap_cycle_data(), aes(x = col, y = row)) +
      geom_raster(aes(fill = pop_pct), 
                  interpolate = ) +
      scale_fill_gradient2(low = "yellow",
                           mid = "orange",
                           high = "red",
                           labels = percent,
                           midpoint = hm_parms$midpoint,
                           limits = hm_parms$limits,
                           guide_legend(title = "population %")
      ) + 
      scale_x_discrete(breaks = NULL) + 
      scale_y_discrete(breaks = NULL)
  })
  
  output$s_heatmap_data_table <- reactive({
    table_data <- heatmap_cycle_data() %>% 
      pivot_wider(id_cols = row, names_from = col, values_from = pop)
    
    kable(table_data) %>% 
      kable_styling()
  })

  # population status changes -----------------------------------------------------------
  output$s_cycle_pop_changes <- reactive(({
    # print(paste0("prt  has ", nrow(scenario_data()$population_reconciliation), "rows. Cycle ", input$s_heatmap_cycle))
    table_data <- population_reconciliation_table(scenario_data()$cell_cycle_reconciliation,
                                                  input$s_heatmap_cycle)
    kable(table_data, align = "rrrrrr") %>% 
      kable_styling() %>% 
      row_spec(c(1,4,10), bold = T, hline_after = T) %>% 
      row_spec(c(3,9), hline_after = T) %>% 
      column_spec(7, bold = T)
    
  }))
  
  output$s_cell_cycle_pop_changes <- reactive(({
    cell = input$s_status_row * model_parms()$world_parms$rows + input$s_status_col
    table_data <- population_reconciliation_table(scenario_data()$cell_cycle_reconciliation,
                                                  input$s_heatmap_cycle, cell)
    kable(table_data, align = "rrrrrr") %>% 
      kable_styling() %>% 
      row_spec(c(1,4,10), bold = T, hline_after = T) %>% 
      row_spec(c(3,9), hline_after = T) %>% 
      column_spec(7, bold = T)
  }))
  
  
  
}