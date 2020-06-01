# Contagion model-#
#  user interface
#
## ui elements:

# content selection
#   select model run
#   select scenario within the run
#
# elements to present - Scenario specific tab
#   model run parameters
#   scenario parameters
#   cell population heatmap (selectable - population category, cycle)
#   cell virulence heatmap (selectable - cycle)
#   disease status area charts (selectable - cell (or all cells))
#   scenario statistics (cases, infections, reinfections, hospitalizations, reccoveries, deaths, etc)
#
# elements to present - Scenario comparison tab

# includes and sources -----------------------------------------------
source("../scripts/contagion_analysis.r")

navbarPage(
  title = "Contagion Model Report",
  
  # Model report ----------------------------------------------------------------------------
  tabPanel(
    "Model overview",
    # Model selection ----------------------------------------------------------------------------
    fluidRow(
      selectInput("m_model_selection",
                  "Model",
                  # selected = NULL,
                  choices = model_dir_list())
    ),
    hr(),
    sidebarLayout(
      # Model parameters ----------------------------------------------------------------------------
      sidebarPanel(
        width = 3,
        style = "border: 0.5px solid grey;",
        h4("Model Parameters"),
        h5(strong("Model description")),
        tableOutput("m_model_desc_parms"),
        h5(strong("World dimensions")),
        tableOutput("m_world_parms"),
        h5(strong("Sensitivity parameters")),
        tableOutput("m_sensitivity_parms"),
        h5(strong("Beta distribution parameters")),
        tableOutput("m_beta_dist_parms"),
        h5(strong("PERT distribution parameters")),
        tableOutput("m_pert_dist_parms"),
        h5(strong("Vulnerability weightings")),
        tableOutput("m_vuln_dist_parms")
        
      ),
      mainPanel(
        # People affected ----------------------------------------------------------------------------
        fluidRow(style = "border: 0.5px solid grey;",
                 column(
                   6,
                   h3("Percent of population affected"),
                   htmlOutput("m_scenario_comparison_table_pct")
                 ),
                 column(
                   6,
                   h3("Number of people affected"),
                   htmlOutput("m_scenario_comparison_table_abs")
                 )),
        hr(),
        fluidRow(column(
          6,
          # Cumulative cases  ----------------------------------------------------------------------------
          h3("Cumulative cases by scenario"),
          plotOutput("m_cumulative_disease_plot")
        ),
        column(
          6,
          # disease evolution  ----------------------------------------------------------------------------
          h3("Disease evolution by scenario"),
          plotOutput("m_disease_evolution_plot")
        ))
      )
    )
  ),
  
  # Scenario report ------------------------------------------------------------
  tabPanel(
    "Scenario Detail",
    fluidRow( # fr1
      # Scenario selection  ----------------------------------------------------------------------------
      selectizeInput(
        "s_scenario_selection",
        "Scenario",
        selected = NULL,
        choices = NULL
      )
    ),
    hr(),
    fluidRow( # fr2
      column( # c21
        5,
        fluidRow( #fr211
          # Disease evolution  ----------------------------------------------------------------------------
          h3("Disease evolution"),
          plotOutput("s_disease_evolution_plot")
        ),
        fluidRow( #fr212
          # Cumulative cases  ----------------------------------------------------------------------------
          h3("Cumulative disease plot"),
          plotOutput("s_cumulative_disease_plot")
        )
        
      ),
      column( #c22
        7,
        fluidRow( # c221
          column( #c2211
            4,
            sliderInput(
              "s_heatmap_cycle",
              label = "Cycle",
              value = 0,
              min = 0,
              max = 100,
              round = TRUE
            )
          ),
          column( #c2212
            4,
            sliderInput(
              "s_status_row",
              label = "Row",
              value = 0,
              min = 0,
              max = 100,
              round = TRUE
            )
          ),
          column( #c2213
            4,
            sliderInput(
              "s_status_col",
              label = "Column",
              value = 0,
              min = 0,
              max = 100,
              round = TRUE
            )
          )
        ),
        # Heatmap  ----------------------------------------------------------------------------
        fluidRow( #fr 222
          h3("Population heatmap"),
          plotOutput("s_population_heat_map"),
          radioButtons(
            "s_heatmap_variable",
            label = "Heatmap variable",
            choices = c(
              "All",
              "Well",
              "Infected",
              "Symptomatic",
              "Hospitalized",
              "Dead"
            ),
            selected = "All",
            inline = TRUE
          )
        ),
        fluidRow( # fr223
          # Cycle pop transitions ---------------------------------------------------------------------
          column( # c2231
            6,
            h3("Population/status changes for cycle"),
            htmlOutput("s_cycle_pop_changes")
            
          ),
          # Cell Cycle pop transitions ----------------------------------------------------------------
          column( #c2231
            6,
            h3("Population/status changes for cell and cycle"),
            htmlOutput("s_cell_cycle_pop_changes"),
          )
        )
      )
    ),

    fluidRow( #fr 3
      htmlOutput("s_heatmap_data_table")
    )
  )
)
