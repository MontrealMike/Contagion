rm(list = ls())
require(shinycssloaders)
library(shiny)
library(DT, quietly = TRUE)
source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

hierarchyList <- HierarchySelectionList()
progCanList <- GeoSelectionList()
subprogList <- GeoSelectionList(includeSubs = TRUE,
                                includeProgs = FALSE,
                                includeCan = FALSE)
progList <- GeoSelectionList(includeSubs = FALSE,
                             includeProgs = TRUE,
                             includeCan = FALSE)
timeList <- TimeSelectionList()
weightingList <- WeightingSelectionList()
levelList <- LevelSelectionList()
statisticList <- StatisticSelectionList()
variableList <- VariableSelectionList()
caVariableList <- ContainerVariableSelectionList()
qtrList <- QuarterSelectionList()
halfList <- HalfSelectionList()
processorList <- ProcessorSelectionList()
brandList <- BrandSelectionList()
strataList <- StrataList()

shinyUI(
  # tagList allows placing tag head without ghosting navbar: https://groups.google.com/forum/#!topic/shiny-discuss/6C9SbSLbKmc Dean Attali
  tagList(
    tags$head(
      # darken horizontal rules 
      tags$style(HTML("hr {border-top: .5px solid #000000;}")),
      
      # shrink parameter setting controls a little
      tags$style(HTML("
          .selectize-input, .selectize-dropdown,
          .select-input, .radio-button, .action-button,
          .download-button, .slider-input, numeric-input
          {font-size: 85%;}")),
      
      # download buttons need their own class for reasons that fail me
      tags$style(".downbutton{ font-size: 85%; }")
    ),
    
    navbarPage(
      title = "EPRA Sampling Analysis",       # this what appears in the browser tab
      # Program reports ---------------------
      navbarMenu("Program reports",
                 # Detail Report ---------------------------------------
                 tabPanel("Detail Report", 
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Detail Report Parameters"), hr(),  style="text-align: center"),
                              selectInput("drHierarchySelection", "Product hierarchy", hierarchyList),
                              selectInput("drGeoSelection", "Program", progCanList),
                              selectInput(
                                "drTimeSelection",
                                "Period",
                                timeList,
                                selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              selectInput("drWeightingSelection", "Weighting", weightingList),
                              selectInput("drLevelSelection", "Levels", levelList),
                              hr(),
                              actionButton("drRunReport", "Run report"),
                              width = 2
                            ),
                            
                            # Show reports in a tabbed panel;
                            mainPanel(
                              tabPanel("Detail Report", DT::dataTableOutput("detailReport") %>% withSpinner())
                            )
                          )
                 ),
                 
                 # Attest Report ---------------------------------------
                 tabPanel("Attest Report",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Attest Report Parameters"), hr(),  style="text-align: center"),
                              selectInput("arHierarchySelection", "Product hierarchy", hierarchyList),
                              selectInput("arGeoSelection", "Program", progCanList),
                              selectInput(
                                "arTimeSelection",
                                "Period",
                                timeList,
                                selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              selectInput("arWeightingSelection", "Weighting", weightingList),
                              selectInput("arLevelSelection", "Levels", levelList),
                              hr(),
                              actionButton("arRunReport", "Run report"),
                              width = 2
                            ),
                            
                            # Show reports in a tabbed panel;
                            mainPanel(
                              tabPanel("Attest Report", DT::dataTableOutput("attestReport") %>% withSpinner())
                            )
                          )
                 ),
                 
                 # Summary Program Comparison Report ---------------------------------------
                 tabPanel("Summary Comparison Report",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Summary Comparison Parameters"), hr(),  style="text-align: center"),
                              selectInput("pcrHierarchySelection", "Product hierarchy", hierarchyList),
                              selectInput(
                                "pcrTimeSelection",
                                "Period",
                                timeList,
                                selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              selectInput("pcrWeightingSelection", "Weighting", weightingList),
                              selectInput("pcrLevelSelection", "Levels", levelList),
                              selectInput("pcrStatisticSelection", "Statistic", statisticList, selected = 1),
                              selectInput("pcrVariableSelection", "Variable", variableList, selected = "mean"),
                              # checkboxInput("pcrIncludeOES", "Include OES", value = FALSE),  # 2018-10-12 (MRB) OES removed from system  
                              hr(),
                              actionButton("pcrRunReport", "Run report"),
                              width = 2
                            ),
                            
                            # Show reports in a tabbed panel;
                            mainPanel(
                              tabPanel("Program Comparison Report", DT::dataTableOutput("programComparisonReport") %>% withSpinner())
                            )
                          )
                 ),
                 
                 # Detail Program Comparison report and chart --------------------------------------------------------
                 tabPanel("Detail Comparison Report",
                          wellPanel(
                            fluidRow(
                              div(h3("Detail Comparison Parameters"), hr(),  style="text-align: center"),
                              column(
                                selectInput("prctHierarchySelection", "Product hierarchy", hierarchyList),
                                selectizeInput("prctNodeSelection", "Product", choices = NULL),
                                width = 2
                              ),
                              column(
                                selectInput("prctProgramIDs", "Programs", choices = progList, multiple = TRUE,
                                            selectize = TRUE, selected = progList),
                                checkboxInput("prctIncludeSummary", "Summary results", value = TRUE),
                                width = 2
                              ),
                              column(
                                selectInput("prctStatisticSelection", "Statistic", statisticList, selected = 1),
                                selectInput("prctWeightingSelection", "Weighting", weightingList),
                                selectInput("prctVariableSelection", "Variable", variableList, selected = "mean"),
                                width = 2
                              ),
                              column(
                                radioButtons(
                                  "prctTimeUnitSelection",
                                  label = "Time unit",
                                  choices = list("Quarter" = 0, "Half" = 1, "Year" = 2),
                                  selected = 2,
                                  inline = FALSE
                                ),
                                conditionalPanel(
                                  "input.prctTimeUnitSelection == 0",
                                  selectInput(
                                    "prctQtrStart",
                                    label = "Start quarter",
                                    choices = qtrList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "prctQtrCount",
                                    label = "Number of quarters",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.prctTimeUnitSelection == 1",
                                  selectInput(
                                    "prctHalfStart",
                                    label = "Start half",
                                    choices = halfList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "prctHalfCount",
                                    label = "Number of halves",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.prctTimeUnitSelection == 2",
                                  sliderInput(
                                    "prctYrRange",
                                    label = "Year range",
                                    min = 2013,
                                    max = year(Sys.Date()),
                                    value = c(2014, year(Sys.Date())),
                                    round = TRUE,
                                    step = 1,
                                    sep = "",
                                    ticks = FALSE
                                  )
                                ),
                                width = 2
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(actionButton("prctRunReport", "Run report"), width = 1),
                              column(downloadButton("prctSaveReport", "Save table", class = "downbutton"), width = 1)
                            )
                          ),
                          fluidRow(
                            uiOutput("prctCaption") %>% withSpinner(),
                            tags$div(id = "prctRows") # placeholder for the insertUI function in the server
                            
                          )
                 )
      ),
      
      navbarMenu("Processor reports",
                 
                 # Processor Product report ---------------------------------------------------------------------------------------
                 tabPanel("Product Report",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Product Report Parameters"), hr(), style="text-align: center"),
                              selectInput("pdProcessorSelection", "Processor", processorList),
                              selectInput("pdHierarchySelection", "Product hierarchy", hierarchyList),
                              selectInput("pdGeoSelection", "Program", progCanList),
                              selectInput(
                                "pdTimeSelection",
                                "Period",
                                timeList,
                                selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              selectInput("pdLevelSelection", "Levels", levelList),
                              hr(),
                              actionButton("pdRunReport", "Run report"),
                              width = 2
                            ),
                            
                            # Show reports in a tabbed panel;
                            mainPanel(
                              DT::dataTableOutput("pdReport") %>% withSpinner()
                            )
                          )
                 ),
                 
                 # Summary Processor Comparison report ---------------------------------------------------------------------------------------
                 tabPanel("Summary Comparison Report",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Summary Comparison Parameters"), hr(), style="text-align: center"),
                              selectInput("pcHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("pcNodeSelection", "Product", choices = NULL),
                              selectInput("pcGeoSelection", "Program", progCanList),
                              selectInput(
                                "pcTimeSelection",
                                "Period",
                                timeList,
                                selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              
                              checkboxInput("pcActiveOnly", "Active processors only", value = TRUE),
                              hr(),
                              actionButton("pcRunReport", "Run report"),
                              width = 2
                            ),
                            
                            # Show reports in a tabbed panel;
                            mainPanel(
                              DT::dataTableOutput("pcReport") %>% withSpinner()
                            )
                          )
                 ),
                 
                 # Detail Processor Comparison report and chart --------------------------------------------------------
                 tabPanel("Detail Comparison Report",
                          wellPanel(
                            fluidRow(
                              div(h3("Detail Comparison Parameters"), hr(),  style="text-align: center"),
                              column(
                                selectInput("pccHierarchySelection", "Product hierarchy", hierarchyList),
                                selectizeInput("pccNodeSelection", "Product", choices = NULL),
                                width = 2
                              ),
                              column(
                                radioButtons("pccProcessorSelectionType", "Select processors",
                                             c("All active" = "allActive",
                                               "Specific" = "specific"),
                                             inline = FALSE,
                                             selected = "allActive"),
                                conditionalPanel("input.pccProcessorSelectionType == 'specific'",
                                                 selectInput("pccProcessorList", "Processors", choices = processorList,
                                                             multiple = TRUE, selectize = TRUE)
                                ),
                                width = 2
                              ),
                              column(
                                selectInput("pccProgramIDs", "Programs", choices = progList, multiple = TRUE,
                                            selectize = TRUE, selected = progList),
                                width = 2
                              ),
                              column(
                                selectInput("pccStatisticSelection", "Statistic", statisticList, selected = 1),
                                selectInput("pccVariableSelection", "Variable", variableList, selected = "mean"),
                                width = 2
                              ),
                              column(
                                radioButtons(
                                  "pccTimeUnitSelection",
                                  label = "Time unit",
                                  choices = list("Quarter" = 0, "Half" = 1, "Year" = 2),
                                  selected = 2,
                                  inline = FALSE
                                ),
                                conditionalPanel(
                                  "input.pccTimeUnitSelection == 0",
                                  selectInput(
                                    "pccQtrStart",
                                    label = "Start quarter",
                                    choices = qtrList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "pccQtrCount",
                                    label = "Number of quarters",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.pccTimeUnitSelection == 1",
                                  selectInput(
                                    "pccHalfStart",
                                    label = "Start half",
                                    choices = halfList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "pccHalfCount",
                                    label = "Number of halves",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.pccTimeUnitSelection == 2",
                                  sliderInput(
                                    "pccYrRange",
                                    label = "Year range",
                                    min = 2013,
                                    max = year(Sys.Date()),
                                    value = c(2014, year(Sys.Date())),
                                    round = TRUE,
                                    step = 1,
                                    sep = "",
                                    ticks = FALSE
                                  )
                                ),
                                width = 2
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(actionButton("pccRunReport", "Run report"), width = 1),
                              column(downloadButton("pccSaveReport", "Save table", class = "downbutton"), width = 1)
                            )
                          ),
                          fluidRow(
                            uiOutput("pccCaption") %>% withSpinner(),
                            fluidPage(
                              tags$div(id = "pccRows") # placeholder for the insertUI function in the server
                            )
                          )
                 )
      ),
      
      navbarMenu("Analysis reports",
                 # Container analysis --------------------------------------------------------------------
                 tabPanel("Container analysis",
                          wellPanel(
                            fluidRow(
                              div(h3("Container Analysis Parameters"), hr(),  style="text-align: center"),
                              column(
                                radioButtons("caOrganizationType", "Organize by",
                                             c("Program/Container" = "program",
                                               "Container/Program" = "containerType"),
                                             inline = FALSE,
                                             selected = "program"),
                                width = 2
                              ),
                              column(
                                selectInput("caProgramIDs", "Programs", choices = progList, multiple = TRUE,
                                            selectize = TRUE, selected = progList),
                                checkboxInput("caIncludeSummary", "Summary results", value = TRUE),
                                width = 2
                              ),
                              column(
                                selectInput("caVariableSelection", "Variable", caVariableList, selected = "meanGr"),
                                width = 2
                              ),
                              column(
                                radioButtons(
                                  "caTimeUnitSelection",
                                  label = "Time unit",
                                  choices = list("Quarter" = 0, "Half" = 1, "Year" = 2),
                                  selected = 2,
                                  inline = FALSE
                                ),
                                conditionalPanel(
                                  "input.caTimeUnitSelection == 0",
                                  selectInput(
                                    "caQtrStart",
                                    label = "Start quarter",
                                    choices = qtrList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "caQtrCount",
                                    label = "Number of quarters",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.caTimeUnitSelection == 1",
                                  selectInput(
                                    "caHalfStart",
                                    label = "Start half",
                                    choices = halfList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "caHalfCount",
                                    label = "Number of halves",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.caTimeUnitSelection == 2",
                                  sliderInput(
                                    "caYrRange",
                                    label = "Year range",
                                    min = 2013,
                                    max = year(Sys.Date()),
                                    value = c(2014, year(Sys.Date())),
                                    round = TRUE,
                                    step = 1,
                                    sep = "",
                                    ticks = FALSE
                                  )
                                ),
                                width = 2
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(actionButton("caRunReport", "Run report"), width = 1),
                              column(downloadButton("caSaveReport", "Save table", class = "downbutton"), width = 1)
                            )
                          ),
                          fluidRow(
                            uiOutput("caCaption") %>% withSpinner(),
                            fluidPage(
                              tags$div(id = "caRows") # placeholder for the insertUI function in the server
                            )
                          )
                 ),
                 
                 # Brand analysis ------------------------------------------------------------------------
                 tabPanel("Brand analysis",
                          wellPanel(
                            fluidRow(
                              div(h3("Brand Analysis Parameters"), hr(),  style="text-align: center"),
                              column(
                                selectInput("barHierarchySelection", "Product hierarchy", hierarchyList),
                                selectizeInput("barNodeSelection", "Product", choices = NULL),
                                width = 2
                              ),
                              column(
                                radioButtons("barBrandSelectionType", 
                                             label = "Selection type",
                                             choices = list("Top brands (by weight)" = 0, "Specific brands" = 1),
                                             selected = 0),
                                conditionalPanel(
                                  "input.barBrandSelectionType == 0",
                                  numericInput("barMaxCount", 
                                               label = "Number of brands", 
                                               value = 10,
                                               min = 1, 
                                               max = 9999)
                                ),
                                conditionalPanel(
                                  "input.barBrandSelectionType == 1",
                                  selectInput("barBrandList",
                                              label = "Brands", 
                                              choices = brandList,
                                              multiple = TRUE,
                                              selectize = TRUE)
                                ),
                                radioButtons("barSummarizationType",
                                             label = "Summary rows",
                                             choices = list("None" = 0,
                                                            "All other brands" = 1,
                                                            "All other and summary" = 2),
                                             selected = 2
                                ),
                                width = 2
                              ),
                              column(
                                selectInput("barProgramIDs", "Programs", choices = progList, multiple = TRUE,
                                            selectize = TRUE, selected = progList),
                                checkboxInput("barIncludeSummary", "Summary results", value = TRUE),
                                width = 2
                              ),
                              column(
                                selectInput("barStatisticSelection", "Statistic", statisticList, selected = 1),
                                selectInput("barVariableSelection", "Variable", variableList, selected = "mean"),
                                width = 2
                              ),
                              column(
                                radioButtons(
                                  "barTimeUnitSelection",
                                  label = "Time unit",
                                  choices = list("Quarter" = 0, "Half" = 1, "Year" = 2),
                                  selected = 2,
                                  inline = FALSE
                                ),
                                conditionalPanel(
                                  "input.barTimeUnitSelection == 0",
                                  selectInput(
                                    "barQtrStart",
                                    label = "Start quarter",
                                    choices = qtrList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "barQtrCount",
                                    label = "Number of quarters",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.barTimeUnitSelection == 1",
                                  selectInput(
                                    "barHalfStart",
                                    label = "Start half",
                                    choices = halfList,
                                    selected = 1
                                  ),
                                  numericInput(
                                    "barHalfCount",
                                    label = "Number of halves",
                                    value = 8,
                                    min = 1,
                                    max = 48
                                  )
                                ),
                                conditionalPanel(
                                  "input.barTimeUnitSelection == 2",
                                  sliderInput(
                                    "barYrRange",
                                    label = "Year range",
                                    min = 2013,
                                    max = year(Sys.Date()),
                                    value = c(2014, year(Sys.Date())),
                                    round = TRUE,
                                    step = 1,
                                    sep = "",
                                    ticks = FALSE
                                  )
                                ),
                                width = 2
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(actionButton("barRunReport", "Run report"), width = 1),
                              column(downloadButton("barSaveReport", "Save table", class = "downbutton"), width = 1)
                            )
                          ),
                          fluidRow(
                            uiOutput("barCaption") %>% withSpinner(),
                            tags$div(id = "barRows") # placeholder for the insertUI function in the server
                            
                          )
                 )
      ),
      
      navbarMenu("Deprecated reports",
                 # Time Series Charts  ---------------------------------------------------------------------------------------
                 tabPanel("Time series",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Time Series Parameters"), hr(),  style="text-align: center"),
                              selectInput("tsHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("tsNodeSelection", "Product", choices = NULL),
                              selectInput("tsStatisticSelection", "Statistic", statisticList, selected = 1),
                              selectInput("tsVariableSelection", "Variable", variableList, selected = "mean"),
                              selectInput("tsWeightingSelection", "Weighting", weightingList),
                              radioButtons(
                                "tsTimeUnitSelection",
                                label = "Time unit",
                                choices = list("Quarter" = 0, "Year" = 1),
                                selected = 1,
                                inline = TRUE
                              ),
                              conditionalPanel(
                                "input.tsTimeUnitSelection == 0",
                                selectInput(
                                  "tsQtrStart",
                                  label = "Start quarter",
                                  choices = qtrList,
                                  selected = 1
                                ),
                                numericInput(
                                  "tsQtrCount",
                                  label = "Number of quarters",
                                  value = 8,
                                  min = 1,
                                  max = 48
                                )
                              ),
                              conditionalPanel(
                                "input.tsTimeUnitSelection == 1",
                                sliderInput(
                                  "tsYrRange",
                                  label = "Year range",
                                  min = 2013,
                                  max = year(Sys.Date()),
                                  value = c(2014, year(Sys.Date())),
                                  round = TRUE,
                                  step = 1,
                                  sep = "",
                                  ticks = FALSE
                                )
                              ),
                              hr(),
                              actionButton("tsRunReport", "Run report"),
                              width = 2
                            ),
                            mainPanel(
                              plotOutput("tsPlot", width = "60%", height = "800px") %>% withSpinner()
                            )
                          )
                 ),
                 
                 # Area Charts  ---------------------------------------------------------------------------------------
                 tabPanel("Area",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Area Chart Parameters"), hr(),  style="text-align: center"),
                              selectInput("acHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("acNodeSelection", "Product", choices = NULL),
                              selectInput("acWeightingSelection", "Weighting", weightingList),
                              radioButtons(
                                "acTimeUnitSelection",
                                label = "Time unit",
                                choices = list("Quarter" = 0, "Year" = 1),
                                selected = 1,
                                inline = TRUE
                              ),
                              conditionalPanel(
                                "input.acTimeUnitSelection == 0",
                                selectInput(
                                  "acQtrStart",
                                  label = "Start quarter",
                                  choices = qtrList,
                                  selected = 1
                                ),
                                numericInput(
                                  "acQtrCount",
                                  label = "Number of quarters",
                                  value = 8,
                                  min = 1,
                                  max = 48
                                )
                              ),
                              conditionalPanel(
                                "input.acTimeUnitSelection == 1",
                                sliderInput(
                                  "acYrRange",
                                  label = "Year range",
                                  min = 2013,
                                  max = year(Sys.Date()),
                                  value = c(2014, year(Sys.Date())),
                                  round = TRUE,
                                  step = 1,
                                  sep = "",
                                  ticks = FALSE
                                )
                              ),
                              hr(),
                              actionButton("acRunReport", "Run report"),
                              width = 2
                            ),
                            mainPanel(
                              plotOutput("acPlot", width = "100%", height = "800px") %>% withSpinner()
                            )
                          )
                 ),
                 
                 # Weight Ratio report and chart --------------------------------------------------------
                 tabPanel("Weight ratios by program",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Weight Ratio Parameters"), hr(),  style="text-align: center"),
                              selectInput("wrpHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("wrpNodeSelection", "Product", choices = NULL),
                              selectInput("wrpProgramIDs", "Programs", choices = progList, multiple = TRUE,
                                          selected = progList),
                              checkboxInput("wrpIncludeSummary", "Summary results", value = TRUE),
                              selectInput("wrpWeightingSelection", "Weighting", weightingList),
                              radioButtons(
                                "wrpTimeUnitSelection",
                                label = "Time unit",
                                choices = list("Quarter" = 0, "Year" = 1),
                                selected = 1,
                                inline = TRUE
                              ),
                              conditionalPanel(
                                "input.wrpTimeUnitSelection == 0",
                                selectInput(
                                  "wrpQtrStart",
                                  label = "Start quarter",
                                  choices = qtrList,
                                  selected = 1
                                ),
                                numericInput(
                                  "wrpQtrCount",
                                  label = "Number of quarters",
                                  value = 8,
                                  min = 1,
                                  max = 48
                                )
                              ),
                              conditionalPanel(
                                "input.wrpTimeUnitSelection == 1",
                                sliderInput(
                                  "wrpYrRange",
                                  label = "Year range",
                                  min = 2013,
                                  max = year(Sys.Date()),
                                  value = c(2014, year(Sys.Date())),
                                  round = TRUE,
                                  step = 1,
                                  sep = "",
                                  ticks = FALSE
                                )
                              ),
                              hr(),
                              actionButton("wrpRunReport", "Run report"),
                              downloadButton("wrpSaveReport", "Save table", class = "downbutton"),
                              width = 1
                            ),
                            mainPanel(
                              h2("Weight Percent"),
                              fluidPage(
                                column(6, tableOutput("wrpTable")%>% withSpinner()),
                                column(5, 
                                       tags$div(
                                         style = "margin-top:50px;",
                                         plotOutput("wrpCharts"))
                                )
                              )
                            )
                          )
                 )
      ),
      
      navbarMenu("Management Reports",
                 # Management Reports ------------------------------------------------------------------------------
                 tabPanel("Word Reports",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Management Report Parameters"), hr(), style="text-align: center"),
                              selectInput(
                                "pdfTimeSelection",
                                label = "Period",
                                choices = timeList,
                                selected = paste("Y-", format(Sys.Date(), "%Y"), sep = "")
                              ),
                              selectInput("pdfWeightingSelection", 
                                          label = "Weighting",
                                          choices = weightingList),
                              hr(),
                              downloadButton("pdfGenerateReport", "Generate Report"),
                              width = 2
                            ),
                            mainPanel(
                              h3("Word Report Generation"),
                              h4("How to create a management report"),
                              p("1. Choose the applicable time period."),
                              p("2. Choose the weighting method."),
                              p("3. Press the Generate Report button."),
                              p("The file will be saved in your download directory under the name ESA Management Report.html"),
                              br(),
                              h4("To edit and save the report as a Microsoft Word document"),
                              p("1. Start Microsoft Word."),
                              p("2. Use the File menu to open the downloaded file (yes you can open html files in Word)."),
                              p("3. Edit as you see fit."),
                              p("4. Save as a Word document.")
                            )
                          )
                 ),
                 tabPanel("Excel Reports",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Excel Report Parameters"), hr(), style="text-align: center"),
                              selectInput(
                                "xlQtrStart",
                                label = "Start quarter",
                                choices = qtrList,
                                selected = 1
                              ),
                              numericInput(
                                "xlQtrCount",
                                label = "Number of quarters",
                                value = 8,
                                min = 1,
                                max = 48
                              ),
                              selectInput("xlWeightingSelection", "Weighting", weightingList),
                              hr(),
                              downloadButton("xlGenerateReport", "Generate Report"),
                              width = 2
                            ),
                            mainPanel(
                              h3("Excel Report Generation")
                            )
                          )
                 )
      ),
      
      navbarMenu("Math check",  
                 # Single Stratum Math Check ------------------------------------------------------------ 
                 tabPanel("Single Stratum",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Single Stratum Math Check Parameters"), hr(), style="text-align: center"),
                              selectInput("ssHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("ssNodeSelection", "Product", choices = NULL),
                              selectInput("ssStratumSelection", "Stratum", strataList),
                              selectInput("ssStatisticSelection", "Statistic", statisticList, selected = 1),
                              hr(),
                              actionButton("ssRunReport", "Run report"),
                              width = 2
                            ),
                            mainPanel(
                              h3(strong(textOutput("ssContentDescription")), align = "center"),
                              DT::dataTableOutput("ssSummaryReport"),
                              hr(),
                              DT::dataTableOutput("ssDetailReport") %>% withSpinner()
                            )
                          )
                 ),
                 
                 # Multi Stratum Math Check ------------------------------------------------------------ 
                 tabPanel("Multi Stratum",
                          sidebarLayout(
                            sidebarPanel(
                              div(h3("Multi Stratum Math Check Parameters"), hr(), style="text-align: center"),
                              selectInput("msHierarchySelection", "Product hierarchy", hierarchyList),
                              selectizeInput("msNodeSelection", "Product", choices = NULL),
                              selectInput("msStatisticSelection", "Statistic", statisticList, selected = 1),
                              selectInput("msWeightingSelection", "Weighting", weightingList, selected = 1),
                              radioButtons(
                                "msStratumSelectionType",
                                label = "Stratum selection type",
                                choices = list("Standard" = 0, "Custom" = 1),
                                selected = 0,
                                inline = TRUE
                              ),
                              conditionalPanel(
                                "input.msStratumSelectionType == 0",
                                selectInput("msGeoSelection", "Program", progCanList),
                                selectInput(
                                  "msTimeSelection",
                                  "Period",
                                  timeList,
                                  selected = paste("Y", format(Sys.Date(), "%Y"), sep = "")
                                )
                              ),
                              conditionalPanel(
                                "input.msStratumSelectionType == 1",
                                selectInput("msStratumSelection", "Strata", multiple = TRUE, strataList)
                              ),
                              hr(),
                              actionButton("msRunReport", "Run report"),
                              hr(),
                              div(imageOutput("msVarianceFormula"), style="height: 60px;"),
                              width = 2
                            ),
                            
                            mainPanel(
                              h3(strong(textOutput("msContentDescription")), align = "center"),
                              fluidRow(DT::dataTableOutput("msSummaryReport")),
                              hr(),
                              fluidRow(DT::dataTableOutput("msDetailReport"))
                            )
                          )
                 )
      ), 
      
      navbarMenu("Utilities",
                 # Refresh ESA data  ----------------------------------------------------------------------------------
                 tabPanel("Refresh Data",
                          div(h3("Refresh ESA data from SQL database")),
                          sidebarPanel(
                            fluidRow(
                              column(3, ""),
                              column(3, h4("Effective date")),
                              column(2, h4("Refresh duration"))
                            ),
                            hr(),
                            fluidRow(
                              column(3, h4("Previous refresh")),
                              column(3, textOutput("utilRefreshCurrentEffectiveDate")),
                              column(2, textOutput("utilRefreshLastDuration"))
                            ),
                            fluidRow(),
                            fluidRow(
                              column(3, h4("This refresh")),
                              column(3, textOutput("utilRefreshEffectiveDate")),
                              column(2, textOutput("utilRefreshDuration"))
                            ),
                            hr(),
                            actionButton("utilRefreshData", "Refresh ESA data"),
                            width = 8
                          )
                 )
      ) 
    )
  )
)