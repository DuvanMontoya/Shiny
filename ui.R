# ui.R
library(shiny)
library(shinythemes)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap",
      rel = "stylesheet"
    )
  ),
  
  theme = shinytheme("flatly"),
  
  div(class = "app-header",
      titlePanel(
        div(class = "title-container",
            h1("EDA", class = "main-title"),
            p("Exploratory Data Analysis Tool", class = "subtitle")
        )
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "custom-sidebar",
      div(class = "sidebar-section",
          h4("Data Management", class = "section-title"),
          div(class = "upload-container",
              fileInput("file", "Upload Analytical", accept = c(".csv", ".RData"))
          ),
          
          div(class = "variable-selection",
              h5("Variable Configuration", class = "subsection-title"),
              div(class = "custom-select-container",
                  selectInput("kpi", "Select KPI", choices = NULL)
              ),
              div(class = "custom-select-container",
                  selectInput("media_vars", "Select Media Variables", choices = NULL, multiple = TRUE)
              ),
              div(class = "custom-select-container",
                  selectInput("spend_vars", "Select Spend Variables", choices = NULL, multiple = TRUE)
              ),
              div(class = "custom-select-container",
                  selectInput("base_vars", "Select Base Variables", choices = NULL, multiple = TRUE)
              )
          ),
          
          hr(class = "custom-hr"),
          
          div(class = "download-section",
              downloadButton("download_analytical", "Download Analytical", class = "custom-download-btn")
          )
      )
    ),
    
    mainPanel(
      class = "custom-main-panel",
      tabsetPanel(
        id = "main-tabs",
        
        # TAB: Information
        tabPanel("Information",
                 div(class = "info-container",
                     fluidRow(class = "data-manager-row",
                              column(12, div(class = "data-manager-box",
                                             h4("Data Manager", class = "box-title"),
                                             fluidRow(
                                               column(12,
                                                      dateRangeInput("date_filter",
                                                                     label = "Select Date Range:",
                                                                     start = Sys.Date() - 30,
                                                                     end = Sys.Date())
                                               )
                                             )
                              ))
                     ),
                     fluidRow(class = "info-row",
                              column(6, div(class = "info-box",
                                            h4("Activity Analysis", class = "box-title"),
                                            tableOutput("activity_table"))),
                              column(6, div(class = "info-box",
                                            h4("Spend Distribution", class = "box-title"),
                                            tableOutput("spend_table")))
                     ),
                     fluidRow(class = "info-row",
                              column(6, div(class = "info-box",
                                            h4("Activity Percentage", class = "box-title"),
                                            tableOutput("activity_percentage_table"))),
                              column(6, div(class = "info-box",
                                            h4("Spend Percentage", class = "box-title"),
                                            tableOutput("spend_percentage_table")))
                     ),
                     fluidRow(class = "info-row",
                              column(12, div(class = "info-box",
                                             h4("CPM/CPC", class = "box-title"),
                                             tableOutput("cpm_cpc"),
                                             div(class = "download-icon",
                                                 downloadButton("download_cpm_cpc", "Download CPM/CPC", class = "custom-download-btn"))
                              ))
                     )
                 )
        ),
        
        # TAB: Univariate
        tabPanel("Univariate",
                 div(class = "analysis-container",
                     div(class = "filters-section",
                         fluidRow(
                           column(12,
                                  div(class = "filter-box",
                                      fluidRow(
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("geography", "Geography", choices = c("All/Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("product", "Product", choices = c("All/Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("campaign", "Campaign", choices = c("Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("outlet", "Outlet", choices = c("Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("creative", "Creative", choices = c("Total"))))
                                      )
                                  )
                           )
                         )
                     ),
                     div(class = "transformation-section",
                         fluidRow(
                           column(12,
                                  div(class = "settings-box",
                                      h4("Transformation Configuration", class = "section-title"),
                                      fluidRow(
                                        column(2, div(class = "custom-numeric",
                                                      numericInput("decay_univ", "Decay", value = 1, min = 0, step = 0.1))),
                                        column(2, div(class = "custom-numeric",
                                                      numericInput("lag_univ", "Lag", value = 0, min = 0))),
                                        conditionalPanel(
                                          condition = "input.transformation_univ != 'Linear'",
                                          column(2, div(class = "custom-numeric",
                                                        numericInput("alpha_univ", "Alpha", value = 0.85, min = 0, step = 0.01))),
                                          column(2, div(class = "custom-numeric",
                                                        numericInput("beta_univ", "Beta", value = 1, min = 0, step = 1))),
                                          column(2, div(class = "custom-numeric",
                                                        numericInput("maxval_univ", "% MaxVal", value = 100, min = 0, step = 1)))
                                        )
                                      )
                                  )
                           )
                         )
                     ),
                     fluidRow(
                       column(3,
                              div(class = "selection-box",
                                  h4("Variable Selection", class = "section-title"),
                                  dateRangeInput("date_range_univ", "Select Date Range:",
                                                 start = Sys.Date() - 365, end = Sys.Date()),
                                  selectInput("kpi_univ", "KPI", choices = NULL),
                                  selectInput("variable_univ", "Variable", choices = NULL),
                                  hr(class = "custom-hr"),
                                  h4("Transformation", class = "section-title"),
                                  radioButtons("transformation_univ", NULL,
                                               choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"),
                                               selected = "Linear")
                              )
                       ),
                       column(9,
                              div(class = "charts-container",
                                  fluidRow(
                                    column(6,
                                           div(class = "chart-box",
                                               h4("Variable Flighting", class = "chart-title"),
                                               plotlyOutput("variable_flighting_chart")
                                           )
                                    ),
                                    column(6,
                                           div(class = "chart-box",
                                               h4("Transformed Variable", class = "chart-title"),
                                               plotlyOutput("var_transf_chart")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           div(class = "chart-box",
                                               h4("S-Curve EDA", class = "chart-title"),
                                               plotlyOutput("s_curve_univariate_plot", height = "400px")
                                           )
                                    )
                                  )
                              )
                       )
                     )
                 )
        ),
        
        # TAB: Multivariate
        tabPanel("Multivariate",
                 div(class = "analysis-container",
                     div(class = "filters-section",
                         fluidRow(
                           column(12,
                                  div(class = "filter-box",
                                      fluidRow(
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("geography_multi", "Geography", choices = c("All/Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("product_multi", "Product", choices = c("All/Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("campaign_multi", "Campaign", choices = c("Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("outlet_multi", "Outlet", choices = c("Total")))),
                                        column(2, div(class = "custom-select-container",
                                                      selectInput("creative_multi", "Creative", choices = c("Total"))))
                                      )
                                  )
                           )
                         )
                     ),
                     fluidRow(
                       column(3,
                              div(class = "selection-box",
                                  h4("Variable Selection", class = "section-title"),
                                  dateRangeInput("date_range_multi", "Select Date Range:",
                                                 start = Sys.Date() - 365, end = Sys.Date()),
                                  radioButtons("sum_all_vars", "Sum all vars",
                                               choices = c("Yes" = "true", "No" = "false"), inline = TRUE),
                                  selectInput("kpi_multi", "KPI", choices = NULL),
                                  selectInput("var1_multi", "Variable 1", choices = NULL),
                                  selectInput("var2_multi", "Variable 2", choices = NULL),
                                  selectInput("var3_multi", "Variable 3", choices = NULL),
                                  selectInput("var4_multi", "Variable 4", choices = c("None" = "None")),
                                  
                                  conditionalPanel(
                                    condition = "input.sum_all_vars == 'true'",
                                    div(class = "transformation-options",
                                        h4("Variable Transformations", class = "section-title"),
                                        selectInput("trans_var1", "Transform Variable 1",
                                                    choices = c("Linear", "S Origin", "S Shaped", "Log", "Exp", "Power")),
                                        selectInput("trans_var2", "Transform Variable 2",
                                                    choices = c("Linear", "S Origin", "S Shaped", "Log", "Exp", "Power")),
                                        selectInput("trans_var3", "Transform Variable 3",
                                                    choices = c("Linear", "S Origin", "S Shaped", "Log", "Exp", "Power")),
                                        selectInput("trans_var4", "Transform Variable 4",
                                                    choices = c("Linear", "S Origin", "S Shaped", "Log", "Exp", "Power"))
                                    )
                                  )
                              )
                       ),
                       column(9,
                              div(class = "analysis-content",
                                  div(class = "settings-box",
                                      h4("Transformation Settings", class = "section-title"),
                                      conditionalPanel(
                                        condition = "input.sum_all_vars == 'true'",
                                        fluidRow(
                                          column(2, numericInput("decay_multi", "Decay", value = 1, min = 0, step = 0.1)),
                                          column(2, numericInput("lag_multi", "Lag", value = 0, min = 0)),
                                          column(2, numericInput("alpha_multi", "Alpha", value = 0.85, min = 0, step = 0.01)),
                                          column(2, numericInput("beta_multi", "Beta", value = 1, min = 0, step = 0.1)),
                                          column(2, numericInput("maxval_multi", "% MaxVal", value = 100, min = 0, step = 1))
                                        )
                                      )
                                  ),
                                  div(class = "charts-container",
                                      fluidRow(
                                        column(6,
                                               div(class = "chart-box",
                                                   h4("Variables Chart", class = "chart-title"),
                                                   plotOutput("variables_chart_multi", height = "350px")
                                               )
                                        ),
                                        column(6,
                                               div(class = "chart-box",
                                                   h4("Distribution Analysis", class = "chart-title"),
                                                   plotOutput("boxplot_multi", height = "250px")
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               div(class = "chart-box",
                                                   h4("Correlation Analysis", class = "chart-title"),
                                                   plotOutput("corr_matrix_multi", height = "250px")
                                               )
                                        )
                                      )
                                  )
                              )
                       )
                     )
                 )
        ),
        
        # NUEVA PESTAÑA: S-Curve EDA
        tabPanel("S-Curve EDA",
                 fluidRow(
                   column(12,
                          plotlyOutput("s_curve_eda_plot", height = "800px")
                   )
                 )
        )
      )
    )
  )
)