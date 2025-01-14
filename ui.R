library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  # Load required dependencies
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap", rel = "stylesheet"),
    # Add meta tags for responsive design
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    # Add custom CSS for improved styling
    tags$style(HTML("
      .tooltip { max-width: 300px; text-align: left; }
      .custom-sidebar { max-height: calc(100vh - 100px); overflow-y: auto; }
      .info-box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px; }
      .chart-box { background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .section-title { border-bottom: 2px solid #f0f0f0; padding-bottom: 10px; margin-bottom: 20px; }
      .custom-download-btn { background-color: #007bff; color: white; border: none; border-radius: 4px; }
      .custom-download-btn:hover { background-color: #0056b3; }
    "))
  ),

  theme = shinytheme("flatly"),
  
  # Header
  div(class = "app-header bg-primary text-white py-4",
      titlePanel(
        div(class = "title-container text-center",
            h1("EDA", class = "main-title display-4"),
            p("Exploratory Data Analysis Tool", class = "subtitle lead")
        )
      )
  ),

  # En lugar de un sidebarLayout fijo, usamos un layout condicional:
  # - Si estamos en la pestaña 'Información', mostramos Data Management a la izquierda.
  # - En las demás pestañas, ocultamos el panel lateral y usamos el espacio completo.
  
  navbarPage(
    id = "main-tabs",
    title = NULL,
    theme = shinytheme("flatly"),
    
    # Pestaña INFORMACIÓN (única pestaña con Data Management visible)
    tabPanel("Información",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "custom-sidebar",
          
          # -- Data Management: solo aquí --
          wellPanel(
            h4("Data Management", class = "section-title"),
            div(
              title = "Upload your analytical file (CSV or RData format)",
              fileInput("file", "Upload Analytical",
                       accept = c(".csv", ".RData"),
                       buttonLabel = "Browse...",
                       placeholder = "No file selected")
            ),
            tags$small("Supported formats: CSV, RData", class = "text-muted"),
            
            # Selector de MES (o TODOS)
            # Notar que con un vector de strings (Enero, Febrero, etc.) se podría usar month.name.
            # Para simplificar, se incluyen 12 meses y la opción "All".
            selectInput("month_filter",
                        "Seleccione Mes:",
                        choices = c("All", month.name),
                        selected = "All"),

            # Variables principales (KPI, media, spend, etc.)
            wellPanel(
              h4("Variable Configuration", class = "section-title"),
              div(
                title = "Select the main KPI for analysis",
                selectInput("kpi", "Select KPI", choices = NULL)
              ),
              div(
                title = "Select one or more media variables",
                selectInput("media_vars", "Select Media Variables",
                           choices = NULL,
                           multiple = TRUE)
              ),
              div(
                title = "Select one or more spend variables",
                selectInput("spend_vars", "Select Spend Variables",
                           choices = NULL,
                           multiple = TRUE)
              ),
              div(
                title = "Select one or more base variables",
                selectInput("base_vars", "Select Base Variables",
                           choices = NULL,
                           multiple = TRUE)
              )
            ),
            
            # Download
            wellPanel(
              downloadButton("download_analytical",
                             "Download Analytical",
                             class = "custom-download-btn btn-block")
            )
          )
        ),
        
        mainPanel(
          width = 9,
          class = "custom-main-panel",
          
          # Contenido principal de la pestaña "Información"
          div(class = "info-container",
              fluidRow(
                column(6,
                       div(class = "info-box",
                           h4("Activity Analysis", class = "box-title"),
                           tableOutput("activity_table"))
                ),
                column(6,
                       div(class = "info-box",
                           h4("Spend Distribution", class = "box-title"),
                           tableOutput("spend_table"))
                )
              ),
              
              fluidRow(
                column(6,
                       div(class = "info-box",
                           h4("Activity Percentage", class = "box-title"),
                           tableOutput("activity_percentage_table"))
                ),
                column(6,
                       div(class = "info-box",
                           h4("Spend Percentage", class = "box-title"),
                           tableOutput("spend_percentage_table"))
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "info-box",
                           h4("CPM/CPC Analysis", class = "box-title"),
                           tableOutput("cpm_cpc"),
                           div(class = "text-right mt-3",
                               downloadButton("download_cpm_cpc",
                                              "Download CPM/CPC",
                                              class = "custom-download-btn"))
                       ))
              )
          )
        )
      )
    ),

    # Pestaña UNIVARIATE (sin sidebar, ocupamos todo el ancho)
    tabPanel("Univariado",
      fluidPage(
        # Transformaciones, boxplot, etc.
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Transformation Settings", class = "section-title"),
                   fluidRow(
                     column(2,
                       numericInput("decay_univ", "Decay", value = 1, min = 0, step = 0.1)
                     ),
                     column(2,
                       numericInput("lag_univ", "Lag", value = 0, min = 0)
                     ),
                     # Se muestra alpha, beta, maxval solo si la transformación es S-based
                     conditionalPanel(
                       condition = "input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped'",
                       column(2,
                         numericInput("alpha_univ", "Alpha", value = 0.85, min = 0, step = 0.01)
                       ),
                       column(2,
                         numericInput("beta_univ", "Beta", value = 1, min = 0, step = 0.1)
                       ),
                       column(2,
                         numericInput("maxval_univ", "% MaxVal", value = 100, min = 0, step = 1)
                       )
                     )
                   )
                 )
          )
        ),
        
        fluidRow(
          column(3,
                 wellPanel(
                   h4("Variable Selection", class = "section-title"),
                   # KPI y variable disponibles
                   selectInput("kpi_univ", "KPI", choices = NULL),
                   selectInput("variable_univ", "Variable", choices = NULL),
                   
                   h4("Transformation", class = "section-title mt-4"),
                   radioButtons("transformation_univ", NULL,
                     choices = c("Linear", "S Origin",
                                 "S Shaped", "Index Exp",
                                 "Log", "Exp", "Power",
                                 "Moving Avg"),
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
                     
                     # En lugar de la Curva S, va el Boxplot
                     fluidRow(
                       column(6,
                              div(class = "chart-box",
                                  h4("Boxplot", class = "chart-title"),
                                  plotOutput("boxplot_univ")  # NUEVO OUTPUT
                              )
                       ),
                       column(6,
                              div(class = "chart-box",
                                  h4("Transformaciones Aplicadas", class = "chart-title"),
                                  # Aquí simplemente mostramos un texto o tabla con la explicación
                                  verbatimTextOutput("transformations_summary_univ")
                              )
                       )
                     ),
                     
                     # Curva S condicional: solo si "S Origin" (o "S Shaped") 
                     # y si realmente la queremos mostrar.
                     # Por ejemplo, si la fuente de datos es S Origin, 
                     conditionalPanel(
                       condition = "input.transformation_univ == 'S Origin'",
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
      )
    ),

    # Pestaña MULTIVARIADO (sin sidebar)
    tabPanel("Multivariado",
      fluidPage(
        fluidRow(
          column(3,
            wellPanel(
              h4("Variable Selection", class = "section-title"),
              prettyRadioButtons("sum_all_vars", "Sum all variables",
                choices = c("Yes" = "true", "No" = "false"),
                inline = TRUE, status = "primary"
              ),
              selectInput("kpi_multi", "KPI", choices = NULL),
              selectInput("var1_multi", "Variable 1", choices = NULL),
              selectInput("var2_multi", "Variable 2", choices = NULL),
              selectInput("var3_multi", "Variable 3", choices = NULL),
              selectInput("var4_multi", "Variable 4", choices = c("None" = "None")),

              conditionalPanel(
                condition = "input.sum_all_vars == 'true'",
                div(class = "transformation-options",
                    h4("Variable Transformations", class = "section-title mt-4"),
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
                conditionalPanel(
                  condition = "input.sum_all_vars == 'true'",
                  wellPanel(
                    h4("Transformation Settings", class = "section-title"),
                    fluidRow(
                      column(2,
                             numericInput("decay_multi", "Decay",
                                         value = 1, min = 0, step = 0.1)),
                      column(2,
                             numericInput("lag_multi", "Lag",
                                         value = 0, min = 0)),
                      column(2,
                             numericInput("alpha_multi", "Alpha",
                                         value = 0.85, min = 0, step = 0.01)),
                      column(2,
                             numericInput("beta_multi", "Beta",
                                         value = 1, min = 0, step = 0.1)),
                      column(2,
                             numericInput("maxval_multi", "% MaxVal",
                                         value = 100, min = 0, step = 1))
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
                    ),
                    
                    # S-Curve EDA Multivariado (también condicional)
                    conditionalPanel(
                      condition = "input.trans_var1 == 'S Origin' || input.trans_var2 == 'S Origin' ||
                                   input.trans_var3 == 'S Origin' || input.trans_var4 == 'S Origin'",
                      fluidRow(
                        column(12,
                               div(class = "chart-box shadow",
                                   h4("S-Curve EDA Multivariado",
                                      class = "chart-title"),
                                   plotlyOutput("s_curve_multivariate_plot",
                                                height = "400px")
                               )
                        )
                      )
                    )
                )
            )
          )
        )
      )
    )

      )
)