library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(zoo)
library(stringr)
library(DT)
library(plotly)

# Fuentes externas (ajusta las rutas si es necesario)
source("EDA/Scripts/EDA Functions.R")          # s_curve_transform, etc.
source("EDA/Scripts/S-Curve EDA Combination.R") # create_flighting_chart + create_s_curve_chart

server <- function(input, output, session) {

  # ReactiveValues para almacenar datos
  rv <- reactiveValues(
    data = NULL,
    filtered_data = NULL
  )

  # -- 1) Carga de datos ---------------------------------------
  loaded_data <- reactive({
    req(input$file)
    file <- input$file
    ext  <- tools::file_ext(file$name)

    tryCatch({
      if(ext == "csv"){
        data <- read.csv(file$datapath, stringsAsFactors = FALSE)
      } else if(ext == "RData"){
        env <- new.env()
        load(file$datapath, envir = env)
        objs <- ls(env)
        data_objs <- objs[sapply(objs, function(x) is.data.frame(get(x, envir = env)))]
        if(length(data_objs) == 0){
          stop("El archivo .RData no contiene ningún data.frame.")
        }
        data <- get(data_objs[1], envir = env)
        if(!is.data.frame(data)){
          stop("El objeto seleccionado no es un data.frame.")
        }
      } else {
        stop("Formato de archivo no soportado. Por favor, suba un archivo .csv o .RData.")
      }
      return(data)
    }, error = function(e){
      showNotification(paste("Error al cargar el archivo:", e$message),
                       type = "error")
      return(NULL)
    })
  })

  # Observador que asigna rv$data cuando loaded_data() cambia
  observeEvent(loaded_data(), {
    rv$data <- loaded_data()
    req(rv$data)

    # Convertir columnas de fecha (Period o periodo) a Date
    if("Period" %in% names(rv$data)){
      rv$data$Period <- as.Date(rv$data$Period)
    }
    if("periodo" %in% names(rv$data)){
      rv$data$periodo <- as.Date(rv$data$periodo)
    }

    # Actualizar selects de variables
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]

    # Identificar media y spend
    MEDIA_VARIABLES <- grep("(Impressions)|(Circulation)|(Clicks)|(Display)|(OOH)|(OLV)|(Magazine)|(Newspaper)",
                            names(rv$data), value = TRUE)
    SPEND_VARIABLES <- grep("(Cost)|(Spend)", MEDIA_VARIABLES, value = TRUE)
    MEDIA_VARIABLES <- setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
    MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
    SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)

    # Actualizar inputs de "Data Management" y univariado/multivariado
    updateSelectInput(session, "kpi",        choices = numeric_cols)
    updateSelectInput(session, "media_vars", choices = MEDIA_VARIABLES)
    updateSelectInput(session, "spend_vars", choices = SPEND_VARIABLES)
    updateSelectInput(session, "base_vars",  choices = numeric_cols)

    updateSelectInput(session, "kpi_univ",      choices = numeric_cols)
    updateSelectInput(session, "variable_univ", choices = numeric_cols)
    updateSelectInput(session, "kpi_multi",     choices = numeric_cols)
    updateSelectInput(session, "var1_multi",    choices = numeric_cols)
    updateSelectInput(session, "var2_multi",    choices = numeric_cols)
    updateSelectInput(session, "var3_multi",    choices = numeric_cols)
    updateSelectInput(session, "var4_multi",    choices = c("None", numeric_cols))
  })

  # -- 2) Observador para filtrar datos según el mes seleccionado -----------
  observe({
    req(rv$data)
    df <- rv$data

    # Checar columna de fecha
    date_col <- NULL
    if("Period" %in% names(df))   date_col <- "Period"
    else if("periodo" %in% names(df)) date_col <- "periodo"

    if(!is.null(date_col)){
      # Convertimos la fecha a year-month
      df$year_month <- format(df[[date_col]], "%Y-%m")

      # Si no es "All", filtramos por el mes
      selected_month <- input$month_filter
      if(!is.null(selected_month) && selected_month != "All"){
        # Buscamos el índice del mes, p.ej. "Enero" -> 1 -> "01"
        month_num   <- which(month.name == selected_month)
        month_num2d <- sprintf("%02d", month_num)
        # Filtramos
        df <- df %>%
          filter(format(.data[[date_col]], "%m") == month_num2d)
      }

      df$year_month <- NULL
    }

    rv$filtered_data <- df
  })

  # Función genérica de transformación
  apply_transformation <- function(data, type,
                                   alpha = 0.85,
                                   beta = 1,
                                   maxval = 100,
                                   decay = 1,
                                   lag = 0){
    if(is.character(data)) return(data)
    data <- as.numeric(data)

    # Aplicamos lag y decay
    if(lag > 0)    data <- dplyr::lag(data, lag)
    if(decay != 1) data <- data * decay

    transformed <- switch(type,
      "Linear"    = data,
      "S Origin"  = s_curve_transform(data, shape = "s-origin",
                                      alpha = alpha, beta = beta,
                                      maxValuePct = maxval),
      "S Shaped"  = s_curve_transform(data, shape = "s-shaped",
                                      alpha = alpha, beta = beta,
                                      maxValuePct = maxval),
      "Index Exp" = exp(data / 100) * 100,
      "Log"       = log1p(data),
      "Exp"       = exp(data),
      "Power"     = data ^ beta,
      "Moving Avg"= zoo::rollmean(data, k = 3, fill = NA),
      data
    )
    return(transformed)
  }

  # ==============================================
  # ================ INFORMATION TAB =============
  # ==============================================

  output$activity_table <- renderTable({
    req(rv$filtered_data, input$media_vars)
    rv$filtered_data %>%
      summarise(across(all_of(input$media_vars),
                       ~ mean(. > 0, na.rm = TRUE) * 100)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Variable",
                   values_to = "Activity %")
  })

  output$spend_table <- renderTable({
    req(rv$filtered_data, input$spend_vars)
    total_spend <- sum(rv$filtered_data[[input$spend_vars[1]]], na.rm = TRUE)
    if(total_spend <= 0) total_spend <- 1  # evitar div/0
    rv$filtered_data %>%
      summarise(across(all_of(input$spend_vars),
                       ~ sum(as.numeric(.), na.rm = TRUE) / total_spend * 100)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Variable",
                   values_to = "Spend %")
  })

  output$activity_percentage_table <- renderTable({
    req(rv$filtered_data, input$media_vars)
    total_activity <- sum(rv$filtered_data[input$media_vars], na.rm = TRUE)
    if(total_activity <= 0) total_activity <- 1
    rv$filtered_data %>%
      summarise(across(all_of(input$media_vars),
                       ~ (sum(., na.rm = TRUE) / total_activity) * 100)) %>%
      pivot_longer(cols = everything(), names_to = "Variable",
                   values_to = "Activity_Percentage") %>%
      mutate(Activity_Percentage = round(Activity_Percentage, 2)) %>%
      select(Variable, Activity_Percentage)
  })

  output$spend_percentage_table <- renderTable({
    req(rv$filtered_data, input$spend_vars)
    total_spend <- sum(rv$filtered_data[input$spend_vars], na.rm = TRUE)
    if(total_spend <= 0) total_spend <- 1
    rv$filtered_data %>%
      summarise(across(all_of(input$spend_vars),
                       ~ (sum(., na.rm = TRUE) / total_spend) * 100)) %>%
      pivot_longer(cols = everything(), names_to = "Variable",
                   values_to = "Spend_Percentage") %>%
      mutate(Spend_Percentage = round(Spend_Percentage, 2)) %>%
      select(Variable, Spend_Percentage)
  })

  output$cpm_cpc <- renderTable({
    req(rv$filtered_data, input$media_vars, input$spend_vars)
    total_activity <- rv$filtered_data %>%
      summarise(across(all_of(input$media_vars), ~ sum(., na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Activity")

    total_spend <- rv$filtered_data %>%
      summarise(across(all_of(input$spend_vars), ~ sum(., na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Spend")

    combined_stats <- total_activity %>%
      mutate(Spend = total_spend$Spend) %>%
      mutate(CPM_CPC = ifelse(Activity > 0,
                              round((Spend / Activity) * 1000, 2),
                              NA))
    combined_stats
  })

  output$download_cpm_cpc <- downloadHandler(
    filename = function() {
      paste("CPM_CPC_Table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      req(rv$filtered_data, input$media_vars, input$spend_vars)

      total_activity <- rv$filtered_data %>%
        summarise(across(all_of(input$media_vars), ~ sum(., na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Activity")

      total_spend <- rv$filtered_data %>%
        summarise(across(all_of(input$spend_vars), ~ sum(., na.rm = TRUE))) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Spend")

      combined_stats <- total_activity %>%
        mutate(Spend = total_spend$Spend) %>%
        mutate(CPM_CPC = ifelse(Activity > 0,
                                round((Spend / Activity) * 1000, 2),
                                NA))

      write.csv(combined_stats, file, row.names = FALSE)
    }
  )

  # ==============================================
  # ================ UNIVARIATE TAB ==============
  # ==============================================

  # 1. Variable Flighting
  output$variable_flighting_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)

    date_col <- if("Period" %in% names(rv$filtered_data)) {
      "Period"
    } else if("periodo" %in% names(rv$filtered_data)) {
      "periodo"
    } else {
      NULL
    }
    req(date_col, input$variable_univ)

    p <- ggplot(rv$filtered_data,
                aes_string(x = date_col, y = input$variable_univ)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Variable Flighting Over Time",
           x = "Time",
           y = input$variable_univ)

    ggplotly(p)
  })

  # 2. Variable Transformada
  output$var_transf_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ, input$transformation_univ)

    var_name <- input$variable_univ
    transformed_data <- apply_transformation(
      rv$filtered_data[[var_name]],
      type   = input$transformation_univ,
      alpha  = input$alpha_univ,
      beta   = input$beta_univ,
      maxval = input$maxval_univ,
      decay  = input$decay_univ,
      lag    = input$lag_univ
    )

    df_trans <- rv$filtered_data %>%
      mutate(Transformed = transformed_data)

    date_col <- if("Period" %in% names(rv$filtered_data)) "Period" else "periodo"

    p <- ggplot(df_trans, aes_string(x = date_col, y = "Transformed")) +
      geom_line(color = "red") +
      theme_minimal() +
      labs(title = paste("Transformed Variable:", var_name),
           x = "Time",
           y = "Transformed Value")

    ggplotly(p)
  })

  # 3. Boxplot (reemplaza la Curva S principal)
  output$boxplot_univ <- renderPlot({
    req(rv$filtered_data, input$variable_univ)
    box_data <- rv$filtered_data[[input$variable_univ]]
    box_data <- box_data[!is.na(box_data)]

    if(length(box_data) < 1){
      showNotification("No hay datos suficientes para el boxplot.", type = "error")
      return(NULL)
    }

    ggplot(data.frame(val = box_data), aes(x = "", y = val)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal() +
      labs(x = "", y = input$variable_univ)
  })

  # 4. Texto con transformaciones aplicadas
  output$transformations_summary_univ <- renderPrint({
    req(input$transformation_univ)
    cat("Transformación seleccionada:", input$transformation_univ, "\n")
    if(input$transformation_univ %in% c("S Origin", "S Shaped")){
      cat("Alpha:", input$alpha_univ, "\n")
      cat("Beta:", input$beta_univ, "\n")
      cat("MaxVal%:", input$maxval_univ, "\n")
    }
    cat("Decay:", input$decay_univ, "\n")
    cat("Lag:", input$lag_univ, "\n")
  })

  # 5. Curva S condicional (solo si "S Origin")
  output$s_curve_univariate_plot <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    validate(
      need(input$transformation_univ == "S Origin",
           "Se muestra solo si 'S Origin'.")
    )

    var_name    <- input$variable_univ
    alpha       <- input$alpha_univ
    beta        <- input$beta_univ
    max_val_pct <- input$maxval_univ
    decay       <- input$decay_univ
    lag         <- input$lag_univ

    df_scurve <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period)
             else if("periodo" %in% names(.)) as.Date(periodo)
             else as.Date(NA)) %>%
      select(Period, value = !!sym(var_name)) %>%
      filter(!is.na(Period))

    if(nrow(df_scurve) == 0){
      showNotification("No hay datos disponibles para crear la S-Curve.", type = "error")
      return(NULL)
    }

    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart  = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart:", e$message),
                       type = "error")
      return(NULL)
    })

    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart  = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart:", e$message),
                       type = "error")
      return(NULL)
    })

    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)){
      return(NULL)
    }

    subplot(flighting_plot_gg, s_curve_plot_gg, nrows = 1,
            titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA")
  })

  # ==============================================
  # ================ MULTIVARIATE TAB ============
  # ==============================================
  output$variables_chart_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)

    data_chart <- rv$filtered_data
    date_col <- if("Period" %in% names(data_chart)) "Period" else "periodo"

    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }

    plot_data <- data_chart %>%
      select(all_of(vars_to_select), !!sym(date_col)) %>%
      pivot_longer(cols = all_of(vars_to_select),
                   names_to = "variable",
                   values_to = "value")

    # Si sum_all_vars == "true", transformamos cada variable
    if(input$sum_all_vars == "true"){
      for(var_i in unique(plot_data$variable)) {

        # Determinamos el tipo de transformación según la variable actual
        trans_type <- if(var_i == input$var1_multi){
          input$trans_var1
        } else if(var_i == input$var2_multi){
          input$trans_var2
        } else if(var_i == input$var3_multi){
          input$trans_var3
        } else if(var_i == input$var4_multi){
          input$trans_var4
        } else {
          "Linear"
        }

        idx <- plot_data$variable == var_i
        plot_data$value[idx] <- apply_transformation(
          plot_data$value[idx],
          trans_type,
          alpha  = input$alpha_multi,
          beta   = input$beta_multi,
          maxval = input$maxval_multi,
          decay  = input$decay_multi,
          lag    = input$lag_multi
        )
      }
    }

    ggplot(plot_data, aes_string(x = date_col, y = "value", color = "variable")) +
      geom_line() +
      theme_minimal() +
      labs(title = "Multiple Variables over Time",
           x = "Period",
           y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$boxplot_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)

    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }

    plot_data <- rv$filtered_data %>%
      select(all_of(vars_to_select)) %>%
      pivot_longer(cols = everything(),
                   names_to = "variable",
                   values_to = "value") %>%
      filter(!is.na(value))

    ggplot(plot_data, aes(x = variable, y = as.numeric(value))) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Distribution of Variables", x = "Variable", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$corr_matrix_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)

    vars_to_correlate <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_correlate <- c(vars_to_correlate, input$var4_multi)
    }
    vars_to_correlate <- vars_to_correlate[vars_to_correlate != "None"]
    vars_to_correlate <- vars_to_correlate[vars_to_correlate %in% names(rv$filtered_data)]

    if(length(vars_to_correlate) < 2){
      showNotification("Se requieren al menos dos variables para la matriz de correlación.", type = "error")
      return(NULL)
    }

    cor_data <- rv$filtered_data %>%
      select(all_of(vars_to_correlate)) %>%
      mutate(across(everything(), as.numeric))

    # Eliminar columnas con varianza cero
    cor_data <- cor_data %>%
      select(where(~ sd(.) > 0))

    if(ncol(cor_data) < 2){
      showNotification("No hay suficientes variables con varianza positiva para la matriz de correlación.", type = "error")
      return(NULL)
    }
    if(nrow(cor_data) < 2){
      showNotification("No hay suficientes datos para la matriz de correlación.", type = "error")
      return(NULL)
    }

    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
    if(any(is.na(cor_matrix))){
      showNotification("La matriz de correlación contiene valores NA.", type = "warning")
    }

    corrplot(cor_matrix, method = "color",
             type = "upper",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             diag = FALSE,
             number.cex = 0.7)
  })

  # S-Curve Multivariado (condicional)
  output$s_curve_multivariate_plot <- renderPlotly({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)

    vars_to_sum <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_sum <- c(vars_to_sum, input$var4_multi)
    }

    # Aseguramos que existan en filtered_data
    available_vars <- vars_to_sum[vars_to_sum %in% names(rv$filtered_data)]
    if(length(available_vars) == 0){
      showNotification("No hay variables para la S-Curve Multivariada.", type = "error")
      return(NULL)
    }

    sum_variable <- rowSums(rv$filtered_data[available_vars], na.rm = TRUE)

    df_scurve_multi <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period)
             else if("periodo" %in% names(.)) as.Date(periodo)
             else as.Date(NA)) %>%
      mutate(value = sum_variable) %>%
      select(Period, value) %>%
      filter(!is.na(Period))

    if(nrow(df_scurve_multi) == 0){
      showNotification("No hay datos disponibles para la S-Curve Multivariada.", type = "error")
      return(NULL)
    }

    var_name    <- "Sum of Selected Variables"
    alpha       <- input$alpha_multi
    beta        <- input$beta_multi
    max_val_pct <- input$maxval_multi
    decay       <- input$decay_multi
    lag         <- input$lag_multi

    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart  = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart Multivariado:", e$message),
                       type = "error")
      return(NULL)
    })

    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart  = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart Multivariado:", e$message),
                       type = "error")
      return(NULL)
    })

    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)) {
      return(NULL)
    }

    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows = 1, titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA Multivariado")
  })

  # =============================================
  # ============ DOWNLOAD ANALYTICAL ============
  # =============================================
  output$download_analytical <- downloadHandler(
    filename = function(){ paste0("analytical_transformed_", Sys.Date(), ".csv") },
    content = function(file){
      download_data <- rv$filtered_data

      # Transformación univar
      if(!is.null(input$variable_univ)){
        newcol <- paste0(input$variable_univ, "_transformed")
        download_data[[newcol]] <- apply_transformation(
          rv$filtered_data[[input$variable_univ]],
          type   = input$transformation_univ,
          alpha  = input$alpha_univ,
          beta   = input$beta_univ,
          maxval = input$maxval_univ,
          decay  = input$decay_univ,
          lag    = input$lag_univ
        )
      }

      # Transformaciones multivariables si sum_all_vars == "true"
      if(input$sum_all_vars == "true"){
        vars_to_transform <- c(input$var1_multi, input$var2_multi, input$var3_multi)
        if(!is.null(input$var4_multi) && input$var4_multi != "None"){
          vars_to_transform <- c(vars_to_transform, input$var4_multi)
        }

        for(var_i in vars_to_transform){
          trans_type <- if(var_i == input$var1_multi){
            input$trans_var1
          } else if(var_i == input$var2_multi){
            input$trans_var2
          } else if(var_i == input$var3_multi){
            input$trans_var3
          } else if(var_i == input$var4_multi){
            input$trans_var4
          } else {
            "Linear"
          }

          newcol <- paste0(var_i, "_transformed")
          download_data[[newcol]] <- apply_transformation(
            rv$filtered_data[[var_i]],
            trans_type,
            alpha  = input$alpha_multi,
            beta   = input$beta_multi,
            maxval = input$maxval_multi,
            decay  = input$decay_multi,
            lag    = input$lag_multi
          )
        }
      }

      write.csv(download_data, file, row.names = FALSE, na = "")
    }
  )
}