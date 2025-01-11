# server.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(zoo)
library(stringr)
library(DT)
library(plotly)

# Fuentes externas
source("EDA/Scripts/EDA Functions.R")             # s_curve_transform, etc.
source("EDA/Scripts/S-Curve EDA Combination.R")    # create_flighting_chart + create_s_curve_chart

server <- function(input, output, session) {
  
  # Valores reactivos para almacenar datos y mapeos
  rv <- reactiveValues(
    data             = NULL,
    filtered_data    = NULL,
    variable_mapping = NULL
  )
  
  # Función para parsear nombres de variables
  parse_variable_name <- function(var_name){
    parts <- str_split(var_name, "_")[[1]]
    if(length(parts) > 5){
      return(list(
        variable_name = paste(parts[1:(length(parts)-4)], collapse = "_"),
        product       = parts[length(parts)-3],
        campaign      = parts[length(parts)-2],
        outlet        = parts[length(parts)-1],
        creative      = parts[length(parts)]
      ))
    }
    return(NULL)
  }
  
  # Función para aplicar transformaciones
  apply_transformation <- function(data,
                                   type,
                                   alpha = 0.85,
                                   beta = 1,
                                   maxval = 100,
                                   decay = 1,
                                   lag = 0){
    if(is.character(data)){
      return(data)
    }
    data <- as.numeric(data)
    if(lag > 0){
      data <- dplyr::lag(data, lag)
    }
    if(decay != 1){
      data <- data * decay
    }
    
    transformed <- switch(type,
      "Linear"    = data,
      "S Origin"  = s_curve_transform(data, shape = "s-origin", alpha = alpha, beta = beta, maxValuePct = maxval),
      "S Shaped"  = s_curve_transform(data, shape = "s-shaped", alpha = alpha, beta = beta, maxValuePct = maxval),
      "Index Exp" = exp(data / 100) * 100,
      "Log"       = log1p(data),
      "Exp"       = exp(data),
      "Power"     = data ^ beta,
      "Moving Avg"= zoo::rollmean(data, k = 3, fill = NA),
      data
    )
    transformed
  }
  
  # Reactive para cargar datos según el tipo de archivo
  loaded_data <- reactive({
    req(input$file)
    file <- input$file
    ext <- tools::file_ext(file$name)
    
    tryCatch({
      if(ext == "csv"){
        data <- read.csv(file$datapath, stringsAsFactors = FALSE)
      } else if(ext == "RData"){
        # Cargar .RData y buscar el primer data.frame
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
      showNotification(paste("Error al cargar el archivo:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Observador para cargar datos cuando se sube un archivo
  observeEvent(loaded_data(), {
    rv$data <- loaded_data()
    
    req(rv$data)
    
    # Convertir columnas de fecha si existen
    if("Period" %in% names(rv$data)){
      rv$data$Period <- as.Date(rv$data$Period)
    }
    if("periodo" %in% names(rv$data)){
      rv$data$periodo <- as.Date(rv$data$periodo)
    }
    
    # Filtrar datos por rango de fecha
    rv$filtered_data <- rv$data %>%
      filter(
        if("Period" %in% names(rv$data)){
          Period >= input$date_filter[1] & Period <= input$date_filter[2]
        } else if("periodo" %in% names(rv$data)){
          periodo >= input$date_filter[1] & periodo <= input$date_filter[2]
        } else TRUE
      )
    
    # Mapeo de variables
    variable_cols <- grep("variablename_", names(rv$data), value = TRUE)
    if(length(variable_cols) > 0){
      rv$variable_mapping <- lapply(variable_cols, parse_variable_name)
      names(rv$variable_mapping) <- variable_cols
      
      unique_products  <- unique(sapply(rv$variable_mapping, function(x) x$product))
      unique_campaigns <- unique(sapply(rv$variable_mapping, function(x) x$campaign))
      unique_outlets   <- unique(sapply(rv$variable_mapping, function(x) x$outlet))
      unique_creatives <- unique(sapply(rv$variable_mapping, function(x) x$creative))
      
      # Actualizar selectInputs con opciones únicas
      updateSelectInput(session, "product",  choices = c("All/Total", unique_products))
      updateSelectInput(session, "campaign", choices = c("Total", unique_campaigns))
      updateSelectInput(session, "outlet",   choices = c("Total", unique_outlets))
      updateSelectInput(session, "creative", choices = c("Total", unique_creatives))
      
      updateSelectInput(session, "product_multi",  choices = c("All/Total", unique_products))
      updateSelectInput(session, "campaign_multi", choices = c("Total", unique_campaigns))
      updateSelectInput(session, "outlet_multi",   choices = c("Total", unique_outlets))
      updateSelectInput(session, "creative_multi", choices = c("Total", unique_creatives))
    }
  })
  
  # Observador para filtrar datos según inputs
  observe({
    req(rv$data)
    filtered <- rv$data
    
    # Filtrar por geografía si aplica
    if(input$geography != "All/Total"){
      if("Geography" %in% names(filtered)){
        filtered <- filtered %>% filter(Geography == input$geography)
      } else if("geografia" %in% names(filtered)){
        filtered <- filtered %>% filter(geografia == input$geography)
      }
    }
    
    # Filtrar por variables si aplica
    if(!is.null(rv$variable_mapping)){
      if(input$product != "All/Total"){
        vars_to_remove <- names(rv$variable_mapping)[sapply(rv$variable_mapping, function(x) x$product != input$product)]
        if(length(vars_to_remove) > 0){
          filtered <- filtered %>% select(-all_of(vars_to_remove))
        }
      }
      if(input$campaign != "Total"){
        vars_to_remove <- names(rv$variable_mapping)[sapply(rv$variable_mapping, function(x) x$campaign != input$campaign)]
        if(length(vars_to_remove) > 0){
          filtered <- filtered %>% select(-all_of(vars_to_remove))
        }
      }
      if(input$outlet != "Total"){
        vars_to_remove <- names(rv$variable_mapping)[sapply(rv$variable_mapping, function(x) x$outlet != input$outlet)]
        if(length(vars_to_remove) > 0){
          filtered <- filtered %>% select(-all_of(vars_to_remove))
        }
      }
      if(input$creative != "Total"){
        vars_to_remove <- names(rv$variable_mapping)[sapply(rv$variable_mapping, function(x) x$creative != input$creative)]
        if(length(vars_to_remove) > 0){
          filtered <- filtered %>% select(-all_of(vars_to_remove))
        }
      }
    }
    
    # Filtrar por rango de fecha univ
    date_col <- if("Period" %in% names(filtered)) "Period" else if("periodo" %in% names(filtered)) "periodo" else NULL
    if(!is.null(date_col)){
      filtered[[date_col]] <- as.Date(filtered[[date_col]])
      filtered <- filtered %>%
        filter(
          !!sym(date_col) >= input$date_range_univ[1],
          !!sym(date_col) <= input$date_range_univ[2]
        )
    }
    
    rv$filtered_data <- filtered
  })
  
  # Observador para actualizar selectInputs basados en datos cargados
  observe({
    req(rv$data)
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    
    # Identificar variables de media y gasto
    MEDIA_VARIABLES <- grep("(Impressions)|(Circulation)|(Clicks)|(Display)|(OOH)|(OLV)|(Magazine)|(Newspaper)", 
                           names(rv$data), value = TRUE)
    SPEND_VARIABLES <- grep("(Cost)|(Spend)", MEDIA_VARIABLES, value = TRUE)
    MEDIA_VARIABLES <- setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
    MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
    SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)
    
    # Actualizar selectInputs
    updateSelectInput(session, "kpi",        choices = numeric_cols)
    updateSelectInput(session, "media_vars", choices = MEDIA_VARIABLES)
    updateSelectInput(session, "spend_vars", choices = SPEND_VARIABLES)
    updateSelectInput(session, "base_vars",  choices = numeric_cols)
    
    # Actualizar selectInputs de geografía
    if("Geography" %in% names(rv$data)){
      updateSelectInput(session, "geography",       choices = c("All/Total", unique(rv$data$Geography)))
      updateSelectInput(session, "geography_multi", choices = c("All/Total", unique(rv$data$Geography)))
    } else if("geografia" %in% names(rv$data)){
      updateSelectInput(session, "geography",       choices = c("All/Total", unique(rv$data$geografia)))
      updateSelectInput(session, "geography_multi", choices = c("All/Total", unique(rv$data$geografia)))
    }
    
    # Actualizar selectInputs para univariate y multivariate
    updateSelectInput(session, "kpi_univ",      choices = numeric_cols)
    updateSelectInput(session, "variable_univ", choices = numeric_cols)
    
    updateSelectInput(session, "kpi_multi",   choices = numeric_cols)
    updateSelectInput(session, "var1_multi",  choices = numeric_cols)
    updateSelectInput(session, "var2_multi",  choices = numeric_cols)
    updateSelectInput(session, "var3_multi",  choices = numeric_cols)
    updateSelectInput(session, "var4_multi",  choices = c("None", numeric_cols))
  })
  
  # TAB: Information - KPIs y tablas
  output$kpi_table <- renderTable({
    req(rv$filtered_data, input$kpi)
    rv$filtered_data %>%
      summarise(
        Mean = mean(.data[[input$kpi]], na.rm = TRUE),
        SD   = sd(.data[[input$kpi]], na.rm = TRUE),
        Min  = min(.data[[input$kpi]], na.rm = TRUE),
        Max  = max(.data[[input$kpi]], na.rm = TRUE)
      )
  })
  
  output$media_table <- renderTable({
    req(rv$filtered_data, input$media_vars)
    rv$filtered_data %>%
      summarise(across(all_of(input$media_vars),
                       list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))))
  })
  
  output$activity_table <- renderTable({
    req(rv$filtered_data, input$media_vars)
    rv$filtered_data %>%
      summarise(across(all_of(input$media_vars),
                       ~mean(. > 0, na.rm = TRUE) * 100)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Activity %")
  })
  
  output$spend_table <- renderTable({
    req(rv$filtered_data, input$spend_vars)
    total_spend <- sum(rv$filtered_data[[input$spend_vars[1]]], na.rm = TRUE)
    rv$filtered_data %>%
      summarise(across(all_of(input$spend_vars),
                       ~sum(as.numeric(.), na.rm = TRUE) / total_spend * 100)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Spend %")
  })
  
  output$activity_percentage_table <- renderTable({
    req(rv$filtered_data, input$media_vars)
    total_activity <- sum(rv$filtered_data[input$media_vars], na.rm = TRUE)
    rv$filtered_data %>%
      summarise(across(all_of(input$media_vars),
                       ~ (sum(., na.rm = TRUE) / total_activity) * 100)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Activity_Percentage") %>%
      mutate(Activity_Percentage = round(Activity_Percentage, 2)) %>%
      select(Variable, Activity_Percentage)
  })
  
  output$spend_percentage_table <- renderTable({
    req(rv$filtered_data, input$spend_vars)
    total_spend <- sum(rv$filtered_data[input$spend_vars], na.rm = TRUE)
    rv$filtered_data %>%
      summarise(across(all_of(input$spend_vars),
                       ~ (sum(., na.rm = TRUE) / total_spend) * 100)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Spend_Percentage") %>%
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
      mutate(CPM_CPC = ifelse(Activity > 0, round((Spend / Activity) * 1000, 2), NA))
    
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
        mutate(CPM_CPC = ifelse(Activity > 0, round((Spend / Activity) * 1000, 2), NA))
      
      write.csv(combined_stats, file, row.names = FALSE)
    }
  )
  
  # TAB: Univariate - Gráficos
  output$kpi_vs_variable_plot <- renderPlotly({
    req(rv$filtered_data, input$kpi_univ, input$variable_univ)
    p <- ggplot(rv$filtered_data, aes(x = .data[[input$variable_univ]], y = .data[[input$kpi_univ]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = "KPI vs Variable",
           x = input$variable_univ,
           y = input$kpi_univ)
    ggplotly(p)
  })
  
  output$variable_flighting_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    data_chart <- rv$filtered_data
    
    date_col <- if("Period" %in% names(data_chart)) "Period" else "periodo"
    plotly::plot_ly(data_chart, x = ~get(date_col)) %>%
      plotly::add_lines(y = ~get(input$variable_univ), name = "Variable Flighting", line = list(color = "blue")) %>%
      plotly::layout(
        title = "Variable Flighting Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
  # TAB: Univariate - Transformed Variable
  output$var_transf_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ, input$transformation_univ)
    
    var_name <- input$variable_univ
    trans_type <- input$transformation_univ
    alpha <- input$alpha_univ
    beta <- input$beta_univ
    maxval <- input$maxval_univ
    decay <- input$decay_univ
    lag <- input$lag_univ
    
    transformed_data <- apply_transformation(
      rv$filtered_data[[var_name]],
      type = trans_type,
      alpha = alpha,
      beta = beta,
      maxval = maxval,
      decay = decay,
      lag = lag
    )
    
    date_col <- if("Period" %in% names(rv$filtered_data)) "Period" else "periodo"
    
    df_transformed <- rv$filtered_data %>%
      mutate(Transformed = transformed_data) %>%
      select(!!sym(date_col), Transformed) %>%
      filter(!is.na(Transformed))
    
    p <- ggplot(df_transformed, aes(x = .data[[date_col]], y = Transformed)) +
      geom_line(color = "red") +
      theme_minimal() +
      labs(title = paste("Transformed Variable:", var_name),
           x = "Time",
           y = "Transformed Value")
    
    ggplotly(p)
  })
  
  # TAB: Univariate - S-Curve EDA Gráfico Integrado
  output$s_curve_univariate_plot <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    
    var_name    <- input$variable_univ
    alpha       <- input$alpha_univ
    beta        <- input$beta_univ
    max_val_pct <- input$maxval_univ
    decay       <- input$decay_univ
    lag         <- input$lag_univ
    
    # Construir dataframe para S-Curve
    df_scurve <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period) else if("periodo" %in% names(.)) as.Date(periodo) else as.Date(NA)) %>%
      select(Period, value = !!sym(var_name)) %>%
      filter(!is.na(Period))
    
    if(nrow(df_scurve) == 0){
      showNotification("No hay datos disponibles para crear la S-Curve.", type = "error")
      return(NULL)
    }
    
    # Crear gráficos
    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart:", e$message), type = "error")
      return(NULL)
    })
    
    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart:", e$message), type = "error")
      return(NULL)
    })
    
    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)){
      return(NULL)
    }
    
    # Combinar con subplot
    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows = 1, titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA")
  })
  
  # TAB: Multivariate - Gráficos
  output$variables_chart_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    data_chart <- rv$filtered_data
    date_col <- if("Period" %in% names(data_chart)) "Period" else "periodo"
    
    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }
    
    plot_data <- data_chart %>%
      select(!!sym(date_col), all_of(vars_to_select)) %>%
      pivot_longer(cols = -!!sym(date_col), names_to = "variable", values_to = "value")
    
    if(input$sum_all_vars == "true"){
      for(var in unique(plot_data$variable)){
        trans_type <- switch(var,
                             "var1_multi" = input$trans_var1,
                             "var2_multi" = input$trans_var2,
                             "var3_multi" = input$trans_var3,
                             "var4_multi" = input$trans_var4,
                             "Linear")
        
        plot_data$value[plot_data$variable == var] <- apply_transformation(
          plot_data$value[plot_data$variable == var],
          trans_type,
          alpha = input$alpha_multi,
          beta = input$beta_multi,
          maxval = input$maxval_multi,
          decay = input$decay_multi,
          lag = input$lag_multi
        )
      }
    }
    
    ggplot(plot_data, aes(x = .data[[date_col]], y = value, color = variable)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Multiple Variables over Time",
           x = "Period",
           y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$boxplot_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    data_chart <- rv$filtered_data
    
    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }
    
    plot_data <- data_chart %>%
      select(all_of(vars_to_select)) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
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
    
    # Excluir "None" y asegurar que las variables existen
    vars_to_correlate <- vars_to_correlate[vars_to_correlate != "None"]
    
    # Asegurar que las variables existen en los datos
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
    
    # Verificar si hay suficientes datos
    if(nrow(cor_data) < 2){
      showNotification("No hay suficientes datos para la matriz de correlación.", type = "error")
      return(NULL)
    }
    
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
    
    # Verificar si la matriz de correlación es válida
    if(any(is.na(cor_matrix))){
      showNotification("La matriz de correlación contiene valores NA. Verifica los datos ingresados.", type = "warning")
    }
    
    # Verificar si alguna columna tiene varianza cero
    if(any(apply(cor_matrix, 2, function(x) all(is.na(x))))){
      showNotification("Algunas variables no tienen correlación válida. Verifica los datos ingresados.", type = "warning")
    }
    
    corrplot(cor_matrix, method = "color",
             type = "upper",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             diag = FALSE,
             number.cex = 0.7)
  })
  
  output$download_analytical <- downloadHandler(
    filename = function(){ paste("analytical_transformed_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "") },
    content = function(file){
      download_data <- rv$filtered_data
      
      # Transformación univariante
      if(!is.null(input$variable_univ)){
        newcol <- paste0(input$variable_univ, "_transformed")
        download_data[[newcol]] <- apply_transformation(
          rv$filtered_data[[input$variable_univ]],
          type = input$transformation_univ,
          alpha = input$alpha_univ,
          beta = input$beta_univ,
          maxval = input$maxval_univ,
          decay = input$decay_univ,
          lag = input$lag_univ
        )
      }
      
      # Transformación multivariante
      if(input$sum_all_vars == "true"){
        vars_to_transform <- c(input$var1_multi, input$var2_multi, input$var3_multi)
        if(!is.null(input$var4_multi) && input$var4_multi != "None"){
          vars_to_transform <- c(vars_to_transform, input$var4_multi)
        }
        for(var in vars_to_transform){
          trans_type <- switch(var,
                               "var1_multi" = input$trans_var1,
                               "var2_multi" = input$trans_var2,
                               "var3_multi" = input$trans_var3,
                               "var4_multi" = input$trans_var4,
                               "Linear")
          newcol <- paste0(var, "_transformed")
          download_data[[newcol]] <- apply_transformation(
            rv$filtered_data[[var]],
            trans_type,
            alpha = input$alpha_multi,
            beta = input$beta_multi,
            maxval = input$maxval_multi,
            decay = input$decay_multi,
            lag = input$lag_multi
          )
        }
      }
      
      write.csv(download_data, file, row.names = FALSE, na = "")
    }
  )
  
  # TAB: S-Curve EDA - Gráfico Principal
  output$s_curve_eda_plot <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    
    var_name    <- input$variable_univ
    alpha       <- input$alpha_univ
    beta        <- input$beta_univ
    max_val_pct <- input$maxval_univ
    decay       <- input$decay_univ
    lag         <- input$lag_univ

    # Construir dataframe para S-Curve
    df_scurve <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period) else if("periodo" %in% names(.)) as.Date(periodo) else as.Date(NA)) %>%
      select(Period, value = !!sym(var_name)) %>%
      filter(!is.na(Period))
    
    if(nrow(df_scurve) == 0){
      showNotification("No hay datos disponibles para crear la S-Curve.", type = "error")
      return(NULL)
    }

    # Crear gráficos
    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart:", e$message), type = "error")
      return(NULL)
    })
    
    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart:", e$message), type = "error")
      return(NULL)
    })
    
    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)){
      return(NULL)
    }
    
    # Combinar con subplot
    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows = 1, titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA")
  })
  
  # TAB: Multivariate - S-Curve EDA Gráfico
  output$s_curve_multivariate_plot <- renderPlotly({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    
    # Crear una suma de variables seleccionadas (excluyendo "None")
    vars_to_sum <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_sum <- c(vars_to_sum, input$var4_multi)
    }
    
    # Asegurar que las variables existen en los datos
    available_vars <- vars_to_sum[vars_to_sum %in% names(rv$filtered_data)]
    if(length(available_vars) == 0){
      showNotification("No hay variables seleccionadas para crear la S-Curve Multivariada.", type = "error")
      return(NULL)
    }
    
    sum_variable <- rowSums(rv$filtered_data[available_vars], na.rm = TRUE)
    
    # Construir dataframe para S-Curve
    df_scurve_multi <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period) else if("periodo" %in% names(.)) as.Date(periodo) else as.Date(NA)) %>%
      mutate(value = sum_variable) %>%
      select(Period, value) %>%
      filter(!is.na(Period))
    
    if(nrow(df_scurve_multi) == 0){
      showNotification("No hay datos disponibles para crear la S-Curve Multivariada.", type = "error")
      return(NULL)
    }
    
    var_name <- "Sum of Selected Variables"
    alpha <- input$alpha_multi
    beta <- input$beta_multi
    max_val_pct <- input$maxval_multi
    decay <- input$decay_multi
    lag <- input$lag_multi
    
    # Crear gráficos
    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart Multivariado:", e$message), type = "error")
      return(NULL)
    })
    
    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart Multivariado:", e$message), type = "error")
      return(NULL)
    })
    
    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)){
      return(NULL)
    }
    
    # Combinar con subplot
    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows = 1, titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA Multivariado")
  })
  
}