# server.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(zoo)
library(stringr)
library(plotly)

source("EDA/Scripts/EDA Functions.R")             # s_curve_transform, etc.
source("EDA/Scripts/S-Curve EDA Combination.R")    # create_flighting_chart + create_s_curve_chart

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data             = NULL,
    filtered_data    = NULL,
    variable_mapping = NULL
  )
  
  observeEvent(input$file, {
    req(input$file)
    rv$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    if("Period" %in% names(rv$data)){
      rv$data$Period <- as.Date(rv$data$Period)
    }
    if("periodo" %in% names(rv$data)){
      rv$data$periodo <- as.Date(rv$data$periodo)
    }
    
    rv$filtered_data <- rv$data %>%
      filter(
        if("Period" %in% names(rv$data)){
          Period >= input$date_filter[1] & Period <= input$date_filter[2]
        } else if("periodo" %in% names(rv$data)){
          periodo >= input$date_filter[1] & periodo <= input$date_filter[2]
        } else TRUE
      )
    
    variable_cols <- grep("variablename_", names(rv$data), value=TRUE)
    if(length(variable_cols)>0){
      rv$variable_mapping <- lapply(variable_cols, parse_variable_name)
      names(rv$variable_mapping) <- variable_cols
      
      unique_products  <- unique(sapply(rv$variable_mapping, function(x)x$product))
      unique_campaigns <- unique(sapply(rv$variable_mapping, function(x)x$campaign))
      unique_outlets   <- unique(sapply(rv$variable_mapping, function(x)x$outlet))
      unique_creatives <- unique(sapply(rv$variable_mapping, function(x)x$creative))
      
      updateSelectInput(session,"product",  choices=c("All/Total", unique_products))
      updateSelectInput(session,"campaign", choices=c("Total", unique_campaigns))
      updateSelectInput(session,"outlet",   choices=c("Total", unique_outlets))
      updateSelectInput(session,"creative", choices=c("Total", unique_creatives))
      
      updateSelectInput(session,"product_multi",  choices=c("All/Total", unique_products))
      updateSelectInput(session,"campaign_multi", choices=c("Total", unique_campaigns))
      updateSelectInput(session,"outlet_multi",   choices=c("Total", unique_outlets))
      updateSelectInput(session,"creative_multi", choices=c("Total", unique_creatives))
    }
    
    observe({
      req(rv$data)
      filtered <- rv$data
      
      if(input$geography!="All/Total"){
        if("Geography" %in% names(filtered)){
          filtered <- filtered %>% filter(Geography==input$geography)
        } else if("geografia" %in% names(filtered)){
          filtered <- filtered %>% filter(geografia==input$geography)
        }
      }
      
      if(!is.null(rv$variable_mapping)){
        if(input$product!="All/Total"){
          filtered <- filtered %>% select(-names(rv$variable_mapping)[
            sapply(rv$variable_mapping, function(x)x$product!=input$product)
          ])
        }
        if(input$campaign!="Total"){
          filtered <- filtered %>% select(-names(rv$variable_mapping)[
            sapply(rv$variable_mapping, function(x)x$campaign!=input$campaign)
          ])
        }
        if(input$outlet!="Total"){
          filtered <- filtered %>% select(-names(rv$variable_mapping)[
            sapply(rv$variable_mapping, function(x)x$outlet!=input$outlet)
          ])
        }
        if(input$creative!="Total"){
          filtered <- filtered %>% select(-names(rv$variable_mapping)[
            sapply(rv$variable_mapping, function(x)x$creative!=input$creative)
          ])
        }
      }
      
      # Filtrar por rango univ
      date_col <- if("Period" %in% names(filtered)) "Period" else "periodo"
      if(date_col %in% names(filtered)){
        filtered[[date_col]] <- as.Date(filtered[[date_col]])
        filtered <- filtered %>%
          filter(
            !!sym(date_col)>=input$date_range_univ[1],
            !!sym(date_col)<=input$date_range_univ[2]
          )
      }
      
      rv$filtered_data <- filtered
    })
    
    # TAB: Information
    output$kpi_table <- renderTable({
      req(rv$filtered_data, input$kpi)
      rv$filtered_data %>%
        summarise(
          Mean = mean(.data[[input$kpi]], na.rm=TRUE),
          SD   = sd(.data[[input$kpi]], na.rm=TRUE),
          Min  = min(.data[[input$kpi]], na.rm=TRUE),
          Max  = max(.data[[input$kpi]], na.rm=TRUE)
        )
    })
    
    output$media_table <- renderTable({
      req(rv$filtered_data, input$media_vars)
      rv$filtered_data %>%
        summarise(across(all_of(input$media_vars),
                         list(mean=~mean(.,na.rm=TRUE), sd=~sd(.,na.rm=TRUE))))
    })
    
    output$activity_table <- renderTable({
      req(rv$filtered_data, input$media_vars)
      rv$filtered_data %>%
        summarise(across(all_of(input$media_vars),
                         ~mean(.>0, na.rm=TRUE)*100)) %>%
        tidyr::gather(key="Variable", value="Activity %")
    })
    
    output$spend_table <- renderTable({
      req(rv$filtered_data, input$spend_vars)
      rv$filtered_data %>%
        summarise(across(all_of(input$spend_vars),
                         ~sum(as.numeric(.),na.rm=TRUE)/
                           sum(as.numeric(rv$filtered_data[[input$spend_vars[1]]]),na.rm=TRUE)*100)) %>%
        tidyr::gather(key="Variable", value="Spend %")
    })
    
    output$activity_percentage_table <- renderTable({
      req(rv$filtered_data, input$media_vars)
      total_activity <- sum(rv$filtered_data[input$media_vars],na.rm=TRUE)
      rv$filtered_data %>%
        summarise(across(all_of(input$media_vars),
                         ~ (sum(.,na.rm=TRUE)/total_activity)*100)) %>%
        tidyr::gather(key="Variable", value="Activity_Percentage") %>%
        mutate(Activity_Percentage=round(Activity_Percentage,2))%>%
        select(Variable, Activity_Percentage)
    })
    
    output$spend_percentage_table <- renderTable({
      req(rv$filtered_data, input$spend_vars)
      total_spend <- sum(rv$filtered_data[input$spend_vars],na.rm=TRUE)
      rv$filtered_data %>%
        summarise(across(all_of(input$spend_vars),
                         ~ (sum(.,na.rm=TRUE)/total_spend)*100)) %>%
        tidyr::gather(key="Variable", value="Spend_Percentage") %>%
        mutate(Spend_Percentage=round(Spend_Percentage,2))%>%
        select(Variable, Spend_Percentage)
    })
    
    output$cpm_cpc <- renderTable({
      req(rv$filtered_data, input$media_vars, input$spend_vars)
      total_activity <- rv$filtered_data %>%
        summarise(across(all_of(input$media_vars), ~ sum(.,na.rm=TRUE))) %>%
        pivot_longer(cols=everything(), names_to="Variable", values_to="Activity")
      
      total_spend <- rv$filtered_data %>%
        summarise(across(all_of(input$spend_vars), ~ sum(.,na.rm=TRUE))) %>%
        pivot_longer(cols=everything(), names_to="Variable", values_to="Spend")
      
      combined_stats <- total_activity %>%
        mutate(Spend = total_spend$Spend) %>%
        mutate(CPM_CPC = ifelse(Activity>0, round((Spend/Activity)*1000,2), NA))
      
      combined_stats
    })
    
    output$download_cpm_cpc <- downloadHandler(
      filename = function() {
        paste("CPM_CPC_Table", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        req(rv$filtered_data, input$media_vars, input$spend_vars)
        
        total_activity <- rv$filtered_data %>%
          summarise(across(all_of(input$media_vars), ~ sum(.,na.rm=TRUE))) %>%
          pivot_longer(cols=everything(), names_to="Variable", values_to="Activity")
        
        total_spend <- rv$filtered_data %>%
          summarise(across(all_of(input$spend_vars), ~ sum(.,na.rm=TRUE))) %>%
          pivot_longer(cols=everything(), names_to="Variable", values_to="Spend")
        
        combined_stats <- total_activity %>%
          mutate(Spend=total_spend$Spend) %>%
          mutate(CPM_CPC=ifelse(Activity>0, round((Spend/Activity)*1000,2),NA))
        
        write.csv(combined_stats, file, row.names=FALSE)
      }
    )
  })  # Fin observeEvent(file)
  
  parse_variable_name <- function(var_name){
    parts <- str_split(var_name,"_")[[1]]
    if(length(parts)>5){
      return(list(
        variable_name = paste(parts[1:(length(parts)-4)], collapse="_"),
        product       = parts[length(parts)-3],
        campaign      = parts[length(parts)-2],
        outlet        = parts[length(parts)-1],
        creative      = parts[length(parts)]
      ))
    }
    return(NULL)
  }
  
  # apply_transformation para la pestaña “Transformed Variable”
  apply_transformation <- function(data,
                                   type,
                                   alpha=0.85,
                                   beta=1,
                                   maxval=100,
                                   decay=1,
                                   lag=0){
    if(is.character(data)){
      return(data)
    }
    data<-as.numeric(data)
    if(lag>0){
      data<-dplyr::lag(data,lag)
    }
    if(decay!=1){
      data<-data*decay
    }
    
    transformed<-switch(type,
      "Linear"    = data,
      "S Origin"  = s_curve_transform(data, shape="s-origin", alpha=alpha, beta=beta, maxValuePct=maxval),
      "S Shaped"  = s_curve_transform(data, shape="s-shaped", alpha=alpha, beta=beta, maxValuePct=maxval),
      "Index Exp" = exp(data/100)*100,
      "Log"       = log1p(data),
      "Exp"       = exp(data),
      "Power"     = data^beta,
      "Moving Avg"= zoo::rollmean(data, k=3, fill=NA),
      data
    )
    transformed
  }
  
  observe({
    req(rv$data)
    numeric_cols<-names(rv$data)[sapply(rv$data, is.numeric)]
    
    MEDIA_VARIABLES<-grep("(Impressions)|(Circulation)|(Clicks)|(Display)|(OOH)|(OLV)|(Magazine)|(Newspaper)",
                          names(rv$data), value=TRUE)
    SPEND_VARIABLES<-grep("(Cost)|(Spend)", MEDIA_VARIABLES, value=TRUE)
    MEDIA_VARIABLES<-setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
    MEDIA_VARIABLES<-intersect(MEDIA_VARIABLES, numeric_cols)
    SPEND_VARIABLES<-intersect(SPEND_VARIABLES, numeric_cols)
    
    updateSelectInput(session,"kpi",        choices=numeric_cols)
    updateSelectInput(session,"media_vars", choices=MEDIA_VARIABLES)
    updateSelectInput(session,"spend_vars", choices=SPEND_VARIABLES)
    updateSelectInput(session,"base_vars",  choices=numeric_cols)
    
    if("Geography" %in% names(rv$data)){
      updateSelectInput(session,"geography",       choices=c("All/Total",unique(rv$data$Geography)))
      updateSelectInput(session,"geography_multi", choices=c("All/Total",unique(rv$data$Geography)))
    } else if("geografia" %in% names(rv$data)){
      updateSelectInput(session,"geography",       choices=c("All/Total",unique(rv$data$geografia)))
      updateSelectInput(session,"geography_multi", choices=c("All/Total",unique(rv$data$geografia)))
    }
    
    updateSelectInput(session,"kpi_univ",      choices=numeric_cols)
    updateSelectInput(session,"variable_univ", choices=numeric_cols)
    
    updateSelectInput(session,"kpi_multi",   choices=numeric_cols)
    updateSelectInput(session,"var1_multi",  choices=numeric_cols)
    updateSelectInput(session,"var2_multi",  choices=numeric_cols)
    updateSelectInput(session,"var3_multi",  choices=numeric_cols)
    updateSelectInput(session,"var4_multi",  choices=c("None", numeric_cols))
  })
  
  # TAB: Univariate
  output$kpi_vs_variable_plot <- renderPlotly({
    req(rv$filtered_data, input$kpi_univ, input$variable_univ)
    plotly::plot_ly() %>%
      plotly::layout(title="KPI vs Variable (Demo)")
  })
  
  output$variable_flighting_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    data_chart <- rv$filtered_data
    
    date_col <- if("Period" %in% names(data_chart)) "Period" else "periodo"
    plotly::plot_ly(data_chart, x=~get(date_col)) %>%
      plotly::add_lines(y=~get(input$variable_univ), name="Variable Flighting", line=list(color="blue")) %>%
      plotly::layout(
        title="Variable Flighting Over Time",
        xaxis=list(title="Time"),
        yaxis=list(title="Value"),
        legend=list(orientation="h", x=0.1, y=-0.2)
      )
  })
  
  output$var_transf_chart <- renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    data_chart<-rv$filtered_data
    
    data_chart$transformed <- apply_transformation(
      data_chart[[input$variable_univ]],
      input$transformation_univ,
      input$alpha_univ,
      input$beta_univ,
      input$maxval_univ,
      input$decay_univ,
      input$lag_univ
    )
    
    date_col<-if("Period"%in%names(data_chart)) "Period" else "periodo"
    
    plotly::plot_ly(data_chart, x=~get(date_col)) %>%
      plotly::add_lines(y=~transformed, name="Transformed Variable", line=list(color="red")) %>%
      plotly::layout(
        title="Transformed Variable Over Time",
        xaxis=list(title="Time"),
        yaxis=list(title="Transformed Value"),
        legend=list(orientation="h", x=0.1, y=-0.2)
      )
  })
  
  # TAB: Multivariate
  output$variables_chart_multi<-renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi)
    data_chart<-rv$filtered_data
    date_col<-if("Period"%in%names(data_chart)) "Period" else "periodo"
    
    plot_data<-data_chart %>%
      select(!!sym(date_col), input$var1_multi, input$var2_multi, input$var3_multi) %>%
      tidyr::gather(key="variable", value="value", -!!sym(date_col))
    
    if(input$sum_all_vars=="true"){
      for(var in unique(plot_data$variable)){
        trans_type<-if(var==input$var1_multi) {
          input$trans_var1
        } else if(var==input$var2_multi){
          input$trans_var2
        } else if(var==input$var3_multi){
          input$trans_var3
        } else if(var==input$var4_multi){
          input$trans_var4
        } else {
          "Linear"
        }
        
        plot_data$value[plot_data$variable==var]<-apply_transformation(
          plot_data$value[plot_data$variable==var],
          trans_type,
          input$alpha_multi,
          input$beta_multi,
          input$maxval_multi,
          input$decay_multi,
          input$lag_multi
        )
      }
    }
    
    ggplot(plot_data, aes(x=.data[[date_col]], y=value, color=variable))+
      geom_line()+
      theme_minimal()+
      labs(title="Multiple Variables over Time",
           x="Period",
           y="Value")+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$boxplot_multi<-renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi)
    data_chart<-rv$filtered_data
    
    plot_data<-data_chart %>%
      select(input$var1_multi, input$var2_multi, input$var3_multi) %>%
      tidyr::gather(key="variable", value="value")
    
    ggplot(plot_data, aes(x=variable, y=as.numeric(value)))+
      geom_boxplot()+
      theme_minimal()+
      labs(title="Distribution of Variables", x="Variable", y="Value")+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$corr_matrix_multi<-renderPlot({
    req(rv$filtered_data,input$var1_multi, input$var2_multi)
    vars_to_correlate<-c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(!is.null(input$var4_multi)&& input$var4_multi!="None"){
      vars_to_correlate<-c(vars_to_correlate,input$var4_multi)
    }
    cor_data<-as.data.frame(lapply(rv$filtered_data[vars_to_correlate],as.numeric))
    cor_matrix<-cor(cor_data,use="pairwise.complete.obs")
    
    corrplot(cor_matrix, method="color",
             type="upper",
             addCoef.col="black",
             tl.col="black",
             tl.srt=45,
             diag=FALSE,
             number.cex=0.7)
  })
  
  output$download_analytical<-downloadHandler(
    filename=function(){ paste("analytical_transformed_", format(Sys.Date(), "%Y%m%d"), ".csv",sep="") },
    content=function(file){
      download_data<-rv$filtered_data
      
      if(!is.null(input$variable_univ)){
        newcol<-paste0(input$variable_univ,"_transformed")
        download_data[[newcol]]<-apply_transformation(
          rv$filtered_data[[input$variable_univ]],
          input$transformation_univ,
          input$alpha_univ,
          input$beta_univ,
          input$maxval_univ,
          input$decay_univ,
          input$lag_univ
        )
      }
      
      if(input$sum_all_vars=="true"){
        vars_to_transform<-c(input$var1_multi,input$var2_multi,input$var3_multi)
        if(!is.null(input$var4_multi)&& input$var4_multi!="None"){
          vars_to_transform<-c(vars_to_transform,input$var4_multi)
        }
        for(var in vars_to_transform){
          trans_type<-if(var==input$var1_multi){
            input$trans_var1
          } else if(var==input$var2_multi){
            input$trans_var2
          } else if(var==input$var3_multi){
            input$trans_var3
          } else if(var==input$var4_multi){
            input$trans_var4
          } else {
            "Linear"
          }
          newcol<-paste0(var,"_transformed")
          download_data[[newcol]]<-apply_transformation(
            rv$filtered_data[[var]],
            trans_type,
            input$alpha_multi,
            input$beta_multi,
            input$maxval_multi,
            input$decay_multi,
            input$lag_multi
          )
        }
      }
      write.csv(download_data, file, row.names=FALSE, na="")
    }
  )

  # =========================
  # PESTAÑA: S-Curve EDA
  # =========================
  output$s_curve_eda_plot<-renderPlotly({
    req(rv$filtered_data, input$variable_univ)
    
    var_name    <- input$variable_univ
    alpha       <- input$alpha_univ
    beta        <- input$beta_univ
    max_val_pct <- input$maxval_univ
    decay       <- input$decay_univ
    lag         <- input$lag_univ

    # Construimos un df con “Period” y “value”
    df_scurve <- rv$filtered_data %>%
      mutate(Period = if("Period"%in%names(.)) as.Date(Period) else if("periodo"%in%names(.)) as.Date(periodo) else as.Date(NA)) %>%
      select(Period, value=!!sym(var_name)) %>%
      filter(!is.na(Period))

    # “flighting_plot_ggplotly”:
    flighting_plot_gg <- create_flighting_chart(
      data_chart = df_scurve,
      alpha      = alpha,
      beta       = beta,
      max_val_pct= max_val_pct,
      decay      = decay,
      lag        = lag,
      var_name   = var_name
    )
    
    # “s_curve_plot_ggplotly”:
    s_curve_plot_gg <- create_s_curve_chart(
      data_chart = df_scurve,
      alpha      = alpha,
      beta       = beta,
      max_val_pct= max_val_pct,
      decay      = decay,
      lag        = lag,
      var_name   = var_name
    )
    
    # Combinamos con subplot
    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows=1, titleX=TRUE, titleY=TRUE)
  })
  
}