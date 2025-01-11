# EDA/Scripts/S-Curve EDA Combination.R

library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(zoo)
library(modeest)

# create_flighting_chart
create_flighting_chart <- function(
  data_chart,
  alpha,
  beta,
  max_val_pct,
  decay=1,
  lag=0,
  var_name="Variable"
) {
  # data_chart: df con col “Period” y “value”
  var_activity <- data_chart
  
  # Reemplazar ceros por NA (pintarlos en rojo)
  var_activity_zeros <- var_activity %>% mutate(value = ifelse(value == 0, NA, value))
  
  # Calcular quantile 0.97
  quantile_97 <- var_activity %>%
    filter(value > 0) %>%
    pull(value) %>%
    quantile(0.97, na.rm=TRUE) %>%
    unname()
  
  # mean < quantile_97
  activity_mean_97 <- var_activity %>%
    filter(value > 0, value < quantile_97) %>%
    pull(value) %>%
    mean(na.rm=TRUE)
  
  # Determinar escala
  if(is.na(activity_mean_97)) activity_mean_97 <- 1
  
  if(activity_mean_97 < 1000){
    scale_level_number <- 1
    scale_level_suffix <- ""
  } else if(dplyr::between(activity_mean_97, 1000, 999999)){
    scale_level_number <- 1000
    scale_level_suffix <- "K"
  } else if(dplyr::between(activity_mean_97, 1e6, 999999999)){
    scale_level_number <- 1e6
    scale_level_suffix <- "MM"
  } else if(dplyr::between(activity_mean_97, 1e9, 999999999999)){
    scale_level_number <- 1e9
    scale_level_suffix <- "B"
  } else {
    scale_level_number <- 1e12
    scale_level_suffix <- "MB"
  }
  
  # Ejemplo dinámico de key_points basado en percentiles
  key_points <- data.frame(
    Key_point = c("Breakthrough","Optimal begins","Saturation begins","Full saturation","Max efficiency"),
    indexing  = quantile(var_activity$value, probs = c(0.2, 0.4, 0.6, 0.8, 1.0), na.rm=TRUE)
  )
  
  max_activity <- max(var_activity$value, na.rm=TRUE)
  
  # 52w_Avg
  n_52 <- min(52, nrow(var_activity))
  last_52 <- tail(var_activity, n_52) %>% filter(value > 1000)
  Avg_52w <- mean(last_52$value, na.rm=TRUE)
  if(is.na(Avg_52w)) Avg_52w <- 0
  
  # Verificar que key_points tienen los puntos necesarios
  required_points <- c("Breakthrough", "Optimal begins", "Saturation begins", "Full saturation")
  key_points_filtered <- key_points %>% filter(Key_point %in% required_points)
  
  if(nrow(key_points_filtered) < length(required_points)){
    stop("Faltan algunos puntos clave en key_points para Flighting Chart.")
  }
  
  # Construir el ggplot
  p <- ggplot(var_activity, aes(x=Period, y=value)) +
    # Solo agregar líneas horizontales si los puntos clave existen
    geom_hline(data=key_points_filtered, aes(yintercept=indexing), color="black", linewidth=0.2) +
    
    # Bandas
    annotate("rect",
             xmin=min(var_activity$Period, na.rm=TRUE),
             xmax=max(var_activity$Period, na.rm=TRUE),
             ymin=0,
             ymax=subset(key_points_filtered, Key_point=="Breakthrough")$indexing,
             alpha=0.2, fill="gray") +
    annotate("rect",
             xmin=min(var_activity$Period, na.rm=TRUE),
             xmax=max(var_activity$Period, na.rm=TRUE),
             ymin=subset(key_points_filtered, Key_point=="Optimal begins")$indexing,
             ymax=subset(key_points_filtered, Key_point=="Saturation begins")$indexing,
             alpha=0.2, fill="blue") +
    annotate("rect",
             xmin=min(var_activity$Period, na.rm=TRUE),
             xmax=max(var_activity$Period, na.rm=TRUE),
             ymin=subset(key_points_filtered, Key_point=="Full saturation")$indexing,
             ymax=max_activity,
             alpha=0.2, fill="gray") +
    
    # Puntos en rojo cuando sea 0
    geom_point(data=var_activity_zeros, aes(x=Period, y=value), color="red") +
    geom_line(color="black") +
    
    # Max efficiency
    geom_hline(data=subset(key_points, Key_point=="Max efficiency"),
               aes(yintercept=indexing),
               linetype=2, color="darkgoldenrod3") +
    # 52w average
    geom_hline(yintercept=Avg_52w, linetype=2, color="darkgreen") +
    
    scale_x_date(date_breaks="3 months", date_labels="%Y-%m") +
    scale_y_continuous(labels=scales::unit_format(
      unit=scale_level_suffix,
      scale=1/scale_level_number,
      big.mark=","),
      n.breaks=10
    ) +
    labs(title="Flighting",
         x="Period",
         y=var_name) +
    theme_minimal()
  
  # Manejar caso donde max_activity es -Inf o Inf
  if(is.infinite(max_activity)){
    p <- p + coord_cartesian(ylim = c(0, max(var_activity$value, na.rm=TRUE)))
  }
  
  ggplotly(p)
}

# create_s_curve_chart
create_s_curve_chart <- function(
  data_chart,
  alpha,
  beta,
  max_val_pct,
  decay=1,
  lag=0,
  var_name="Variable"
) {
  max_activity <- max(data_chart$value,na.rm=TRUE)
  if(is.infinite(max_activity) || max_activity == 0){
    max_activity <- 1
  }
  
  i_max <- 500
  index <- seq(0,i_max,by=1)
  Max_value <- max_activity*(max_val_pct/100)
  
  data_s_curves <- data.frame(index=index) %>%
    mutate(indexing = (index*Max_value)/100) %>%
    mutate(s_curve_index   = s_curve_indexing(serie=indexing, alpha=alpha, beta=beta)) %>%
    mutate(first_deriv     = first_derivative(x=s_curve_index, shape="s-origin", alpha=alpha, beta=beta)) %>%
    mutate(second_deriv    = second_derivative(x=s_curve_index,shape="s-origin", alpha=alpha, beta=beta)) %>%
    mutate(third_deriv     = third_derivative(x=s_curve_index, shape="s-origin", alpha=alpha, beta=beta)) %>%
    mutate(ROI_max_eff     = s_curve_index / indexing)
  
  # key_points
  max_1st_deriv <- which.max(data_s_curves$first_deriv)
  max_2nd_deriv <- which.max(data_s_curves$second_deriv)
  min_2nd_deriv <- which.min(data_s_curves$second_deriv)
  max_ROI_eff   <- which.max(data_s_curves$ROI_max_eff)
  
  s_curve_key_cols <- c("index","s_curve_index","indexing")
  key_points <- rbind(
    Breakthrough       = data_s_curves[max_2nd_deriv, s_curve_key_cols],
    `Optimal begins`   = data_s_curves[max_1st_deriv, s_curve_key_cols],
    `Saturation begins`= data_s_curves[min_2nd_deriv, s_curve_key_cols],
    `Full saturation`  = subset(data_s_curves, s_curve_index>=0.98)[1, s_curve_key_cols],
    `Max efficiency`   = data_s_curves[max_ROI_eff, s_curve_key_cols]
  ) %>%
    as.data.frame()
  key_points$Key_point <- rownames(key_points)
  rownames(key_points) <- NULL
  
  # 52w_Avg real
  n_52 <- min(52, nrow(data_chart))
  last_52 <- tail(data_chart, n_52) %>% filter(value > 1000)
  Avg_52w <- mean(last_52$value, na.rm=TRUE)
  if(is.na(Avg_52w)) Avg_52w <- 0
  
  # Escala
  quantile_97 <- data_chart %>% filter(value>0) %>% pull(value) %>% quantile(0.97, na.rm=TRUE)
  activity_mean_97 <- data_chart %>% filter(value>0, value<quantile_97) %>% pull(value) %>% mean(na.rm=TRUE)
  
  if(is.na(activity_mean_97)) activity_mean_97 <- 1
  
  if(activity_mean_97 < 1000){
    scale_level_number <- 1;   scale_level_suffix <- ""
  } else if(dplyr::between(activity_mean_97, 1000, 999999)){
    scale_level_number <- 1000;   scale_level_suffix <- "K"
  } else if(dplyr::between(activity_mean_97, 1e6, 999999999)){
    scale_level_number <- 1e6;    scale_level_suffix <- "MM"
  } else if(dplyr::between(activity_mean_97, 1e9, 999999999999)){
    scale_level_number <- 1e9;    scale_level_suffix <- "B"
  } else {
    scale_level_number <- 1e12;   scale_level_suffix <- "MB"
  }
  
  # Filtrar data_s_curves para indexing <= max_activity
  data_s_curves_plot <- data_s_curves %>% filter(indexing <= max_activity)
  
  # Verificar que key_points tienen los puntos necesarios
  required_points <- c("Breakthrough", "Optimal begins", "Saturation begins", "Full saturation")
  key_points_filtered <- key_points %>% filter(Key_point %in% required_points)
  
  if(nrow(key_points_filtered) < length(required_points)){
    stop("Faltan algunos puntos clave en key_points para S-Curve Chart.")
  }
  
  gg_s_curve <- ggplot(data_s_curves_plot, aes(x=indexing, y=s_curve_index)) +
    # Solo agregar rectángulos si key_points_filtered tiene los puntos
    annotate("rect",
             xmin=subset(key_points_filtered, Key_point=="Optimal begins")$indexing,
             xmax=subset(key_points_filtered, Key_point=="Saturation begins")$indexing,
             ymin=0, ymax=1,
             alpha=0.2, fill="blue") +
    geom_line() +
    geom_point(data=key_points, aes(x=indexing,y=s_curve_index, color=Key_point)) +
    scale_x_continuous(labels=scales::unit_format(unit=scale_level_suffix,
                                                  scale=1/scale_level_number,
                                                  big.mark=","),
                       n.breaks=10) +
    scale_y_continuous(labels=scales::percent_format(scale=100),
                       n.breaks=10) +
    labs(title=paste("S-Curve for", var_name),
         x="Activity", y="Index Value") +
    theme_minimal()
  
  s_curve_info <- paste("Alpha:", alpha,
                        "| Beta:", beta,
                        "| MaxVal%:", max_val_pct,
                        "| Real 52w_Avg =", round(Avg_52w,2))
  
  ggplotly_s_curve <- ggplotly(gg_s_curve) %>%
    layout(
      title = list(
        text = paste0("S-Curve for ", var_name, 
                      "<br><sup>", s_curve_info, "</sup>")
      ),
      legend = list(x=0.8, y=0.5)
    )
  
  ggplotly_s_curve
}