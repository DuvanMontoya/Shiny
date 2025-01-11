# EDA/Scripts/Channel vs KPI Chart.R

max_first  <- max(var_activity[, 2], na.rm = TRUE)
min_first  <- min(var_activity[, 2], na.rm = TRUE)
max_second <- max(KPI_data$KPI, na.rm = TRUE)
min_second <- 0

scale <- (max_second - min_second) / (max_first - min_first)
shift <- min_first - min_second

channel_kpi <- ggplotly(
  ggplot(var_activity) +
    aes(x = Period, y = str_to_MemoryVar(VariableName_1, accent = TRUE)) +
    geom_line(aes(y = inv_scale_function(KPI, scale, shift)), color = "steelblue") +
    geom_line(color = "darkred", linewidth = 1) +
    scale_x_date(breaks = seq(start_period, end_period, by = "quarter")) +
    scale_y_continuous(
      sec.axis = sec_axis(~ scale_function(., scale, shift), name = KPI["Level"]),
      labels   = unit_format(unit = "MM", scale = 1e-6, prefix = "", big.mark = ","),
      n.breaks = 10
    ) +
    labs(title = "Flighting",
         y     = str_split(VariableName_1, "_")[[1]][1],
         color = "")
)