# EDA/Scripts/Flighting Chart.R

var_activity_zeros <- var_activity %>%
  replace(. == 0, NA)

quantile_97 <- var_activity %>%
  filter(str_to_MemoryVar(VariableName_1, accent = TRUE) > 0) %>%
  pull(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
  quantile(0.97) %>%
  unname()

activity_mean_97 <- var_activity %>%
  filter(str_to_MemoryVar(VariableName_1, accent = TRUE) > 0,
         str_to_MemoryVar(VariableName_1, accent = TRUE) < quantile_97) %>%
  pull(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
  mean()

if (activity_mean_97 < 1000) {
  scale_level_number <- 1
  scale_level_suffix <- ""
} else if (between(activity_mean_97, 1000, 999999)) {
  scale_level_number <- 1000
  scale_level_suffix <- "K"
} else if (between(activity_mean_97, 1e6, 999999999)) {
  scale_level_number <- 1e6
  scale_level_suffix <- "MM"
} else if (between(activity_mean_97, 1e9, 999999999999)) {
  scale_level_number <- 1e9
  scale_level_suffix <- "B"
} else if (activity_mean_97 > 999999999999) {
  scale_level_number <- 1e12
  scale_level_suffix <- "MB"
}

only_channel_chart_2 <- ggplot(var_activity) +
  aes(x = Period, y = str_to_MemoryVar(VariableName_1, accent = TRUE)) +
  geom_hline(yintercept = subset(key_points, Key_point == "Breakthrough")$indexing,
             linewidth = 0.2, color = "black") +
  geom_hline(yintercept = subset(key_points, Key_point == "Optimal begins")$indexing,
             linewidth = 0.2, color = "black") +
  geom_hline(yintercept = subset(key_points, Key_point == "Saturation begins")$indexing,
             linewidth = 0.2, color = "black") +
  geom_hline(yintercept = subset(key_points, Key_point == "Full saturation")$indexing,
             linewidth = 0.2, color = "black") +
  annotate("rect",
           xmin = start_period,
           xmax = end_period,
           ymin = 0,
           ymax = subset(key_points, Key_point == "Breakthrough")$indexing,
           alpha = 0.3,
           fill  = "darkgray") +
  annotate("rect",
           xmin = start_period,
           xmax = end_period,
           ymin = subset(key_points, Key_point == "Optimal begins")$indexing,
           ymax = subset(key_points, Key_point == "Saturation begins")$indexing,
           alpha = 0.3,
           fill  = "blue") +
  annotate("rect",
           xmin = start_period,
           xmax = end_period,
           ymin = subset(key_points, Key_point == "Full saturation")$indexing,
           ymax = max_activity,
           alpha = 0.3,
           fill  = "darkgray") +
  geom_point(data = var_activity_zeros,
             aes(x = Period,
                 y = str_to_MemoryVar(VariableName_1, accent = TRUE)),
             color = "darkred") +
  geom_line(color = "black") +
  geom_hline(yintercept = subset(key_points, Key_point == "Max efficiency")$indexing,
             linetype = 2, color = "darkgoldenrod3") +
  geom_hline(yintercept = Avg_52w, 
             linetype = 2, color = "darkgreen") +
  scale_x_date(breaks = seq(start_period, end_period, by = "quarter")) +
  scale_y_continuous(
    labels   = scales::unit_format(unit = scale_level_suffix,
                                   scale = 1/scale_level_number,
                                   prefix = "",
                                   big.mark = ","),
    n.breaks = 10
  ) +
  labs(title = "Flighting",
       y     = str_split(VariableName_1, "_")[[1]][1],
       color = "",
       caption = paste0("Suggested MaxVal%: ",
                        round(suggested_max_val_perc * 100, 2), "%; ",
                        format(max_no_outlier, big.mark = ",", decimal.mark = "."))) +
  theme(
    plot.title    = element_text(size = 23, face = "bold", hjust = 0.5),
    axis.text.x   = element_text(size = 10, angle = 30),
    axis.text.y   = element_text(size = 10),
    axis.title    = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid    = element_line(colour = "#0A0A0A1C")
  )

only_channel_chart_ggplotly <- plotly::ggplotly(only_channel_chart_2)