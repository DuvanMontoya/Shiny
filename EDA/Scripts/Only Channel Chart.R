# EDA/Scripts/Only Channel Chart.R

only_channel_chart <- ggplot(var_activity) +
  aes(x = Period, y = str_to_MemoryVar(VariableName_1, accent = TRUE)) +
  geom_line(color = "darkred", linewidth = 1) +
  scale_x_date(breaks = seq(start_period, end_period, by = "quarter")) +
  scale_y_continuous(
    labels   = scales::unit_format(unit = "MM", scale = 1e-6, prefix = "", big.mark = ","),
    n.breaks = 10
  ) +
  labs(title = "Flighting",
       y     = str_split(VariableName_1, "_")[[1]][1],
       color = "")
