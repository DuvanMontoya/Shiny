# EDA/Scripts/Boxplot.R

channel_boxplot <- ggplotly(
  var_activity %>%
    filter(str_to_MemoryVar(VariableName_1, accent = TRUE) > 0) %>%
    ggplot(aes(y = str_to_MemoryVar(VariableName_1, accent = TRUE))) +
    geom_boxplot(fill = "steelblue") +
    scale_y_continuous(
      labels   = unit_format(unit = "MM", scale = 1e-6, prefix = "", big.mark = ","),
      n.breaks = 10
    ) +
    labs(y = str_split(VariableName_1, "_")[[1]][1]) +
    theme(axis.text.x = element_blank())
)