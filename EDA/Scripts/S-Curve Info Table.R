# EDA/Scripts/S-Curve Info Table.R

s_curve_chart_info_df <- tibble::tribble(
  ~Metric,      ~Value,
  "Alpha",      alpha,
  "Beta",       beta,
  "MaxVal%",    max_val_pct,
  "MaxVal",     max_activity[1] * (max_val_pct / 100)
)
