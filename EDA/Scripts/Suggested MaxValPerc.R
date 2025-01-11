# EDA/Scripts/Suggested MaxValPerc.R

car_boxplot <- var_activity %>%
  filter(str_to_MemoryVar(VariableName_1, accent = TRUE) > 1000) %>%
  pull(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
  car::Boxplot(id.method = "y")

if (length(car_boxplot) == 0) {
  suggested_max_val_perc <- 1
} else {
  outliers_from_boxplot <- min(car_boxplot)
  
  min_outlier <- var_activity %>%
    filter(str_to_MemoryVar(VariableName_1, accent = TRUE) > 1000) %>%
    slice(outliers_from_boxplot) %>%
    pull(str_to_MemoryVar(VariableName_1, accent = TRUE))
  
  max_no_outlier <- var_activity %>%
    filter(str_to_MemoryVar(VariableName_1, accent = TRUE) < min_outlier) %>%
    slice_max(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
    pull(str_to_MemoryVar(VariableName_1, accent = TRUE))
  
  max_activity <- var_activity %>%
    slice_max(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
    pull(str_to_MemoryVar(VariableName_1, accent = TRUE))
  
  suggested_max_val_perc <- max_no_outlier[1] / max_activity
}