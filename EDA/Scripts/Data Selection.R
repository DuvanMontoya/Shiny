# EDA/Scripts/Data Selection.R

KPI_analytical <- paste(KPI["Level"], Product, "TOTAL_Total_Total", sep = "_")

analytical <- analytical %>%
  filter(between(Period, start_period, end_period))

KPI_data <- analytical %>%
  filter(between(Period, start_period, end_period)) %>%
  group_by(Period) %>%
  summarise(KPI = sum(eval(parse(text = paste0('`', KPI_analytical, '`')))))

if (Geography == "All") {
  Geo_filter <- analytical %>%
    distinct(Geography) %>%
    pull(Geography)
} else {
  Geo_filter <- Geography
}

if (str_to_lower(geo_type) == "rag") {
  var_activity <- analytical %>%
    filter(Geography %in% Geo_filter[1]) %>%
    select(Period, all_of(VariableName_1)) %>%
    bind_cols(KPI_data %>% select(-Period))
} else if (str_to_lower(geo_type) == "no rag") {
  var_activity <- analytical %>%
    select(Period, Geography, all_of(VariableName_1)) %>%
    group_by(Period) %>%
    summarise(VariableName_1 = sum(str_to_MemoryVar(VariableName_1))) %>%
    bind_cols(KPI_data %>% select(-Period))
  
  colnames(var_activity) <- c("Period", VariableName_1, "KPI")
}
