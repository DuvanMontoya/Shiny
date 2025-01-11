# EDA/Scripts/S-Curve data to export.R

optimal_begins_indexing    <- subset(key_points, Key_point == "Optimal begins")$indexing
saturation_begins_indexing <- subset(key_points, Key_point == "Saturation begins")$indexing

s_curve_data_report <- dplyr::left_join(
  data_s_curves,
  key_points,
  by = c("indexing" = "indexing"),
  suffix = c("", "_key")
) %>%
  dplyr::select(indexing, s_curve_index, s_curve_index_key, Key_point) %>%
  dplyr::rename(points = s_curve_index_key) %>%
  dplyr::mutate(
    Optimal_rage = ifelse(
      dplyr::between(indexing, optimal_begins_indexing, saturation_begins_indexing), 1, NA
    ),
    .before = "Key_point"
  )

# Ejemplo de export a Excel (necesita openxlsx u otro paquete):
# openxlsx::write.xlsx(
#   s_curve_data_report,
#   paste0("EDA/EDA by channel/", VariableName_1, "S-Curve.xlsx"),
#   overwrite = TRUE
# )