# EDA/Scripts/S-Curve Chart for EDA.R

maxvalue <- max_val_pct / 100
carry_over <- 1 - decay

max_activity <- var_activity %>%
  slice_max(str_to_MemoryVar(VariableName_1, accent = TRUE)) %>%
  pull(str_to_MemoryVar(VariableName_1, accent = TRUE))

Max_value <- max_activity * maxvalue

i     <- 1
i_max <- 500
index <- seq(0, i_max, by = i)

data_s_curves <- data.frame(index = index) %>%
  mutate(indexing = (index * Max_value) / 100) %>%
  mutate(s_curve_index  = s_curve_indexing(serie = indexing, alpha = alpha, beta = beta)) %>%
  mutate(first_derivative  = first_derivative(x = s_curve_index, alpha = alpha, beta = beta)) %>%
  mutate(second_derivative = second_derivative(x = s_curve_index, alpha = alpha, beta = beta)) %>%
  mutate(third_derivative  = third_derivative(x = s_curve_index, alpha = alpha, beta = beta)) %>%
  mutate(ROI_max_efficiency = s_curve_index / indexing)

# Puntos clave
max_1st_deriv  <- which.max(data_s_curves$first_derivative)
max_2nd_deriv  <- which.max(data_s_curves$second_derivative)
min_2nd_deriv  <- which.min(data_s_curves$second_derivative)
max_ROI_eff    <- which.max(data_s_curves$ROI_max_efficiency)

s_curve_key_columns <- c("index", "s_curve_index", "indexing")

key_points <- rbind(
  Breakthrough       = data_s_curves[max_2nd_deriv,  s_curve_key_columns],
  `Optimal begins`   = data_s_curves[max_1st_deriv,  s_curve_key_columns],
  `Saturation begins`= data_s_curves[min_2nd_deriv,  s_curve_key_columns],
  `Full saturation`  = subset(data_s_curves, s_curve_index >= 0.98)[1, s_curve_key_columns],
  `Max efficiency`   = data_s_curves[max_ROI_eff,    s_curve_key_columns]
) %>%
  mutate(Key_point = rownames(.), .after = "index")

rownames(key_points) <- NULL

# Cálculo del promedio 52 semanas (ejemplo)
Avg_52w <- var_activity %>%
  slice_tail(n = 52) %>%
  select(all_of(VariableName_1)) %>%
  filter(. > 1000) %>%
  pull(all_of(VariableName_1)) %>%
  mean()

# Se añaden datos a key_points si se desea
