# EDA/EDA.R
pacman::p_load(
  tidyverse, plotly, scales, modeest
)

# setwd("C:/ruta/a/tu/proyecto")
source("EDA/Scripts/EDA Functions.R")
load("data/AnalyticalDataset.RData")

analytical <- AnalyticalDataset
rm("AnalyticalDataset")

KPI        <- c("Level" = "Gross Sales", "Logaritm" = "LN-Gross Sales")
Geography  <- "All"
Product    <- "Leather Goods"
Campaign   <- "Total"
Outlet     <- "Total"
Creative   <- "Total"

start_period <- as.Date("2021-04-01")
end_period   <- as.Date("2024-03-31")

source("EDA/Scripts/Data Selection.R")
source("EDA/Scripts/Channel vs KPI Chart.R")
source("EDA/Scripts/Boxplot.R")
source("EDA/Scripts/Suggested MaxValPerc.R")
source("EDA/Scripts/S-Curve Chart for EDA.R")
source("EDA/Scripts/Flighting Chart.R")
source("EDA/Scripts/Only Channel Chart.R")
# source("EDA/Scripts/S-Curve data to export.R")
source("EDA/Scripts/S-Curve Info Table.R")
