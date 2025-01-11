# global.R

options(shiny.maxRequestSize = 30*1024^2)
options(future.globals.maxSize = 1000 * 1024^2)
options(DT.options = list(pageLength = 25))

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(zoo)
library(stringr)
library(DT)
library(plotly)

theme_set(theme_minimal())
Sys.setlocale("LC_TIME", "Spanish")
options(scipen = 999)

app_colors <- c(
  "primary"   = "#0072B2",
  "secondary" = "#E69F00",
  "tertiary"  = "#009E73",
  "warning"   = "#D55E00",
  "info"      = "#56B4E9"
)

options(lubridate.week.start = 1)

format_num <- function(x) {
  format(round(x, 2), big.mark=",", scientific=FALSE)
}

# En R >= 4.2, memory.limit() no es soportado en otras plataformas
# memory.limit(size = 8000)
