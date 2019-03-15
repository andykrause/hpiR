#***************************************************************************************************
#
#   Simple Comparison Script for ARES paper
#
#***************************************************************************************************

### Preliminary Commands ---------------------------------------------------------------------------

## Load Libraries

library(hpiR)
library(tidyverse)

## Load Data

data(seattle_sales)
data(ex_sales)

## Load Custom Functions

source(file.path(getwd(), 'papers', 'ares_script_functions.r'))

## Errors by time (check oversmoothing....are errors bigger in periods with larger increases)

### Random Forest Hyperparmater tests --------------------------------------------------------------
#
#   ## Small Area
# exgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                       .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                       .f = testRfHpi,
#                       ex_sales = ex_sales)
#
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
#  # All Seattle
#   allgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                         .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                         .f = testRfHpi,
#                         ex_sales = seattle_sales)
#
#   # Area X
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
#   x <- 43
#   xgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == x))
#   # Area Y
#   y <- 77
#   ygrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == y))
#
#   # Area Z
#   z <- 22
#   zgrid <- purrr::map2(.x = rep(c(10, 50, 100, 200), 4),
#                          .y = c(rep(10, 4), rep(50, 4), rep(100, 4), rep(200, 4)),
#                          .f = testRfHpi,
#                          ex_sales = seattle_sales  %>% dplyr::filter(area == z))
#
#   save.image("C:/Users/andyx/Desktop/ares.RData")
#
# ###############
#
#
#
