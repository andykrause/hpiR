#*****************************************************************************************
#
#   Unit tests for hpiR package - Setup Functions
#
#*****************************************************************************************

library(hpiR)
library(testthat)

## Load Data

data(seattle_sales)
sales <- seattle_sales

# ### Test all plot functions --------------------------------------------------------------
#
#
# context('Plot functions')
#
#   test_that('plot.hpiindex works', {
#     expect_is(plot(modelToIndex(hpiModel(hpi_data = rs_df))), 'gg')
#     expect_error(plot(hpiModel(hpi_data = rs_df)))
#   })
#
#   test_that('plot.hpiindex works', {
#     expect_is(plot(rsIndex(sales_df = rs_df,
#                            estimator = 'weighted',
#                            log_dep = TRUE)), 'gg')
#   })
#
# context('Plot Functions')
