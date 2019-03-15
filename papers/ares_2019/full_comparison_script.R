#***************************************************************************************************
#
#   Analytical Script for ARES paper
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

### Create Data ------------------------------------------------------------------------------------

  # Hedonic Data
  hed_df <- hedCreateTrans(trans_df = seattle_sales,
                           prop_id = 'pinx',
                           trans_id = 'sale_id',
                           price = 'sale_price',
                           date = 'sale_date',
                           periodicity = 'monthly')

  # Repeat Transaction Data
  rt_df <- rtCreateTrans(trans_df = seattle_sales,
                         prop_id = 'pinx',
                         trans_id = 'sale_id',
                         price = 'sale_price',
                         date = 'sale_date',
                         periodicity = 'monthly')

### Comparisons ------------------------------------------------------------------------------------

 ## Full Dataset

 full_ <- threeWayComparison(data_obj = seattle_sales,
                             ntrees = 100,
                             sim_count = 100)

 full_comp <- summarizeComp(list(full_))

 saveRDS(full_comp$summ, file.path(getwd(), 'papers', 'ares_2019', 'full_summ.RDS'))
 saveRDS(full_comp, file.path(getwd(), 'papers', 'ares_2019', 'full_comp.RDS'))

## Small Geo Areas

  geo_df <- seattle_sales %>% dplyr::filter(!area %in% 23)

  geo_ <- suppressWarnings(
    purrr::map(.x = split(geo_df, geo_df$area),
                          .f = threeWayComparison,
                          hed_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age'),
                          lm_recover = TRUE))

  geo_comp <- summarizeComp(geo_)
  saveRDS(geo_comp$summ, file.path(getwd(), 'papers', 'ares_2019', 'geo_summ.RDS'))
  saveRDS(geo_comp, file.path(getwd(), 'papers', 'ares_2019', 'geo_comp.RDS'))


## Small Time Chunks

  time_data <- list(seattle_sales %>% dplyr::filter(sale_date <= '2011-12-31'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2012-12-31' &
                                                      sale_date >= '2011-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2013-12-31' &
                                                      sale_date >= '2012-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2014-12-31' &
                                                      sale_date >= '2013-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2015-12-31' &
                                                      sale_date >= '2014-01-01'),
                    seattle_sales %>% dplyr::filter(sale_date <= '2016-12-31' &
                                                      sale_date >= '2015-01-01'))

  time_ <- purrr::map(.x = time_data,
                      .f = threeWayComparison,
                      train_period = 12,
                      max_period = 24,
                      lm_recover = TRUE)

  time_summ <- summarizeComp(time_)
  saveRDS(time_summ$summ, file.path(getwd(), 'papers', 'ares_2019', 'time_summ.RDS'))
  saveRDS(time_summ, file.path(getwd(), 'papers', 'ares_2019', 'time_comp.RDS'))

##############

geo_comp <- readRDS(file.path(getwd(), 'papers', 'ares_2019', 'geo_comp.RDS'))
full_comp <- readRDS(file.path(getwd(), 'papers', 'ares_2019', 'full_comp.RDS'))
time_comp <- readRDS(file.path(getwd(), 'papers', 'ares_2019', 'time_comp.RDS'))


geo_comp$pr %>%
  dplyr::mutate(uniq_id = paste0(prop_id, '_', pred_period)) %>%
  dplyr::distinct(prop_id, pred_period, model, .keep_all = TRUE) %>%
  dplyr::select(-pred_price) %>%
  tidyr::spread(., key = 'model', value = 'pred_error')->x

  plyr::dlply(., pred_period)




