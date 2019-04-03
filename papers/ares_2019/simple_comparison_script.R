#***************************************************************************************************
#
#   Simple Comparison Script for ARES paper
#
#***************************************************************************************************

### Preliminary Commands ---------------------------------------------------------------------------

 ## Load Libraries

  library(hpiR)
  library(tidyverse)
  library(mlr)
  library(shapleyR)

 ## Load Data

  data(seattle_sales)
  data(ex_sales)

 ## Load Custom Functions

  source(file.path(getwd(), 'papers', 'ares_2019', 'ares_script_functions.r'))

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

### Random Forest Example --------------------------------------------------------------------------

  # Estimate Model
  rf_model <- ranger::ranger(price ~ use_type + beds + baths + bldg_grade + tot_sf + latitude +
                               longitude + trans_period,
                             data = hed_df,
                             num.trees = 100,
                             seed = 1)

  # Set up example pdp
  pred_grid <- data.frame(trans_period = 1:max(hed_df$trans_period))

  # Example 1
  pdp_1 <- pdp::partial(rf_model,
                        train = hed_df[1, ],
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

  # Example 2
  pdp_2 <- pdp::partial(rf_model,
                        train = hed_df[2, ],
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

  # Example 3
  pdp_3 <- pdp::partial(rf_model,
                        train = hed_df[3, ],
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

  # Example 4
  pdp_4 <- pdp::partial(rf_model,
                        train = hed_df[44, ],
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

  # Example 5
  pdp_5 <- pdp::partial(rf_model,
                        train = hed_df[5, ],
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

  # Combine for plotting
  pdp_1to5 <- dplyr::bind_rows(list(pdp_1 %>% dplyr::mutate(example = 'Ex. 1'),
                                    pdp_2 %>% dplyr::mutate(example = 'Ex. 2'),
                                    pdp_3 %>% dplyr::mutate(example = 'Ex. 3'),
                                    pdp_4 %>% dplyr::mutate(example = 'Ex. 4'),
                                    pdp_5 %>% dplyr::mutate(example = 'Ex. 5')))

  # Averaged Examples 1 to 5
  pdp_1_5 <- pdp::partial(rf_model,
                          train = hed_df[c(1,2,3,44,5), ],
                          pred.var = "trans_period",
                          pred.grid = pred_grid) %>%
    dplyr::rename(period = trans_period,
                  value = yhat)

## Plotting

  gg_1 <- ggplot() +
    geom_line(data = pdp_1,
              aes(x = period, y = value), size = 1.5, color = 'indianred2') +
    scale_color_manual(name = 'Model', values = 'gray10') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5)) +
    ylab('Simulated Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    scale_y_continuous(breaks = c(4e5, 5e5, 6e5, 7e5),
                       labels = c('$400k', '$500k', '$600K', '$700K')) +
    ggtitle('Example Individual Conditional Expectations Plot\n
            (Observation 1, Feature = "Time")\n') +
    coord_cartesian(ylim = c(360000, 710000))

saveRDS(gg_1, file.path(getwd(), 'papers','ares_2019', 'ex1plot.RDS'))

reds <- RColorBrewer::brewer.pal(n = 9, name = "Reds")[3:7]
gg_1to5 <- ggplot() +
  geom_line(data = pdp_1to5,
            aes(x = period, y = value, group = example, color = example),
            size = 1.5) +
  scale_color_manual(name = 'Model', values = reds) +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Simulated Value\n') +
  xlab('\nTime') +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  scale_y_continuous(breaks = c(3e5, 4e5, 5e5, 6e5, 7e5, 8e5),
                     labels = c('$300K', '$400k', '$500k', '$600K', '$700K', '$800K'))+
  ggtitle('Example Individual Conditional Expectations Plot\n
            (Observations 1 to 5, Feature = "Time")\n') +
  coord_cartesian(ylim = c(260000, 850000))

saveRDS(gg_1to5, file.path(getwd(), 'papers', 'ares_2019', 'ex15plot.RDS'))

gg_1to5a <- ggplot() +
  geom_line(data = pdp_1_5,
            aes(x = period, y = value), size = 1.5, color = 'indianred2') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Simulated Value\n') +
  xlab('\nTime') +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  scale_y_continuous(breaks = c(3e5, 4e5, 5e5, 6e5, 7e5, 8e5),
                     labels = c('$300K', '$400k', '$500k', '$600K', '$700K', '$800K'))+
  ggtitle('Example Partial Dependence Plot (*Time*)\n
            (Observations 1 to 5, Feature = "Time")\n') +
  coord_cartesian(ylim = c(260000, 850000))

saveRDS(gg_1to5a, file.path(getwd(), 'papers','ares_2019', 'ex15aplot.RDS'))

## Add all
pdp_all <- pdp::partial(rf_model,
                        train = hed_df,
                        pred.var = "trans_period",
                        pred.grid = pred_grid) %>%
  dplyr::rename(period = trans_period,
                value = yhat) %>%
  dplyr::mutate(index = 100*(value/value[1]),
                from = 'Examples 1 to 5')

## Convert to index
rf_15 <- pdp_1_5 %>%
  dplyr::mutate(index = 100*(value/value[1]),
                from = 'Full sample')

rfa_df <- dplyr::bind_rows(list(pdp_all, rf_15))


gg_15i_plot <- ggplot() +
  geom_line(data = rfa_df,
            aes(x = period, y = index, color = from, size = from)) +
  scale_color_manual(name = 'Sample', values = c(2, 'gray20')) +
  scale_size_manual(name = '', values = c(2, 1), guide = 'none') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Index Value\n') +
  xlab('\nTime') +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  scale_y_continuous(breaks = seq(100, 150, 10)) +
  ggtitle('Example Random Forest-Derived HPI (Seattle)')

saveRDS(gg_15i_plot, file.path(getwd(), 'papers','ares_2019', 'ind15plot.RDS'))

### Shapley option ---------------------------------------------------------------------------------

# Estimate model
mod_df <-  hed_df[, c('beds', 'baths', 'bldg_grade', 'tot_sf', 'latitude', 'longitude',
                     'trans_period', 'price')]
mod_df$price <- log(mod_df$price)

regr.task = makeRegrTask(id = "aa", data = mod_df, target = "price")
regr.lrn = mlr::makeLearner("regr.ranger", par.vals = list(num.trees = 100))
rf_model = mlr::train(regr.lrn, regr.task)

# One
shap_df1 <- mod_df %>%
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  dplyr::group_by(trans_period) %>%
  dplyr::slice(1) %>%
  dplyr::arrange(row_id)

shapvalue_df1 <- shapleyR::getShapleyValues(
  shapley(shap_df1$row_id,
          task = regr.task,
          model = rf_model)) %>%
  dplyr::mutate(period = shap_df1$trans_period)

coef_df1 <- data.frame(time = 1:max(mod_df$trans_period)) %>%
  dplyr::left_join(shapvalue_df1  %>%
                     dplyr::group_by(period) %>%
                     dplyr::summarize(value = mean(trans_period)) %>%
                     dplyr::filter(period %in% mod_df$trans_period)%>%
                     dplyr::select(time = period,
                                   coefficient = value),
                   by = 'time') %>%
  dplyr::mutate(coefficient = coefficient - coefficient[1])

# Five
shap_df5 <- mod_df %>%
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  dplyr::group_by(trans_period) %>%
  dplyr::slice(1:5) %>%
  dplyr::arrange(row_id)

shapvalue_df5 <- shapleyR::getShapleyValues(
  shapley(shap_df5$row_id,
          task = regr.task,
          model = rf_model)) %>%
  dplyr::mutate(period = shap_df5$trans_period)

coef_df5 <- data.frame(time = 1:max(mod_df$trans_period)) %>%
  dplyr::left_join(shapvalue_df5   %>%
                     dplyr::group_by(period) %>%
                     dplyr::summarize(value = mean(trans_period)) %>%
                     dplyr::filter(period %in% mod_df$trans_period) %>%
                     dplyr::select(time = period,
                                   coefficient = value),
                   by = 'time') %>%
  dplyr::mutate(coefficient = coefficient - coefficient[1])

# Five
shap_df50 <- mod_df %>%
  dplyr::mutate(row_id = 1:nrow(.)) %>%
  dplyr::group_by(trans_period) %>%
  dplyr::slice(1:50) %>%
  dplyr::arrange(row_id)

shapvalue_df50 <- shapleyR::getShapleyValues(
  shapley(shap_df50$row_id,
          task = regr.task,
          model = rf_model)) %>%
  dplyr::mutate(period = shap_df50$trans_period)

coef_df50 <- data.frame(time = 1:max(mod_df$trans_period)) %>%
  dplyr::left_join(shapvalue_df50  %>%
                     dplyr::group_by(period) %>%
                     dplyr::summarize(value = mean(trans_period)) %>%
                     dplyr::filter(period %in% mod_df$trans_period)%>%
                     dplyr::select(time = period,
                                   coefficient = value),
                   by = 'time') %>%
  dplyr::mutate(coefficient = coefficient - coefficient[1])


ggplot() +
  geom_point(data = shapvalue_df1, aes(x=period, y = trans_period), size = .3, color='gray50') +
  geom_line(data = coef_df1 %>%
              dplyr::mutate(coefficient = coefficient +
                              mean(shapvalue_df1$trans_period[shapvalue_df1$period == 1])),
            aes(x=time, y = coefficient), color = 'red', size = 1) +
  ylab('Shapley Value') + xlab('Time Period') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Example Shapley Value Plot\n (1 Observation per Period, Feature = "Time")\n') ->gg_shap1

 ggplot() +
   geom_point(data = shapvalue_df5, aes(x=period, y = trans_period), size = .3, color='gray50') +
   geom_line(data = coef_df5 %>%
               dplyr::mutate(coefficient = coefficient +
                               mean(shapvalue_df5$trans_period[shapvalue_df5$period == 1])),
             aes(x=time, y = coefficient), color = 'red', size = 1) +
   ylab('Shapley Value') + xlab('Time Period') +
   theme(legend.position = 'bottom',
         plot.title = element_text(hjust = 0.5)) +
   scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
   ggtitle('Example Shapley Value Plot\n (5 Observations per Period, Feature = "Time")\n')->gg_shap5

 ggplot() +
   geom_point(data = shapvalue_df50, aes(x=period, y = trans_period), size = .3, color='gray50') +
   geom_line(data = coef_df50 %>%
               dplyr::mutate(coefficient = coefficient +
                               mean(shapvalue_df50$trans_period[shapvalue_df50$period == 1])),
             aes(x=time, y = coefficient), color = 'red', size = 1) +
   ylab('Shapley Value') + xlab('Time Period') +
   theme(legend.position = 'bottom',
         plot.title = element_text(hjust = 0.5)) +
   scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
   ggtitle('Example Shapley Value Plot\n (50 Observations per Period, Feature = "Time")\n')->gg_shap50


 rfs_50 <- coef_df50 %>%
   dplyr::mutate(index = 100*(coefficient+1))
 ggplot() +
   geom_line(data = rfs_50,
             aes(x=time, y = index), color = 'red', size = 1) +
   ylab('Shapley Value') + xlab('Time Period') +
   theme(legend.position = 'bottom',
         plot.title = element_text(hjust = 0.5)) +
   scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
   ggtitle('Example Shapley Value Derived HPID\n (50 Observations per Period)\n')->gg_hpishap


 saveRDS(gg_shap1, file.path(getwd(), 'papers','ares_2019', 'shap1plot.RDS'))
 saveRDS(gg_shap5, file.path(getwd(), 'papers','ares_2019', 'shap5plot.RDS'))
 saveRDS(gg_shap50, file.path(getwd(), 'papers','ares_2019', 'shap50plot.RDS'))
 saveRDS(gg_hpishap, file.path(getwd(), 'papers','ares_2019', 'shapindex.RDS'))


### Compare to RT/HED ------------------------------------------------------------------------------

rt_hpi <- rtIndex(trans_df = rt_df,
                  estimator = 'robust',
                  log_dep = TRUE,
                  trim_model = FALSE,
                  max_period = 84,
                  smooth = TRUE)
he_hpi <- hedIndex(trans_df = hed_df,
                   estimator = 'robust',
                   log_dep = TRUE,
                   dep_var = 'price',
                   ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area'),
                   trim_model = FALSE,
                   max_period = 84,
                   smooth = TRUE)

rf_hpi <- rfIndex(trans_df = hed_df,
                  estimator = 'pdp',
                  dep_var = 'price',
                  ind_var = c('use_type', 'lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area',
                              'latitude', 'longitude'),
                  max_period = 84,
                  smooth = FALSE,
                  ntrees = 100)


rfs_hpi <- rfIndex(trans_df = hed_df,
                  estimator = 'shap',
                  dep_var = 'price',
                  ind_var = c('lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area',
                              'latitude', 'longitude'),
                  max_period = 84,
                  smooth = FALSE,
                  shap_k = 50,
                  ntrees = 100)


rti_df <- data.frame(period = rt_hpi$index$period,
                     value = as.numeric(rt_hpi$index$value),
                     model = 'Repeat Sales')
rts_df <- data.frame(period = rt_hpi$index$period,
                     value = as.numeric(rt_hpi$index$smooth),
                     model = 'Repeat Sales - Smooth')
hei_df <- data.frame(period = he_hpi$index$period,
                     value = as.numeric(he_hpi$index$value),
                     model = 'Hedonic')
hes_df <- data.frame(period = he_hpi$index$period,
                     value = as.numeric(he_hpi$index$smooth),
                     model = 'Hedonic - Smooth')
rfi_df <- data.frame(period = rf_hpi$index$period,
                     value = as.numeric(rf_hpi$index$value),
                     model = 'Random Forest - PDP')
rsi_df <- data.frame(period = rfs_hpi$index$period,
                     value = as.numeric(rfs_hpi$index$value),
                     model = 'Random Forest - Shapley')


rhr_df <- rbind(rti_df, hei_df, rfi_df, rsi_df)
rhs_df <- rbind(rts_df, hes_df, rfi_df, rsi_df)

gg_rhr <- ggplot() +
  geom_line(data = rhr_df,
            aes(x = period, y = value, group = model, color = model, size = model)) +
  scale_color_manual(name = 'Model', values = c('gray50', 'red' ,'orange', 'black')) +
  scale_size_manual(values = c(1.5, 2, 2, 1.5), guide = 'none' )+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Index Value\n') +
  xlab('') +
  scale_y_continuous(breaks = seq(90, 170, 10)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Comparison of HPIs\n')+
  coord_cartesian(ylim = c(88, 172))


gg_rhs <- ggplot() +
  geom_line(data = rhs_df,
            aes(x = period, y = value, group = model, color = model, size = model)) +
  scale_color_manual(name = 'Model', values = c('gray50', 'red', 'black')) +
  scale_size_manual(values = c(1.5, 1.5, 2), guide = 'none' )+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Index Value\n') +
  xlab('') +
  scale_y_continuous(breaks = seq(90, 170, 10)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Comparison of HPIs (Smoothed)\n')+
  coord_cartesian(ylim = c(88, 172))

saveRDS(gg_rhr, file.path(getwd(), 'papers', 'ares_2019', 'rhrplot.RDS'))
saveRDS(gg_rhs, file.path(getwd(), 'papers', 'ares_2019', 'rhsplot.RDS'))

#***************************************************************************************************
#***************************************************************************************************
#
#  ## By Bed
#
# pdp_1bed <- pdp::partial(rf_model,
#                          train = hed_df[hed_df$beds < 2, ],
#                          pred.var = "trans_period",
#                          pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'ZOne Bedrooms')
#
# pdp_2bed <- pdp::partial(rf_model,
#                         train = hed_df[hed_df$beds == 2, ],
#                         pred.var = "trans_period",
#                         pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'Two Bedrooms')
# pdp_3bed <- pdp::partial(rf_model,
#                          train = hed_df[hed_df$beds == 3, ],
#                          pred.var = "trans_period",
#                          pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'Three Bedrooms')
# pdp_4bed <- pdp::partial(rf_model,
#                          train = hed_df[hed_df$beds >= 4, ],
#                          pred.var = "trans_period",
#                          pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'Four Bedrooms+')
#
# pdp_bed <- dplyr::bind_rows(list(pdp_1bed, pdp_2bed, pdp_3bed, pdp_4bed))
# ggplot(pdp_bed, aes(x=period,y=index, group=from, color=from))+geom_line()
#
#
# pdp_sfr <- pdp::partial(rf_model,
#                          train = hed_df[hed_df$use == 'sfr', ],
#                          pred.var = "trans_period",
#                          pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'SFR')
# pdp_th <- pdp::partial(rf_model,
#                          train = hed_df[hed_df$use == 'townhouse', ],
#                          pred.var = "trans_period",
#                          pred.grid = pred_grid) %>%
#   dplyr::rename(period = trans_period,
#                 value = yhat) %>%
#   dplyr::mutate(index = 100*(value/value[1]),
#                 from = 'TH')
#
# pdp_use <- dplyr::bind_rows(list(pdp_th, pdp_sfr))
# ggplot(pdp_use, aes(x=period,y=index, group=from, color=from))+geom_line()
#
# # hed_df$dist <- sqrt((hed_df$longitude - -122.3398)^2 + (hed_df$latitude - 47.6074)^2)
# # hed_df$distd <-
#
