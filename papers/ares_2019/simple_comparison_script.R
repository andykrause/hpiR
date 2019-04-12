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
                             num.trees = 200,
                             seed = 1)

  # Set up example pdp
  pred_grid <- data.frame(trans_period = 1:max(hed_df$trans_period))

  # Calculate ICEs
  ice_all <- pdp::partial(rf_model,
                          train = hed_df,
                          pred.var = "trans_period",
                          pred.grid = pred_grid,
                          ice = TRUE) %>%
    dplyr::rename(period = trans_period,
                  value = yhat,
                  obs = yhat.id) %>%
    dplyr::group_by(obs) %>%
    dplyr::mutate(scaled_value = 100*(value / value[1]))

  pdp_all <- ice_all %>%
    dplyr::group_by(period) %>%
    dplyr::summarize(value = mean(value)) %>%
    dplyr::mutate(scaled_value = 100 * (value / value[1]))

  gg_1 <-
    ggplot() +
    geom_line(data = ice_all %>% dplyr::filter(obs == 2),
              aes(x = period, y = scaled_value), size = 1.5, color = 'black') +
    scale_color_manual(name = 'Model', values = 'gray10') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5)) +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    ggtitle('Individual Conditional Expectations Plot\n(Observation 1, Feature = "Time")\n')

  gg_5 <-
    ggplot() +
    geom_line(data = ice_all %>% dplyr::filter(obs %in% c(1, 2, 3, 5, 6)),
              aes(x = period, y = scaled_value, group = obs), size = 1, color = 'gray50') +
    scale_color_manual(name = 'Model', values = 'gray10') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5)) +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    ggtitle('Individual Conditional Expectations Plot\n(Observation 1 to 5, Feature = "Time")\n') +
    geom_line(data = ice_all %>%
                dplyr::filter(obs %in% c(1,2,3,5,6)) %>%
                dplyr::group_by(period) %>%
                dplyr::summarize(index = mean(scaled_value)),
              aes(x = period, y = index), color = 'red', size = 2)

  gg_all <-
    ggplot() +
    geom_line(data = ice_all,
              aes(x = period, y = scaled_value, group = obs), size = .1, color = 'gray10',
              alpha = .01) +
    scale_color_manual(name = 'Model', values = 'gray10') +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5)) +
    ylab('Index Value\n') +
    xlab('\nTime') +
    scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
    ggtitle('Individual Conditional Expectations Plot\n(All Observations, Feature = "Time")\n') +
    geom_line(data = pdp_all,
              aes(x = period, y = scaled_value), color = 'red', size = 2) +
    coord_cartesian(ylim = c(88, 170))


  saveRDS(gg_1, file.path(getwd(), 'papers','ares_2019', 'ice1.RDS'))
  png( file.path(getwd(), 'papers','ares_2019', 'ice1.png'), bg = "transparent",
       width = 800, height = 580)
    gg_1
  dev.off()

  saveRDS(gg_5, file.path(getwd(), 'papers','ares_2019', 'ice5.RDS'))
  png( file.path(getwd(), 'papers','ares_2019', 'ice5.png'), bg = "transparent",
       width = 800, height = 580)
  gg_5
  dev.off()

  saveRDS(pdp_all, file.path(getwd(), 'papers','ares_2019', 'gg_all.RDS'))
  png( file.path(getwd(), 'papers','ares_2019', 'pdp_all.png'), bg = "transparent",
       width = 800, height = 580)
    gg_all
  dev.off()










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

#
# rfs_hpi <- rfIndex(trans_df = hed_df,
#                   estimator = 'shap',
#                   dep_var = 'price',
#                   ind_var = c('lot_sf', 'tot_sf', 'beds', 'baths', 'eff_age', 'area',
#                               'latitude', 'longitude'),
#                   max_period = 84,
#                   smooth = FALSE,
#                   shap_k = 50,
#                   ntrees = 100)


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
                     model = 'PDP (RF)')
# rsi_df <- data.frame(period = rfs_hpi$index$period,
#                      value = as.numeric(rfs_hpi$index$value),
#                      model = 'Shapley (RF)')


rhr_df <- rbind(rti_df, hei_df, rfi_df)#, rsi_df)
rhs_df <- rbind(rts_df, hes_df, rfi_df)#, rsi_df)

gg_rhr <-
  ggplot() +
  geom_line(data = rhr_df,
            aes(x = period, y = value, group = model, color = model, size = model)) +
  scale_color_manual(name = '', values = c('gray50','red', 'black')) +
  scale_size_manual(values = c(1.5, 1.5, 2), guide = 'none' )+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  ylab('Index Value\n') +
  xlab('') +
  scale_y_continuous(breaks = seq(90, 170, 10)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Comparison of HPIs (Seattle) \n')+
  coord_cartesian(ylim = c(88, 172))

gg_rhrf <-
  gg_rhr + facet_wrap(~model) +theme(legend.position = 'none')

saveRDS(gg_rhr, file.path(getwd(), 'papers','ares_2019', 'gg_comp.RDS'))
png( file.path(getwd(), 'papers','ares_2019', 'comp.png'), bg = "transparent",
     width = 900, height = 580)
gg_rhr
dev.off()

saveRDS(gg_rhrf, file.path(getwd(), 'papers','ares_2019', 'gg_compf.RDS'))
png( file.path(getwd(), 'papers','ares_2019', 'compf.png'), bg = "transparent",
     width = 900, height = 580)
gg_rhrf
dev.off()





















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
            aes(x=time, y = coefficient), color = 'orange', size = 1) +
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
            aes(x=time, y = coefficient), color = 'orange', size = 1) +
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
            aes(x=time, y = coefficient), color = 'orange', size = 1) +
  ylab('Shapley Value') + xlab('Time Period') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Example Shapley Value Plot\n (50 Observations per Period, Feature = "Time")\n')->gg_shap50


rfs_50 <- coef_df50 %>%
  dplyr::mutate(index = 100*(coefficient+1))
ggplot() +
  geom_line(data = rfs_50,
            aes(x=time, y = index), color = 'orange', size = 1) +
  ylab('Shapley Value') + xlab('Time Period') +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(seq(1,85,12)), labels = 2010:2017) +
  ggtitle('Example Shapley Value Derived HPI\n (50 Observations per Period)\n')->gg_hpishap


saveRDS(gg_shap1, file.path(getwd(), 'papers','ares_2019', 'shap1plot.RDS'))
saveRDS(gg_shap5, file.path(getwd(), 'papers','ares_2019', 'shap5plot.RDS'))
saveRDS(gg_shap50, file.path(getwd(), 'papers','ares_2019', 'shap50plot.RDS'))
saveRDS(gg_hpishap, file.path(getwd(), 'papers','ares_2019', 'shapindex.RDS'))

png( file.path(getwd(), 'papers','ares_2019', 'shap1plot.png'), bg = "transparent",
     width = 800, height = 580)
gg_shap1
dev.off()

png( file.path(getwd(), 'papers','ares_2019', 'shap5plot.png'), bg = "transparent",
     width = 800, height = 580)
gg_shap5
dev.off()

png( file.path(getwd(), 'papers','ares_2019', 'shap50plot.png'), bg = "transparent",
     width = 800, height = 580)
gg_shap50
dev.off()

png( file.path(getwd(), 'papers','ares_2019', 'shapindex.png'), bg = "transparent",
     width = 800, height = 580)
gg_hpishap
dev.off()


