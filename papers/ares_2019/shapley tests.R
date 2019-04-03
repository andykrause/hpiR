
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

source(file.path(getwd(), 'papers', 'ares_2019', 'ares_script_functions.r'))

### Create Data ------------------------------------------------------------------------------------

# Hedonic Data
hed_df <- hedCreateTrans(trans_df = seattle_sales,
                         prop_id = 'pinx',
                         trans_id = 'sale_id',
                         price = 'sale_price',
                         date = 'sale_date',
                         periodicity = 'monthly')

a <- rfModel(estimator = structure('shap', class = 'shap'),
             rf_df = hed_df,
             rf_spec = NULL,
             dep_var = c('lot_sf', 'tot_sf', 'trans_period', 'bldg_grade', 'age', 'beds', 'baths'),
             ind_var = 'price',
             shap_k = 10)



X <-  hed_df[, c('lot_sf', 'trans_period', 'age', 'beds', 'price')]
X$price <- log(X$price)

regr.task = makeRegrTask(id = "aa", data = X, target = "price")
regr.task

regr.lrn = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))

regr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 200))

mod = train(regr.lrn, regr.task)

k <- 5
ss <- hed_df %>% dplyr::group_by(trans_period) %>% dplyr::slice(1:k)
kk <- which(hed_df$trans_id %in% ss$trans_id)
kk


x <- getShapleyValues(shapley(kk, task = regr.task, model = train(regr.lrn, regr.task)))

a <- as.data.frame(cbind(X$trans_period[kk], x$trans_period))

names(a) <- c('period', 'value')
aa <- a %>% dplyr::group_by(period) %>% dplyr::summarize(med = mean(value))
plot(aa$period, aa$med)

aa$med <- aa$med - aa$med[1]

aa$med <- aa$med / aa$med[1]

