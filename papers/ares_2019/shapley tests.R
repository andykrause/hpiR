
X <-  hed_df[, c('lot_sf', 'trans_period', 'age', 'beds', 'price')]
X$price <- log(X$price)

predictor = Predictor$new(rf_model, data = X, y=Y)

imp = FeatureImp$new(predictor, loss = "mae")
plot(imp)


shapley = Shapley$new(predictor, x.interest = hed_df[1,c('lot_sf', 'trans_period', 'age', 'beds')])
shapley$plot()


shap.values = getShapleyValues(shapley(1:6, task = bh.task, model = train("regr.lm", bh.task)))


regr.task = makeRegrTask(id = "aa", data = X, target = "price")
regr.task

regr.lrn = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))

mod = train(regr.lrn, regr.task)

x <- getShapleyValues(shapley(1:43074, task = regr.task, model = train(regr.lrn, regr.task)))

a <- as.data.frame(cbind(X$trans_period, x$trans_period))

names(a) <- c('period', 'value')
aa <- a %>% dplyr::group_by(period) %>% dplyr::summarize(med = mean(value))
plot(aa$period, aa$med)
