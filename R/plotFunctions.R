#'
#' Plot method for `hpiindex` object
#'
#' Specific plotting method for hpiindex objects
#'
#' @param x Object to plot of class `hpiindex``
#' @param show_imputed default = FALSE; highlight the imputed points
#' @param smooth default = FALSE; plot the smoothed index
#' @param ... Additional Arguments
#' @import ggplot2
#' @method plot hpiindex
#' @return `plotindex` object inheriting from a ggplot object
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # With a raw transaction data.frame
#'  rt_data <- rtCreateTrans(trans_df = ex_sales,
#'                           prop_id = 'pinx',
#'                           trans_id = 'sale_id',
#'                           price = 'sale_price',
#'                           periodicity = 'monthly',
#'                           date = 'sale_date')
#'
#'  # Create model object
#'  hpi_model <- hpiModel(model_type = 'rt',
#'                        hpi_df = rt_data,
#'                        estimator = 'base',
#'                        log_dep = TRUE)
#'
#'  # Create Index
#'  hpi_index <- modelToIndex(hpi_model,
#'                            max_period = 84)
#'
#'  # Make Plot
#'  plot(hpi_index)
#'
#' @export

plot.hpiindex <- function(x,
                          show_imputed=FALSE,
                          smooth=FALSE,
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=x$numeric,
                         y=as.numeric(x$value),
                         imp=x$imputed,
                         stringsAsFactors=FALSE)

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes_string(x="x", y="y")) +
    geom_line(size=1.1, color='gray40') +
    ylab("Index Value\n") +
    xlab('\nTime Period')

  if (show_imputed){

    hpi_data$imp <- ifelse(hpi_data$imp, 1, 0)

    gg_obj <- gg_obj +
      geom_point(data=hpi_data,
                 aes_string(x="x", y="y",
                     color="as.factor(imp)",
                     size="imp")) +
      scale_color_manual(values=c('black', 'red')) +
      theme(legend.position="none")
  }

  if (smooth){

    if ('smooth' %in% names(x)){

      sm_data <- data.frame(x=x$numeric,
                            y=as.numeric(x$smooth),
                            stringsAsFactors=FALSE)

      gg_obj <- gg_obj +
        geom_line(data=sm_data,
                  aes_string(x="x", y="y"),
                  size=1.3,
                  linetype=1,
                  color='red')
    } else {
      message('No smoothed index (index_obj$smooth) present.\n')
    }

  }

  # Return Values
  structure(gg_obj, class = c('plotindex', class(gg_obj)))

}

#'
#' Plot method for `hpi` object
#'
#' Specific plotting method for hpi objects
#'
#' @method plot hpi
#' @param x Object to plot of class `hpi`
#' @param ... Additional Arguments
#' @return `plotindex` object inheriting from a ggplot object
#' @importFrom graphics plot
#' @section Further Details:
#' Additional argument can include those argument for `plot.hpindex``
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Plot data
#'  plot(rt_index)
#'  plot(rt_index, smooth = TRUE)
#'
#' @export

plot.hpi <- function(x,
                     ...){

  plot(x$index, ...)

}

#'
#' Plot method for `indexvolatility` object
#'
#' Specific plotting method for indexvolatility objects
#'
#' @method plot indexvolatility
#' @param x Object to plot of class `indexvolatility``
#' @param ... Additional Arguments
#' @return `plotvolatility` object inheriting from a ggplot object
#' @import ggplot2
#' @examples
#'
#'  # Load Data
#'  data(ex_sales)
#'
#'  # Create index with raw transaction data
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Calculate Volatility
#'  index_vol <- calcVolatility(index = rt_index,
#'                              window = 3)
#'
#'  # Make Plot
#'  plot(index_vol)
#'
#' @export

plot.indexvolatility <- function(x, ...){

  # Set up dimensions
  data_df <- data.frame(time_period=1:length(attr(x, 'orig')),
                        volatility = c(rep(NA_integer_, attr(x, 'window')),
                                       as.numeric(x$roll)),
                        stringsAsFactors=FALSE)

  # Plot base volatility
  vol_plot <- ggplot(data_df, aes_string(x="time_period", y="volatility")) +
    geom_line(color='navy', size=2) +
    ylab('Volatility\n') +
    xlab('\nTime Period') +
    geom_hline(yintercept = x$mean, size=1, linetype = 2, color='gray50') +
    geom_hline(yintercept = x$median, size=1, linetype = 3, color='gray50' )

  # Return Plot
  structure(vol_plot, class = c('plotvolatility', class(vol_plot)))

}

#'
#' Plot method for `hpiaccuracy` object
#'
#' Specific plotting method for hpiaccuracy objects
#'
#' @method plot hpiaccuracy
#' @param x Object to plot of class `hpiaccuracy``
#' @param return_plot default = FALSE; Return the plot to the function call
#' @param do_plot default = FALSE; Execute plotting to terminal/console
#' @param use_log_error [FALSE] Use the log error?
#' @param ... Additional Arguments
#' @return `plotaccuracy` object inheriting from a ggplot object
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom graphics plot
#' @importFrom gridExtra grid.arrange
#' @examples
#'
#'  # Load Data
#'  data(ex_sales)
#'
#'  # Create Index
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Calculate insample accuracy
#'  hpi_accr <- calcAccuracy(hpi_obj = rt_index,
#'                           test_type = 'rt',
#'                           test_method = 'insample')
#'
#'  # Make Plot
#'  plot(hpi_accr)
#'
#' @export

plot.hpiaccuracy <- function(x,
                             return_plot = FALSE,
                             do_plot = TRUE,
                             use_log_error = FALSE,
                             ...){

  if (use_log_error) x$error <- x$log_error

  # Get period count
  p_cnt <- length(unique(x$pred_period))

  # Make the absolute box plot
  bar_abs <- ggplot(x, aes_string(x="as.factor(pred_period)",
                                          y="abs(error)"), alpha=.5) +
    geom_boxplot(fill='lightblue') +
    coord_cartesian(ylim=c(0, quantile(abs(x$error),.99))) +
    ylab('Absolute Error') +
    xlab('Time Period')

  # Make the magnitude box plot
  bar_mag <- ggplot(x, aes_string(x="as.factor(pred_period)",
                                         y="error"), alpha=.5) +
    geom_boxplot(fill='salmon') +
    coord_cartesian(ylim=c(stats::quantile(x$error, .01),
                           stats::quantile(x$error, .99))) +
    ylab('Error') +
    xlab('Time Period')

  # Adjust axis if too many periods
  if (p_cnt > 12){
    breaks <- seq(from=min(x$pred_period),
                  to=max(x$pred_period),
                  length.out=12)
    bar_abs <- bar_abs +
      scale_x_discrete(breaks=breaks)

    bar_mag <- bar_mag +
      scale_x_discrete(breaks=breaks)
  }

  # Make absolute density plot
  dens_abs <- ggplot(x, aes_string(x="abs(error)"), alpha=.5) +
    geom_density(fill='lightblue') +
    coord_cartesian(xlim=c(0, stats::quantile(abs(x$error),.99))) +
    xlab('Absolute Error') +
    ylab('Density of Error')

  # Make magnitude density plot
  dens_mag <- ggplot(x, aes_string(x="error"), alpha=.5) +
    geom_density(fill='salmon') +
    coord_cartesian(xlim=c(stats::quantile(x$error, .01),
                           stats::quantile(x$error, .99))) +
    xlab('Error') +
    ylab('Density of Error')

  # Combine
  full_plot <- gridExtra::grid.arrange(bar_abs, bar_mag, dens_abs, dens_mag,
                                       nrow = 2)

  # Plot
  if (do_plot) plot(full_plot)

  # Return or plot
  if (return_plot){
    return(structure(full_plot, class = c('plotaccuracy', class(full_plot))))
  }

}

#'
#' Plot method for `seriesaccuracy` object
#'
#' Specific plotting method for seriesaccuracy objects
#'
#' @method plot seriesaccuracy
#' @param x Object of class `hpiaccuracy``
#' @param return_plot default = FALSE; Return the plot to the function call
#' @param ... Additional argument (passed to `plot.hpiaccuracy()``)
#' @return `plotaccuracy` object inheriting from a ggplot object
#' @import ggplot2
#' @importFrom graphics plot
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  #  Create Series (Suppressing messages do to small sample size of this example)
#'  suppressMessages(
#'     hpi_series <- createSeries(hpi_obj = rt_index,
#'                                train_period = 12))
#'
#'  # Calculate insample accuracy
#'  hpi_series_accr <- calcSeriesAccuracy(series_obj = hpi_series,
#'                                        test_type = 'rt',
#'                                        test_method = 'insample')
#'  # Make Plot
#'  plot(hpi_series_accr)
#'
#' @export

plot.seriesaccuracy <- function(x,
                                return_plot = FALSE,
                                ...){

  class(x) <- c('hpiaccuracy', 'data.frame')
  plot(x, return_plot=return_plot, do_plot=FALSE, ...)

}

#'
#' Plot method for `serieshpi` object
#'
#' Specific plotting method for serieshpi objects
#'
#' @method plot serieshpi
#' @param x Object of class `serieshpi`
#' @param smooth default = FALSE; plot the smoothed object
#' @param ... Additional Arguments`
#' @return `plotseries` object inheriting from a ggplot object
#' @import ggplot2
#' @importFrom purrr map
#' @examples
#'
#'  # Load data
#'  data(ex_sales)
#'
#'  # Create index
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Create Series (Suppressing messages do to small sample size of this example)
#'  suppressMessages(
#'    hpi_series <- createSeries(hpi_obj = rt_index,
#'                               train_period = 12))
#'
#'  # Make Plot
#'  plot(hpi_series)
#'
#' @export

plot.serieshpi<- function(x,
                          smooth = FALSE,
                          ...){

  # Extract the indexes
  indexes_. <- purrr::map(.x=x$hpis,
                          .f = function(x) x$index)

  # Get the longest
  largest <- indexes_.[[length(indexes_.)]]

  # Set the value field
  if (smooth && 'smooth' %in% names(largest)){
    index_name <- 'smooth'
  } else {
    index_name <- 'value'
  }

  # Create blank_df
  blank_df <- data.frame(time_period = 1:length(largest[[index_name]]),
                         value=seq(min(largest[[index_name]]),
                                   max(largest[[index_name]]),
                                   length.out=length(largest[[index_name]])),
                         stringsAsFactors=FALSE)

  # Plot canvas
  series_plot <- ggplot(blank_df,
                        aes_string(x="time_period", y="value"))

  # Plot each of the non-terminal indexes
  for(i in 1:length(indexes_.)){

    data_df <- data.frame(x=1:length(indexes_.[[i]][[index_name]]),
                          y=as.numeric(indexes_.[[i]][[index_name]]),
                          stringsAsFactors=FALSE)
    series_plot <- series_plot + geom_line(data=data_df,
                                           aes_string(x="x",y="y"),
                                           color='gray70')
  }

  # Add the terminal index
  data_df <- data.frame(x=1:length(indexes_.[[length(indexes_.)]][[index_name]]),
                        y=as.numeric(indexes_.[[length(indexes_.)]][[index_name]]),
                        stringsAsFactors=FALSE)

  series_plot <- series_plot + geom_line(data=data_df,
                                         aes_string(x="x",y="y"),
                                         color='red',
                                         size=2) +
    ylab('Index Value\n') +
    xlab('\nTime Period')

  structure(series_plot, class = c('plotseries', class(series_plot)))

}

#'
#' Plot method for `seriesrevision` object
#'
#' Specific plotting method for seriesrevision objects
#'
#' @method plot seriesrevision
#' @param x Object to plot of class `seriesrevision`
#' @param measure default = 'median'; Metric to plot ('median' or 'mean')
#' @param ... Additional Arguments
#' @return `plotrevision` object inheriting from a ggplot object
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @examples
#'
#'  # Load example sales
#'  data(ex_sales)
#'
#'  # Create Index
#'  rt_index <- rtIndex(trans_df = ex_sales,
#'                      periodicity = 'monthly',
#'                      min_date = '2010-06-01',
#'                      max_date = '2015-11-30',
#'                      adj_type = 'clip',
#'                      date = 'sale_date',
#'                      price = 'sale_price',
#'                      trans_id = 'sale_id',
#'                      prop_id = 'pinx',
#'                      estimator = 'robust',
#'                      log_dep = TRUE,
#'                      trim_model = TRUE,
#'                      max_period = 48,
#'                      smooth = FALSE)
#'
#'  # Create Series (Suppressing messages do to small sample size of this example)
#'  suppressMessages(
#'    hpi_series <- createSeries(hpi_obj = rt_index,
#'                               train_period = 12))
#'
#'  # Calculate revision
#'  series_rev <-  calcRevision(series_obj = hpi_series)
#'
#'  # Make Plot
#'  plot(series_rev)
#'
#' @export

plot.seriesrevision <- function(x,
                                measure = 'median',
                                ...){

  # Make Data
  plot_data <- x$period

  if (measure == 'median'){
    plot_data$revision <- plot_data$median
    yint <- x$median
    y_lab <- 'Median Revision\n'
  } else {
    plot_data$revision <- plot_data$mean
    yint <- x$mean
    y_lab <- 'Mean Revision\n'
  }

  # Create Plot
  plot_data <- plot_data %>%
    dplyr::mutate(col = ifelse(.data$revision > 0, 1, 0))

  rev_plot <- ggplot(plot_data, aes_string(x="period",
                                           y="revision",
                                           fill="as.factor(col)",
                                           alpha=.5)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('red', 'blue')) +
    geom_hline(yintercept = yint, size=1, linetype = 2) +
    ylab(y_lab) +
    xlab('\nTime Period') +
    theme(legend.position='none',
          legend.title = element_blank())

  structure(rev_plot, class = c('plotrevision', class(rev_plot)))

}
