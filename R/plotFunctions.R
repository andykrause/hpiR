#' @title plot.hpiindex
#' @description Simple Plot of an hpiindex object
#' @usage Lorem Ipsum...
#' @param index_obj Object of class hpiindex
#' @param show_imputed Highlight the imputed points
#' @param ... Additional Arguments
#' @return ggplot object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

plot.hpiindex <- function(index_obj,
                          show_imputed=FALSE,
                          smooth=FALSE,
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=index_obj$numeric,
                         y=as.numeric(index_obj$value),
                         imp=index_obj$imputed,
                         stringsAsFactors=FALSE)

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line(size=1.1) +
    ylab("Index Value\n") +
    xlab('\nTime Period')

  if (show_imputed){

    hpi_data$imp <- ifelse(hpi_data$imp, 1, 0)

    gg_obj <- gg_obj +
      geom_point(data=hpi_data,
                 aes(x=x, y=y,
                     color=as.factor(imp),
                     size=imp)) +
      scale_color_manual(values=c('black', 'red')) +
      theme(legend.position="none")
  }

  if (smooth){

    if ('smooth' %in% names(index_obj)){

      sm_data <- data.frame(x=index_obj$numeric,
                            y=as.numeric(index_obj$smooth),
                            stringsAsFactors=FALSE)

      gg_obj <- gg_obj +
        geom_line(data=sm_data,
                  aes(x=x, y=y),
                  size=1,
                  linetype=1,
                  color='red')
    } else {
      message('No smoothed index (index_obj$smooth) present.\n')
    }

  }

  # Return Values
  structure(gg_obj, class = c('plotindex', class(gg_obj)))

}

#' @title plot.hpi
#' @description Simple Plot of an HPI object
#' @usage Lorem Ipsum...
#' @param hpi_obj Object of class HPI
#' @param show_imputed Highlight the imputed points
#' @param ... Additional Arguments
#' @return ggplot object
#' @section Further Details:
#' Lorem Ipsum...
#' @examples
#' a <- 1
#' @export

plot.hpi <- function(hpi_obj,
                     ...){

  plot(hpi_obj$index, ...)

}

#' @title plot.hpiblend
#' @export

#' @title plot.indexvolatility
#' @export

plot.indexvolatility <- function(vol_obj){

  # Set up dimensions
  data_df <- data.frame(time_period=1:length(attr(vol_obj, 'orig')),
                        volatility = c(rep(NA_integer_, attr(vol_obj, 'window')),
                                       as.numeric(vol_obj$roll)),
                        stringsAsFactors=FALSE)

  # Plot base volatility
  vol_plot <- ggplot(data_df, aes(x=time_period, y=volatility)) +
    geom_line(color='navy', size=2) +
    ylab('Volatility\n') +
    xlab('\nTime Period') +
    geom_hline(yintercept = vol_obj$mean, size=1, linetype = 2, color='gray50') +
    geom_hline(yintercept = vol_obj$median, size=1, linetype = 3, color='gray50' )

  # Return Plot
  structure(vol_plot, class = c('plotvolatility', class(vol_plot)))

}

#' @title plot.indexaccuracy
#' @export

plot.indexaccuracy <- function(error_obj,
                               return_plot = FALSE){

  # Get period count
  p_cnt <- length(unique(error_obj$pred_period))

  # Make the absolute box plot
  bar_abs <- ggplot(error_obj, aes(x=as.factor(pred_period),
                                   y=abs(pred_error)), alpha=.5) +
    geom_boxplot(fill='lightblue') +
    coord_cartesian(ylim=c(0, quantile(abs(error_obj$pred_error),.99))) +
    ylab('Absolute Error') +
    xlab('Time Period')

  # Make the magnitude box plot
  bar_mag <- ggplot(error_obj, aes(x=as.factor(pred_period),
                                   y=pred_error), alpha=.5) +
    geom_boxplot(fill='salmon') +
    coord_cartesian(ylim=c(quantile(error_obj$pred_error, .01),
                           quantile(error_obj$pred_error, .99))) +
    ylab('Error') +
    xlab('Time Period')

  # Adjust axis if too many periods
  if (p_cnt > 12){
    breaks <- seq(from=min(error_obj$pred_period),
                  to=max(error_obj$pred_period),
                  length.out=12)
    bar_abs <- bar_abs +
      scale_x_discrete(breaks=breaks)

    bar_mag <- bar_mag +
      scale_x_discrete(breaks=breaks)
  }

  # Make absolute density plot
  dens_abs <- ggplot(error_obj, aes(x=abs(pred_error)), alpha=.5) +
    geom_density(fill='lightblue') +
    coord_cartesian(xlim=c(0, quantile(abs(error_obj$pred_error),.99))) +
    xlab('Absolute Error') +
    ylab('Density of Error')

  # Make magnitude density plot
  dens_mag <- ggplot(error_obj, aes(x=pred_error), alpha=.5) +
    geom_density(fill='salmon') +
    coord_cartesian(xlim=c(quantile(error_obj$pred_error, .01),
                           quantile(error_obj$pred_error, .99))) +
    xlab('Error') +
    ylab('Density of Error')

  # Combine
  full_plot <- gridExtra::grid.arrange(bar_abs, bar_mag, dens_abs, dens_mag,
                                       nrow = 2)

  # Plot
  plot(full_plot)

  # Return or plot
  if (return_plot){
    return(structure(full_plot, class = c('plotaccuracy', class(full_plot))))
  }

}


#' @title plot.serieshpi
#' @export

plot.serieshpi<- function(series_obj,
                          smooth = FALSE){

  # Extract the indexes
  indexes_. <- purrr::map(.x=series_obj$hpis,
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
                        aes(x=time_period, y=value))

  # Plot each of the non-terminal indexes
  for(i in 1:length(indexes_.)){

    data_df <- data.frame(x=1:length(indexes_.[[i]][[index_name]]),
                          y=as.numeric(indexes_.[[i]][[index_name]]),
                          stringsAsFactors=FALSE)
    series_plot <- series_plot + geom_line(data=data_df,
                                           aes(x=x,y=y),
                                           color='gray70')
  }

  # Add the terminal index
  data_df <- data.frame(x=1:length(indexes_.[[length(indexes_.)]][[index_name]]),
                        y=as.numeric(indexes_.[[length(indexes_.)]][[index_name]]),
                        stringsAsFactors=FALSE)

  series_plot <- series_plot + geom_line(data=data_df,
                                         aes(x=x,y=y),
                                         color='red',
                                         size=2) +
    ylab('Index Value\n') +
    xlab('\nTime Period')

  structure(series_plot, class = c('plotseries', class(series_plot)))

}

#' @title plot.seriesrevision
#' @export

plot.seriesrevision <- function(rev_obj,
                                measure = 'median',
                               ...){

  # Make Data
  plot_data <- rev_obj$period

  if (measure == 'median'){
    plot_data$revision <- plot_data$median
    yint <- rev_obj$median
    y_lab <- 'Median Revision\n'
  } else {
    plot_data$revision <- plot_data$mean
    yint <- rev_obj$mean
    y_lab <- 'Mean Revision\n'
  }

  # Create Plot
  plot_data <- plot_data %>%
    dplyr::mutate(col = ifelse(revision > 0, 1, 0))

  rev_plot <- ggplot(plot_data, aes(x=period, y=revision, fill=as.factor(col),
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


