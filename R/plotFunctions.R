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
                          smooth=NULL,
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=index_obj$numeric,
                         y=as.numeric(index_obj$index),
                         imp=index_obj$imputed,
                         stringsAsFactors=FALSE)

  ## Make the base plot object
  gg_obj <- ggplot(hpi_data, aes(x=x, y=y)) +
    geom_line(size=2) +
    ylab("Index Value\n") +
    xlab('\nTime Period')

  if (show_imputed){

    hpi_data$imp <- ifelse(hpi_data$imp, 3, 0)

    gg_obj <- gg_obj +
      geom_point(data=hpi_data, aes(x=x, y=y,
                                    color=as.factor(imp),
                                    size=imp)) +
      theme(legend.position="none")
  }

  if (!is.null(smooth)){

    if (smooth %in% names(index_obj)){

      sm_data <- data.frame(x=index_obj$numeric,
                             y=as.numeric(index_obj[[smooth]]),
                             stringsAsFactors=FALSE)

      gg_obj <- gg_obj +
        geom_line(data=sm_data,
                  aes(x=x, y=y),
                  size=1,
                  linetype=2,
                  color='red')

    }

  }

  # Return Values
  structure(gg_obj, class = c('indexplot', class(gg_obj)))

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

#' @title plot.indexsmooth
#' @export

plot.indexsmooth <- function(s_index){

  # Extract Length
  l <- length(s_index$index)

  # Build Data
  plot_data <- data.frame(period=rep(1:l, 2),
                          index=c(s_index$index, s_index$original),
                          type=c(rep('Smoothed ', l), rep('Raw Index   ', l)),
                          stringsAsFactors=FALSE)

  # Make Plbot
  smooth_plot <- ggplot(plot_data,
                        aes(x=period, y=index,
                            group = as.factor(type),
                            color=as.factor(type),
                            size=as.factor(type))) +
    geom_line() +
    scale_color_manual(values=c('gray50', 'red')) +
    scale_size_manual(values=c(1.2, 1.8)) +
    ylab('Index Value\n') +
    xlab('\nTime Period') +
    theme(legend.position='bottom',
          legend.title = element_blank())

  # Return
  structure(smooth_plot, class = c('smoothplot', class(smooth_plot)))

}

#' @title plot.hpiblend
#' @export

plot.hpiblend <- function(b_index){

  # Create index data
  index_data <- data.frame(period=b_index$period,
                           index=as.numeric(b_index$index),
                           name='Blended',
                           type='b',
                           stringsAsFactors=FALSE)

 anc_data <- data.frame(period=rep(b_index$period, length(b_index$parents)),
                        index=unlist(b_index$parents),
                        name=as.character(paste0('Ancestor  :',
                              sort(rep(1:length(b_index$parents),
                                         length(b_index$index))))),
                         type='a',
                        stringsAsFactors = FALSE)

  plot_data <- rbind(index_data, anc_data)

  # Set colors and sizes
  col_vals <- c('gray50', 'blue')
  size_vals <- c(.5, 1.5)


  # Create plot
  blend_plot <- ggplot(plot_data,
                        aes(x=period, y=index,
                            group = as.factor(name),
                            color=as.factor(type),
                            size=as.factor(type))) +
    geom_line() +
    scale_color_manual(values=col_vals) +
    scale_size_manual(values=size_vals) +
    ylab('Index Value\n') +
    xlab('\nTime Period') +
    theme(legend.position='none',
          legend.title = element_blank())

  structure(blend_plot, class = c('blendplot', class(blend_plot)))

}

#' @title plot.hpirevision
#' @export

plot.hpirevision <- function(rev_obj,
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

  structure(rev_plot, class = c('revisionplot', class(rev_plot)))

}

#' @title plot.indexerrors
#' @export

plot.indexerrors <- function(error_obj){

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

  # Plot all four
  gridExtra::grid.arrange(bar_abs, bar_mag, dens_abs, dens_mag,
                                    nrow = 2)

}

#' @title plot.hpiseries
#' @export
plot.hpiseries <- function(series_obj){

  # Extract the dimensions
  largest <- series_obj[[length(series_obj)]]
  blank_df <- data.frame(time_period = 1:length(largest$index),
                         value=seq(min(largest$index),
                                   max(largest$index),
                                   length.out=length(largest$index)),
                         stringsAsFactors=FALSE)

  # Plot canvas
  series_plot <- ggplot(blank_df,
                        aes(x=time_period, y=value))

  # Plot each of the non-terminal indexes
  for(i in 1:length(series_obj)){

    data_df <- data.frame(x=1:length(series_obj[[i]]$index),
                          y=as.numeric(series_obj[[i]]$index),
                          stringsAsFactors=FALSE)
    series_plot <- series_plot + geom_line(data=data_df,
                                           aes(x=x,y=y),
                                           color='gray70')
  }

  # Add the terminal index
  data_df <- data.frame(x=1:length(series_obj[[length(series_obj)]]$index),
                        y=as.numeric(series_obj[[length(series_obj)]]$index),
                        stringsAsFactors=FALSE)

  series_plot <- series_plot + geom_line(data=data_df,
                                         aes(x=x,y=y),
                                         color='red',
                                         size=2) +
                               ylab('Index Value\n') +
                               xlab('\nTime Period')

  structure(series_plot, class = c('seriesplot', class(series_plot)))

}

#' @title plot.indexvol
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

  # Plot Original Index
  orig_df <- data.frame(time_period=1:length(attr(vol_obj, 'orig')),
                        index=as.numeric(attr(vol_obj, 'orig')),
                        stringsAsFactors=FALSE)
  orig_plot <- ggplot(orig_df, aes(x=time_period, y=index)) +
    geom_line(color='black', size=2) +
    ylab('Index Value\n') +
    xlab('\nTime Period')

  # Combine
  gridExtra::grid.arrange(vol_plot, orig_plot, nrow = 2)

}

