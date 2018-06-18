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
                          ...){

  ## Extract Data
  hpi_data <- data.frame(x=index_obj$numeric,
                         y=as.numeric(index_obj$index),
                         imp=index_obj$imputed)

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

  # Return Values
  gg_obj

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

#' @title plot.smoothindex
#' @export

plot.smoothindex <- function(s_index){

  # Extract Length
  l <- length(s_index)

  # Build Data
  plot_data <- data.frame(period=rep(1:l, 2),
                          index=c(s_index, attr(s_index, 'raw')),
                          type=c(rep('Smoothed ', l), rep('Raw Index   ', l)))

  # Make Plbot
  smooth_plot <- ggplot(plot_data,
                        aes(x=period, y=index, color=as.factor(type),
                            size=as.factor(type))) +
    geom_line() +
    scale_color_manual(values=c('gray50', 'red')) +
    scale_size_manual(values=c(1.2, 2.5)) +
    ylab('Index Value\n') +
    xlab('\nTime Period') +
    theme(legend.position='bottom',
          legend.title = element_blank())

  # Return
  smooth_plot

}

#' @title plot.blendindex
#' @export

plot.blendindex <- function(b_index){

  # Get index length
  l <- length(b_index)

  # Create index data
  index_data <- data.frame(period=1:l,
                           index=as.numeric(unlist(b_index)),
                           name=as.character('Blended'),
                           type='b')

  anc <- attr(b_index, 'ancestry')$parents
  anc_data <- data.frame(period=rep(1:l, length(anc)),
                         index=unlist(lapply(anc, function(x) x$index)),
                         name=as.character(paste0('Ancestor  :',
                                                  c(sort(rep(1:length(anc), l))))),
                         type='a')
  plot_data <- rbind(index_data, anc_data)

  # Set colors and sizes
  col_vals <- c('blue', rep('gray50', length(anc)))
  size_vals <- c(1.5, rep(.5, length(anc)))

  # Create plot
  blend_plot <- ggplot(plot_data,
                        aes(x=period, y=index, group = name, color=as.factor(type),
                            size=as.factor(type))) +
    geom_line() +
    scale_color_manual(values=col_vals) +
    scale_size_manual(values=size_vals) +
    ylab('Index Value\n') +
    xlab('\nTime Period') +
    theme(legend.position='none',
          legend.title = element_blank())

  blend_plot

}

#' @title plot.indexrevision
#' @export

plot.indexrevision <- function(rev_obj,
                               measure = 'median',
                               ...){

  # Make Data
  plot_data <- rev_obj$period

  if (measure == 'median'){
    plot_data$revision <- plot_data$median
    yint <- rev_obj$median
  } else {
    plot_data$revision <- plot_data$mean
    yint <- rev_obj$mean
  }

  # Create Plot
  plot_data <- plot_data %>%
    dplyr::mutate(col = ifelse(revision > 0, 1, 0))

  rev_plot <- ggplot(plot_data, aes(x=period, y=revision, fill=as.factor(col),
                                    alpha=.5)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('red', 'blue')) +
    geom_hline(yintercept = yint, size=1, linetype = 2) +
    ylab('Average Revision\n') +
    xlab('\nTime Period') +
    theme(legend.position='none',
          legend.title = element_blank())

  rev_plot
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
  gridExtra::grid.arrange(bar_abs, bar_mag, dens_abs, dens_mag, nrow = 2)

}

#' @title plot.hpiseries
#' @export
plot.hpiseries <- function(series_obj){

  # Extract the dimensions
  largest <- series_obj[[length(series_obj)]]
  blank_df <- data.frame(time_period = 1:length(largest),
                         value=seq(min(largest), max(largest), length.out=length(largest)))

  # Plot canvas
  series_plot <- ggplot(blank_df,
                        aes(x=time_period, y=value))

  # Plot each of the non-terminal indexes
  for(i in 1:length(series_obj)){

    data_df <- data.frame(x=1:length(series_obj[[i]]),
                          y=as.numeric(series_obj[[i]]))
    series_plot <- series_plot + geom_line(data=data_df,
                                           aes(x=x,y=y),
                                           color='gray70')
  }

  # Add the terminal index
  data_df <- data.frame(x=1:length(series_obj[[length(series_obj)]]),
                        y=as.numeric(series_obj[[length(series_obj)]]))

  series_plot + geom_line(data=data_df,
                          aes(x=x,y=y),
                          color='red',
                          size=2) +
            ylab('Index Value\n') +
            xlab('\nTime Period')

}

#' @title plot.indexvol
#' @export
plot.indexvol <- function(vol_obj){

  # Set up dimensions
  data_df <- data.frame(time_period=1:length(attr(vol_obj, 'orig')),
                        volatility = c(rep(NA_integer_, attr(vol_obj, 'window')),
                                       as.numeric(vol_obj$roll)))

  # Plot base volatility
  vol_plot <- ggplot(data_df, aes(x=time_period, y=volatility)) +
    geom_line(color='navy') +
    ylab('Volatility\n') +
    xlab('\nTime Period')

  # Plot Original Index
  orig_df <- data.frame(time_period=1:length(attr(vol_obj, 'orig')),
                        index=as.numeric(attr(vol_obj, 'orig')))
  orig_plot <- ggplot(orig_df, aes(x=time_period, y=index)) +
    geom_line(color='black', size=2) +
    ylab('Index Value\n') +
    xlab('\nTime Period')

  # Combine
  suppressWarnings(gridExtra::grid.arrange(vol_plot, orig_plot, nrow = 2))

}

