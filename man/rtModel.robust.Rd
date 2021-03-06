% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtModel.R
\name{rtModel.robust}
\alias{rtModel.robust}
\title{Repeat transaction model approach with robust estimator}
\usage{
\method{rtModel}{robust}(rt_df, time_matrix, price_diff, estimator, lm_recover = TRUE, ...)
}
\arguments{
\item{rt_df}{Repeat transactions dataset from rtCreateTrans()}

\item{time_matrix}{Time matrix object from rtTimeMatrix()}

\item{price_diff}{Difference in price between the two transactions}

\item{estimator}{Type of model to estimates (base, robust, weighted).  Must be in that class.}

\item{lm_recover}{(TRUE) Allows robust model to use linear model if it fails}

\item{...}{Additional arguments}
}
\description{
Use of robust estimator in repeat transactions model approach
}
\section{Further Details}{

See `?rtModel` for more information
}

