% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcForecastError.R
\name{buildForecastIDs.heddata}
\alias{buildForecastIDs.heddata}
\title{Create the row IDs for forecast accuracy (hed approach)}
\usage{
\method{buildForecastIDs}{heddata}(time_cut, hpi_df, forecast_length = 1, train = TRUE)
}
\arguments{
\item{time_cut}{Period after which to cut off data}

\item{hpi_df}{Data to be converted to training or scoring}

\item{forecast_length}{default = 1; Length of forecasting to do}

\item{train}{Default=TRUE; Create training data?  FALSE = Scoring data}
}
\description{
Generate a vector of row IDs for use in forecast accuracy tests (hed approach)
}
