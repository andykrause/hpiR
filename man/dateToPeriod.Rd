% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateToPeriod.R
\name{dateToPeriod}
\alias{dateToPeriod}
\title{dateToPeriod}
\usage{
dateToPeriod(sales_df, date, periodicity = "year", min_date = NULL,
  max_date = NULL, adj_type = "move", ...)
}
\arguments{
\item{sales_df}{data.frame of raw sales transactions}

\item{date}{name of field containing the date of the sale in Date or POSIXt format}

\item{periodicity}{type of periodicity to use ('year', 'qtr', 'month' or 'week)}
}
\value{
data frame with three new fields:
date_period: integer value counting from the minimum sale date in the periodicity selected. Base value is 1. Primarily for modeling
date_value: float value of year and periodicty in numeric form (primarily for plotting)
date_name: text value of the period in the format, "Year-Period". (primarily for labeling)
}
\description{
Convert dates into time periods for use in sale-resale models
}
\section{Further Details}{

date_period conat from the minimum sale date provided.  As such the period counts
are relative, not absolute
Additionally, this function modifies the data.frame that it is given and return that same
data.frame that it is given and returns that data.frame with the new fields attached.
It does so because this function is not intended as a stand-alone function but rather
one to be called by the ***CreateSales set of functions with hpiR
}

\examples{
seattle_sales <- dateToPeriod(sales_df = seattle_sales,
                              date = sale_date,
                              periodicity = 'qtr')
}