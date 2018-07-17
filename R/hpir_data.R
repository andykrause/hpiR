#' Seattle Home Sales
#'
#' Seattle home sales from 2010 to 2016. Includes only detached single family
#' residences and townhomes.  Data gathered from the King County Assessor's FTP site.
#' A number of initial data munging tasks were necessary to bring the data into this format.
#'
#' @docType data
#' @usage data(seattle_sales)
#' @source King County Assessor: http://info.kingcounty.gov/assessor/DataDownload/
#' @format A \code{"data.frame"} with 43,313 rows and 16 variables
#' \describe{
#'   \item{pinx}{The unique property identifying code.  Original value is preceded by two '..'s to prevent the dropping of leading zeros}
#'   \item{sale_id}{The unique transaction identifying code.}
#'   \item{sale_price}{Price of the home}
#'   \item{sale_date}{Date of sale}
#'   \item{use_type}{Property use type}
#'   \item{area}{Assessment area or zone}
#'   \item{lot_sf}{Size of lot in square feet}
#'   \item{wfnt}{Is property waterfront?}
#'   \item{bldg_grade}{Quality of the building construction (higher is better)}
#'   \item{tot_sf}{Size of home in square feet}
#'   \item{beds}{Number of bedrooms}
#'   \item{baths}{Number of bathrooms}
#'   \item{age}{Age of home}
#'   \item{eff_age}{Age of home, considering major remodels}
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude }
#'}
#'
"seattle_sales"

#' Subset of Seattle Home Sales
#'
#' Seattle home sales from areas 13, 14,an 15 (central Seattle) 2010 to 2016.
#' Includes only detached single family residences and townhomes.  Data gathered from
#' the King County Assessor's FTP site. A number of initial data munging tasks were
#' necessary to bring the data into this format.
#'
#' @docType data
#' @usage data(ex_sales)
#' @source King County Assessor: http://info.kingcounty.gov/assessor/DataDownload/
#' @format A \code{"data.frame"} with 5,348 rows and 16 variables
#' \describe{
#'   \item{pinx}{The unique property identifying code.  Original value is preceded by
#'   two '..'s to prevent the dropping of leading zeros}
#'   \item{sale_id}{The unique transaction identifying code.}
#'   \item{sale_price}{Price of the home}
#'   \item{sale_date}{Date of sale}
#'   \item{use_type}{Property use type}
#'   \item{area}{Assessment area or zone}
#'   \item{lot_sf}{Size of lot in square feet}
#'   \item{wfnt}{Is property waterfront?}
#'   \item{bldg_grade}{Quality of the building construction (higher is better)}
#'   \item{tot_sf}{Size of home in square feet}
#'   \item{beds}{Number of bedrooms}
#'   \item{baths}{Number of bathrooms}
#'   \item{age}{Age of home}
#'   \item{eff_age}{Age of home, considering major remodels}
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude }
#'}
#'
"ex_sales"
