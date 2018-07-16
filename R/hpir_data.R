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
#'   \item{sale_id}{The unique transation identifying code.}
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
#'   \item{sale_id}{The unique transation identifying code.}
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

#' Example `hpidata` object
#'
#' Based off of the `ex_sales` object.  This is an `hpidata` object as indicated by the
#' presence of the two standardize fields: "trans_date" and "trans_period".  All other
#' fields can vary based on the raw data being used
#'
#' @docType data
#' @usage data(ex_hpidata)
#' @source Output from the dateToPeriod() function
#' @format A \code{"hpidata"} and \code{"data.frame"} with 5,348 rows and 16 variables
#' \describe{
#'   \item{pinx}{The unique property identifying code.  Original value is preceded by
#'   two '..'s to prevent the dropping of leading zeros}
#'   \item{sale_id}{The unique transation identifying code.}
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
#'   \item{trans_date}{Formatted R date field}
#'   \item{trans_period}{Standardized relative period value (integer)}
#'}
#'
"ex_hpidata"

#' Example `heddata` object
#'
#' Based off of the `ex_hpidata` object.  This is an `heddata` object as indicated by the
#' presence of the five standardize fields: "prop_id", "trans_id", "price", "trans_date"
#' and "trans_period".  All other fields can vary based on the raw data being used
#'
#' @docType data
#' @usage data(ex_heddata)
#' @source Output from the hedCreateTrans() function
#' @format A \code{"heddata"}, \code{"hpidata"} and \code{"data.frame"} with 5,319 rows
#' and 16 variables
#' \describe{
#'   \item{prop_id}{The unique property identifying code.  Original value is preceded by
#'   two '..'s to prevent the dropping of leading zeros}
#'   \item{trans_id}{The unique transation identifying code.}
#'   \item{price}{Price of the home}
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
#'   \item{trans_date}{Formatted R date field}
#'   \item{trans_period}{Standardized relative period value (integer)}
#'}
#'
"ex_heddata"


#' Example `rtdata` object
#'
#' Based off of the `ex_hpidata` object.  This is an `rtdata` object as indicated by the
#' standardized nature of the object.  All `rtdata` object contain the same eight fields.
#'
#' @docType data
#' @usage data(ex_rtdata)
#' @source Output from the rtCreateTrans() function
#' @format A \code{"rtdata"}, \code{"hpidata"} and \code{"data.frame"} with 656 rows
#' and 8 variables
#' \describe{
#'   \item{prop_id}{The unique property identifying code}
#'   \item{period_1}{Period of the first transaction}
#'   \item{period_2}{Period of the second transaction}
#'   \item{price_1}{First Price}
#'   \item{price_2}{Second Price}
#'   \item{trans_id1}{First Unique transaction ID}
#'   \item{trans_id2}{Second Unique transaction ID}
#'   \item{pair_id}{Unique pairing ID}
#'}
#'
"ex_rtdata"

#'
#' Example `hedmodel` object
#'
#' An object of class `hedmodel`, resulting from `hedModel()`
#'
#' @docType data
#' @usage data(ex_hedmodel)
#' @source hedModel()
#' @format List of 12 objects (`lm` object)
#' \describe{
#'   \item{coefficients}{Vector of coefficients, length 87}
#'   \item{residuals}{Vector of residuals, length 5319}
#'   \item{effects}{Model effects, length 5319}
#'   \item{rank}{model rank, length 87}
#'   \item{fitted.values}{Fitted values, length 5319}
#'   \item{assign}{Vector of length 87}
#'   \item{df.residual}{Residual degrees of freeedom, integer}
#'   \item{contrasts}{list of contrasts}
#'   \item{xlevels}{levels}
#'   \item{call}{model call}
#'   \item{terms}{formula terms of the model}
#'   \item{model}{model dep and ind variables}
#' }
"ex_hedmodel"

#'
#' Example `hpi` model
#'
#' An example hpi collection object of class `hpi`
#'
#' @docType data
#' @usage data(ex_hpi)
#' @source `rtIndex()` or `hedIndex()` functions
#' @format List of three objects
#' \describe{
#'   \item{data}{Object of class `hpidata` and `rtdata` or `heddata`}
#'   \item{model}{Object of class `hpimodel`}
#'   \item{index}{Object of class `hpiindex`}
#' }
"ex_hpi"

#'
#' Example `hpiaccuracy` object
#'
#' Object containing accuracy information for an `hpi` object
#'
#' @docType data
#' @usage data(ex_hpiaccuracy)
#' @source From the `calcAccuracy()` function`
#' @format `data.frame` containing four variables:
#' \describe{
#'   \item{prop_id}{Unique property ID}
#'   \item{pred_price}{Predicted price}
#'   \item{pred_error}{Error of the prediction}
#'   \item{pred_period}{Prediction period}
#' }
"ex_hpiaccuracy"

#'
#' Example `hpiindex` object
#'
#' An object of class `hpiindex`
#'
#' @docType data
#' @usage data(ex_hpiindex)
#' @source `modelToIndex()` function
#' @format List containing five objects:
#' \describe{
#'   \item{name}{Vector of period names, length 84}
#'   \item{numeric}{Vector of numeric period values, length 84}
#'   \item{period}{Vector of period numbers, length 84}
#'   \item{value}{Vector of index values, length 84}
#'   \item{imputed}{Vector showing if value was imputed, length 84}
#' }
"ex_hpiindex"


#'
#' Example `hpimodel` object
#'
#' An `hpimodel` object example (from an `rtModel` or `hedModel` object)
#'
#' @docType data
#' @usage data(ex_hpimodel)
#' @source `hpiModel()``
#' @format List containing 8 objects:
#' \describe{
#'   \item{estimator}{type of estimator used}
#'   \item{coefficients}{data.frame of model coefficients}
#'   \item{model_obj}{`rtmodel` object}
#'   \item{mod_spec}{Full Model specification}
#'   \item{log_dep}{Is the dependent variable in log format?}
#'   \item{base_price}{mean price in period 1}
#'   \item{periods}{data.frame of period data (object)}
#'   \item{approach}{Approach used (rt or hed)}
#' }
"ex_hpimodel"


#'
#' Example `serieshpi` object
#'
#' An example `serieshpi` object
#'
#' @docType data
#' @usage data(ex_serieshpi)
#' @source `createSeries()`
#' @format list of length 2 containing
#' \describe{
#'   \item{data}{series data (class `rtdata`)}
#'   \item{hpis}{list of hpi objects (length 13)}
#' }
"ex_serieshpi"

#'
#' Example `indexvolatility` object
#'
#' An example of an `indexvolatility` object
#'
#' @docType data
#' @usage data(ex_indexvolatility)
#' @source `calcVolatility()`
#' @format List of length three containing:
#' \describe{
#'   \item{roll}{vector of period volatilities, length 79}
#'   \item{mean}{mean of volatility}
#'   \item{median}{median of volatility}
#' }
"ex_indexvolatility"

#'
#' Example `plotaccuracy` object
#'
#' An example plotaccuracy object
#'
#' @docType data
#' @usage data(ex_plotaccuracy)
#' @source plot.hpiaccuracy()
#' @format ggplot object:

"ex_plotaccuracy"

#'
#' Example `plotindex` object
#'
#' An example plotindex object
#'
#' @docType data
#' @usage data(ex_plotindex)
#' @source plot.hpiindex() or plot.hpi()
#' @format ggplot object:
"ex_plotindex"

#'
#' Example `plotrevision` object
#'
#' An example plotrevision object
#'
#' @docType data
#' @usage data(ex_plotrevision)
#' @source plot.seriesrevision()
#' @format ggplot object:
"ex_plotrevision"

#'
#' Example `plotseries` object
#'
#' An example plotseries object
#'
#' @docType data
#' @usage data(ex_plotseries)
#' @source plot.serieshpi()
#' @format ggplot object:
"ex_plotseries"

#'
#' Example `plotvolatility` object
#'
#' An example plotvolatility object
#'
#' @docType data
#' @usage data(ex_plotvolatility)
#' @source plot.indexvolatility()
#' @format ggplot object:
"ex_plotvolatility"

#'
#' Example `rtmodel` object
#'
#' An object of class `rtmodel`, resulting from `rtModel()`
#'
#' @docType data
#' @usage data(ex_rtmodel)
#' @source rtModel()
#' @format List of 11 objects (`lm` object)
#' \describe{
#'   \item{coefficients}{Vector of coefficients, length 87}
#'   \item{residuals}{Vector of residuals, length 5319}
#'   \item{effects}{Model effects, length 5319}
#'   \item{rank}{model rank, length 87}
#'   \item{fitted.values}{Fitted values, length 5319}
#'   \item{assign}{Vector of length 87}
#'   \item{df.residual}{Residual degrees of freeedom, integer}
#'   \item{xlevels}{levels}
#'   \item{call}{model call}
#'   \item{terms}{formula terms of the model}
#'   \item{model}{model dep and ind variables}
#' }
"ex_rtmodel"

#'
#' Example `seriesaccuracy` object
#'
#' Object containing accuracy information for a `serieshpi` object
#'
#' @docType data
#' @usage data(ex_seriesaccuracy)
#' @source From the `calcSeriesAccuracy()` function`
#' @format `data.frame` containing five variables:
#' \describe{
#'   \item{prop_id}{Unique property ID}
#'   \item{pred_price}{Predicted price}
#'   \item{pred_error}{Error of the prediction}
#'   \item{pred_period}{Prediction period}
#'   \item{series}{series number from which prediction was generated}
#' }
"ex_seriesaccuracy"

#'
#' Example `seriesrevision` object
#'
#' Revision statistics for a series
#'
#' @docType data
#' @usage data(ex_seriesrevision)
#' @source `calcRevision()``
#' @format List of 3 containing:
#' \describe{
#'   \item{period}{data.frame with period number, mean and median revision for each period}
#'   \item{median}{median revision of all periods}
#'   \item{mean}{mean revision of all periods}
#' }
"ex_seriesrevision"

#'
#' Example `smoothindex` object
#'
#' An index that has been smoothed
#'
#' @docType data
#' @usage data(ex_smoothindex)
#' @source `smoothIndex()`
#' @format `ts` object of length 84
"ex_smoothindex"

#'
#' Example `timematrix` object
#'
#' Time matrix object that is used in the `rtmodel()` (repeat transaction approach)
#'
#' @docType data
#' @usage data(ex_timematrix)
#' @source `rtTimeMatrix()``
#' @format array of 656 rows and 83 columns
"ex_timematrix"
