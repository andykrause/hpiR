
[![Travis-CI Build
Status](https://travis-ci.org/andykrause/hpiR.svg?branch=master)](https://travis-ci.org/andykrause/hpiR)
[![CRAN
status](https://www.r-pkg.org/badges/version/hpiR)](https://cran.r-project.org/package=hpiR)
[![Coverage
status](https://codecov.io/gh/andykrause/hpiR/branch/master/graph/badge.svg)](https://codecov.io/github/andykrause/hpiR?branch=master)

 

<!-- README.md is generated from README.Rmd. Please edit that file -->

# hpiR

![](/images/hpiR.png)

This package intends to simplify and standardize the creation of house
price indexes in R. It also provides a framework for judging the quality
of a given index by testing for predictive accuracy, volatility and
revision. By providing these metrics various index methods (and
estimators) can be accurately compared against each other.

While there are a (ever-increasing) variety of methods and models to use
in house price index creation, this initial version (0.3.0) focuses on
the two most common: repeat sales (transactions) and hedonic price.
Base, robust and weighted estimators are provided when appropriate.
Additionally, a new method using random forests and a post model
interpretability method – partial dependence plots – is also used.

The package also includes a dataset of single family and townhome sales
from the City of Seattle during January 2010 to December 2016 time
period.

Please see the
[vignette](https://github.com/andykrause/hpiR/blob/master/vignettes/introduction.Rmd)
for more information on using the package.

Also, please log issues or pull requests on this [github
page](http://www.github.com/andykrause/hpiR).

## Installation

You can install hpiR from github with:

**Install the released version from CRAN**

``` r
  install.packages("hpiR")
```

**Development version from GitHub:**

``` r
  #install.packages("devtools")
  devtools::install_github("andykrause/hpiR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

  library(hpiR)

  # Load prepared data
  data(ex_rtdata)

  # Create an index
  hpi <- rtIndex(trans_df = ex_rtdata,
                 estimator = 'robust',
                 log_dep = TRUE,
                 trim_model = TRUE,
                 smooth = TRUE)
```
