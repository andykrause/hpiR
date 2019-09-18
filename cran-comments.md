## Update

This is an updated version 0.3.0.  The first submission of this version. New to this version:

* Added a random forest and partial dependency option for estimating house price indexes
* Added ability to filter repeat transactions by repeat time period distance
* Added custom weighting option (user request) to repeat transaction model
* Bug fix: Improved error catching when robust model fails (revert to lm)
* Bug fix: Fixed error when validation set has 0 rows

## Test environments
* local OS X install, R 3.6.0
* ubuntu 16.04.6 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are no known downstream dependencies 


