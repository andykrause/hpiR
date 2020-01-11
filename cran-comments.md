## Re-submission

This is the second re-submission of version 0.3.0.  In this re-submisison I have fixed:

* All of the broken github URL found in both the Description and Vignette

### This version contains the following updates

* Added a random forest and partial dependency option for estimating house price indexes
* Added ability to filter repeat transactions by repeat time period distance
* Added custom weighting option (user request) to repeat transaction model
* Added 'pair_id' -- the unique identifier for repeat sales as a standard output in all repeat sales models
* Bug fix: Improved error catching when robust model fails (revert to lm)
* Bug fix: Fixed error when validation set has 0 rows
* Bug fix: Fixed recovery error when Robust Repeat Sales model fails (rtModel)
* Tests: Increased test coverage

## Test environments
* local OS X install, R 3.6.2
* ubuntu 16.04.6 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are no known downstream dependencies 


