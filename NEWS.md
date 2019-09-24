# hpiR 0.3.0

## Major Changes

Added a random forest option for generating house price indexes

## Bug Fixes

Improved error catching when robust model fails (now revert to lm)
Fixed error when accuracy validation set has 0 rows

## Minor Changes

Added ability to filter repeat transactions by repeat time period distance
Added custom weighting option (user request) to repeat transaction model
Increased test coverage across 'xxindex()' and 'plotxx()' functions
