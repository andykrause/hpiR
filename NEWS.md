# hpiR 0.3.3

## Bug Fixes

Fixed bug in using weighted linear models caused by forced data filtering.  Have removed the forced data filtering which means that users must no ensure that there are not observations with two transactions in a single period (this mostly occurs when using quarterly or annual periodicity)

## Changes

* Updated the 'period_table' attribute resulting from calls to dateToPeriod().  This simplified table structure offers more clarity on the range of dates for each period as well as additional portability for future extensions. 

