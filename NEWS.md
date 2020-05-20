# hpiR 0.3.3

## Bug Fixes

Fixed bug in using weighted linear models caused by forced data filtering.  Have removed the forced data filtering which means that users must no ensure that there are not observations with two transactions in a single period (this mostly occurs when using quarterly or annual periodicity)

