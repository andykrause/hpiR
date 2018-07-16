## Test environments
* ubuntu 14.04.5 (on travis-ci), R 3.5.0
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* New submission

* checking installed package size ... NOTE
  installed size is  9.2Mb
  sub-directories of 1Mb or more:
    data   8.6Mb
    
The 'data' sub-directory is large because the example data is a set of real estate sales
transactions.  House price indexes generally require fairly large datasets to create stable
estimates. The 'data' sub-directory also includes examples of all classes of objects used 
in this package (~6.5mb) of data.  This could be removed if necessary. 

This is my first submission. 

## Downstream dependencies
This is the first version of this package and there are no known downstream dependencies. 


