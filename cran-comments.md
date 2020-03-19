## Overview
This is a package update in response to a kind reminder of CRAN volunteers to fix warnings under new(er) R versions.
Unfortunately the archiving deadline was missed, despite best efforts.

## Resubmission
* updated package documentation to fix warnings



## Test environments
* local Windows 10 install (x86_64, mingw32), R version 3.6.3
* Travis on Ubuntu 16.04.6 LTS:   
  - travis-ci: x86_64-pc-linux-gnu (64-bit) R version 3.6.2 (2017-01-27)
  - travis-ci: x86_64-pc-linux-gnu (64-bit) R Under development (unstable) (2020-02-19 r77831)
* R-hub:  
  - R-hub windows-x86_64-devel (r-devel)
  - R-hub ubuntu-gcc-release (r-release)
  - R-hub fedora-clang-devel (r-devel)


## R CMD check results 
* Local: 
  - 0 errors | 0 warnings | 0 notes 
* Travis:
  - 0 errors | 0 warnings | 0 notes 
* r-hub: 
  - 0 errors | 0 warnings | 1 note  
    + checking CRAN incoming feasibility ... NOTE
    + Maintainer: 'Richard L. Peters <richardlouispeters3@hotmail.com>'

## Downstream dependencies
* All tests on downstream dependencies passed.
