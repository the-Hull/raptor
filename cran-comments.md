## Overview
This is a resubmission to CRAN with an updated version of RAPTOR.

## Resubmission
* Changed title to title case and omitted superfluous "..in R"
* Changed date format to ISO 8601
* In Description, added auto link to DOI for reference in "Description" field; removed 
      "References" field
* Removed any instance of changing or writing directly to working/home directory
      with requirements of providing character string


## Test environments
* local OS X install, R version 3.4.2
* local Windows 10 install (x86_64, mingw32), R version 3.4.2 (2017-09-28); Could not run on R-devel as Rtools not compatible with R >= 3.4.x
* travis-ci x86_64-pc-linux-gnu (64-bit), R Version 3.4.2   (2017-10-12)

## R CMD check results
* There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies
* All tests on downstream dependencies passed.
