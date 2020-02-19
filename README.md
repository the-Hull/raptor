# RAPTOR <img src="man/figures/raptor_logo.png" align="right" width = "150"/>

<!-- badges: start -->
[![Lifecycle: mature](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/the-Hull/RAPTOR.svg?branch=master)](https://travis-ci.org/the-Hull/RAPTOR)
<!-- badges: end -->

## Row And Position Tracheid Organizer

### Description:

Performs wood cell anatomical data analyses on spatially explicit xylem (tracheids) data sets derived from wood anatomical thin sections. The package includes functions for the visualization, alignment and detection of continuous tracheid radial file (defines as rows) and tracheid position within an annual ring of coniferous species. This package is designed to use elaborate cell output, like provided with ROXAS (von Arx and Carrer, *2014*). The package has been validated for *Picea abies*, *Larix Siberica*, *Pinus cembra* and *Pinus sylvestris*.


-------------

### Important Notes:

This repository contains the release version of RAPTOR for submission to the
*Comprehensive R Archive and Network* (CRAN) and accompanies the manuscript [Peters et al. (2017)](https://authors.elsevier.com/c/1W5pl3-~MPTEVq). 
This document outlines the package installation process and provides initial 
steps for using the package's functions via examples provided in the package documentation.

### Installation:

A development version of `RAPTOR` can be installed and used via

```r
remotes::install_github("the-Hull/RAPTOR")

library(RAPTOR)

```

The stable CRAN veresion can be installed using

```r

install.packages("the-Hull/RAPTOR")

library(RAPTOR)

```
            
### Usage:

The package's functions are to be executed sequentially (cf. [manuscript](https://authors.elsevier.com/c/1W5pl3-~MPTEVq)):

1. **is.raptor()**
2. **graph.cells()**
3. **align()**
4. **first.cell()**
5. **pos.det()**
6. **write.output()**
7. **batch.mode()** is a convenient 'wrapper' function streamlining steps 1 through 6
8. **anatomy.data** calls complimentary example data sets

Each function is complimented by detailed documentation, which can be accessed by calling
a function's name preceded by a question mark, i.e.:

"**?is.raptor()**".


Simple examples illustrating each function's usage are 
provided at the bottom of each documentation file. 
The functions can be tested and explored using the 
complimentary example data sets (see [manuscript](https://authors.elsevier.com/c/1W5pl3-~MPTEVq)
or execute "**?anatomy.data**" in an R session.)

To begin exploring the package, navigate to *Examples* in the documentation of   "**?is.raptor()**":

```r
#validating example data

input<-example.data(species="LOT_PICEA")

input<-is.raptor(input, str=TRUE)

input

```



### Contributors:


* Richard L. Peters <richardlouispeters3@hotmail.com> [aut, cre, cph]
* Daniel Balanzategui [ctr]                                          
* Alexander G. Hurley [ctr]                                             
* Georg von Arx [ctr]                                                
* Angela Luisa Prendin [ctr]                                         
* Henri E. Cuny [ctr]                                                
* Jesper Bjoerklund [ctr]                                            
* David C. Frank [ctr]                                               
* Patrick Fonti [ths, ctr, fnd]


Please note that the 'RAPTOR' project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

