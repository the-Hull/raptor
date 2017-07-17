RAPTOR
======================

## Row And Position Tracheid Organizer in R

### Description:

Performs wood cell anatomical data analyses on spatially explicit xylem (tracheids) datasets derived from wood anatomical thin sections. The package includes functions for the visualisation, alignment and detection of continuous tracheid radial file (defines as rows) and tracheid position within an annual ring of coniferous species. This package is designed to use elaborate cell output, like provided with ROXAS (von Arx & Carrer 2014). The package has been validated for Picea abies, Larix Siberica, Pinus cembra and Pinus sylvestris.


-------------

### Important Notes:

This repository contains the pre-release version of RAPTOR for submission to the
*Comprehensive R Archive and Network* (CRAN) and accompanies the manuscript **Fonti et al. 2017)**; a bundled version of the package (i.e. for installation) found in the file **RAPTOR_0.0.9.tar.gz**.
This document outlines the package installation process and provides initial 
steps for using the package's functions via examples provided in the package documentation.

### Installation:

1. Download the source package from this repository (**RAPTOR_0.0.9.tar.gz**)
2. Run the folling command in an R session:
      **install.packages(file_path, repos = NULL, type="source")**
      file_path must take the following format:
            - for Windows Systems: "X:/Y/Z/RAPTOR_0.0.9.tar.gz"
            - for UNIX-based Systems: "/home/user/RAPTOR_0.0.9.tar.gz"
            
### Usage:

The package's functions are to be executed sequentially (cf. manuscript):

1. **is.raptor()**
2. **graph.cells()**
3. **align()**
4. **first.cell()**
5. **pos.det()**
6. **write.output()**
7. (**batch.mode()** is a convenient 'wrapper' function streamlining steps 1 through 6)
8. (**anatomy.data** calls complimentary example data sets)

Each function is complimented by detailed documentation, which can be accessed by calling
a function's name preceded by a question mark, i.e.:

"**?is.raptor()**".

Simple examples that illustrate the each functions usage are provided at the bottom
of each documentation file along; the functions can be tested and explored using the 
complimentary example data sets (cf. manuscript or execute **?anatomy.data in an 
R session.)

-------------


            
            



### Contributors:


Richard L. Peters [aut, cre,cph] (email: richardlouispeters3@hotmail.com), Daniel Balanzategui [ctr], Angela Luisa Prendin [crt], Georg von Arx [aut], Henri E. Cuny [ctr] , Jesper Björklund [ctr], David C. Frank [ctr], Patrick Fonti [ths,ctr.fnd]



