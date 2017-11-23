RAPTOR
======================

## Row And Position Tracheid Organizer in R

### Description:

Performs wood cell anatomical data analyses on spatially explicit xylem (tracheids) data sets derived from wood anatomical thin sections. The package includes functions for the visualization, alignment and detection of continuous tracheid radial file (defines as rows) and tracheid position within an annual ring of coniferous species. This package is designed to use elaborate cell output, like provided with ROXAS (von Arx and Carrer, *2014*). The package has been validated for *Picea abies*, *Larix Siberica*, *Pinus cembra* and *Pinus sylvestris*.


-------------

### Important Notes:

This repository contains the release version of RAPTOR for submission to the
*Comprehensive R Archive and Network* (CRAN) and accompanies the manuscript [Peters et al. (2017)](https://authors.elsevier.com/c/1W5pl3-~MPTEVq). A bundled version of the package (i.e. for installation) is found in the file **RAPTOR_1.0.0.tar.gz** (accessible via e.g. *7Zip*).
This document outlines the package installation process and provides initial 
steps for using the package's functions via examples provided in the package documentation.

### Installation:

1. Download the source package from this repository:  [**RAPTOR_1.0.0.tar.gz**](https://github.com/the-Hull/raptor/blob/master/RAPTOR_1.0.0.tar.gz) (Click 'download'); alternatively, download the entire repository in *.zip format using this [link](https://codeload.github.com/the-Hull/raptor/zip/master?token=ALP_Z6J1GbKWPO3MdV9unIsq5lC5L9_cks5ZbOu4wA==), unzip the archive to any location on your computer.
2. Run the following commands in an R session:
      * **install.packages(file_path, repos = NULL, type="source")**
      
      
      file_path must take the following format:
      
      
            - for Windows Systems: "X:/Y/Z/RAPTOR_1.0.0.tar.gz"
            - for UNIX-based Systems: "/home/user/RAPTOR_1.0.0.tar.gz"
            
      * library(RAPTOR)
            
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


Simple examples that illustrate each functions usage are provided at the bottom
of each documentation file. The functions can be tested and explored using the 
complimentary example data sets (cf. [manuscript](https://authors.elsevier.com/c/1W5pl3-~MPTEVq) or execute "**?anatomy.data**" in an 
R session.)

To begin exploring the package, navigate to *Examples* in the documentation of   "**?is.raptor()**":

> #validating example data

> input<-example.data(species="LOT_PICEA")

> input<-is.raptor(input, str=TRUE)

> input


-------------


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



