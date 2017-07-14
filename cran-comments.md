---
title: "cran-comments"
author: "Alexander Hurley"
date: "14 July 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Test environments
* local OS X install, R 3.1.2
* local Windows 10 install, R 3.1.4 beta

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.

## Downstream dependencies
