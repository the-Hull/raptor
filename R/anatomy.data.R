#' @title Anatomical measurements
#'
#' @description This example dataset includes ROXAS output files (cf. von Arx & Carrer, 2014) with lumen area and cell wall thickness measurements from;
#' i) Norway Spruce (\emph{Picea abies}) from the Loetschental, Switzerland (2007-2013),
#' ii) Siberian larch (\emph{Larix siberica}) from Siberia, Russia (2007-2010),
#' iii) Stone pine (\emph{Pinus cembra}) from the Dolomite mountains, Italy (2007-2010), and
#' iv) Scots pine (\emph{Pinus sylvestris}) from the eastern lowlands, Germany (2007-2010).
#' The image includes multiple annual rings for the years (with a common overlap from 2007 until 2010).
#' @usage anatomy.data
#' @format Provides a \code{\link{data.frame}} with 18838 rows and 7 columns containing data on:
#'
#' [ ,1] \strong{ID} = Site and species id \emph{<factor>}\cr
#' [ ,2] \strong{CID}  = Unique cell id \emph{<integer>}\cr
#' [ ,3] \strong{YEAR} = Year of the ring \emph{<integer>}\cr
#' [ ,4] \strong{CA} = Lumen size (micron) \emph{<numeric>}\cr
#' [ ,5] \strong{XCAL} = X-value of cell center in calibrated coordinate system (micron) \emph{<numeric>}\cr
#' [ ,6] \strong{YCAL} = Y-value of cell center in calibrated coordinate system (micron) \emph{<numeric>}\cr
#' [ ,7] \strong{CWTALL} = Mean thickness of all cell walls (micron) \emph{<numeric>}
#' @name anatomy.data
#' @docType data
#' @usage anatomy.data
#' @references von Arx, G., & Carrer, M. (2014) ROXAS - A new tool to build centuries-long tracheid-lumen chronologies in conifers. Dendrochronologia 32, 290-293.
#' @examples
#' #viewing anatomy data
#' anatomy.data
#' @keywords dataset, wood anatomy, lumean area, cell wall thickness
"anatomy.data"
