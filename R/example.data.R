#' @title Species-specific anatomy input
#'
#' @description This function delivers a \code{\link{data.frame}} containing standard anatomical measurements from images of xylem cross-sections, analysed with ROXAS (version 3.0), From the dataset described in \code{\link{anatomy.data}}, multiple input examples can be provided; 1) Norway Spruce (\emph{Picea abies}) from the Loetschental, Switzerland, 2) Siberian larch (\emph{Larix siberica}) from Siberia, Russia, 3) Stone pine (\emph{Pinus cembra}) from the Dolomite mountains, Italy, and 4) Scots pine (\emph{Pinus sylvestris}) from the eastern lowlands, Germany.
#' @param species a character specifying the species from which the example data should be obtained. Options include \emph{Picea abies} = "LOT_PICEA", \emph{Larix siberica} = "SIB_LARIX", \emph{Pinus cembra} = "MOUNT_PINUS", \emph{Pinus sylvestris} = "LOW_PINUS".
#' @details This datasets can be applied as an example to test different functions. Other examples of ROXAS output can be obtained via: \href{http://www.wsl.ch//roxas/}{WSL - ROXAS}.
#' @export
#' @return A \code{\link{data.frame}} containing anatomical measurements from the selected species.
#' @examples
#' #loading example data
#' input<-example.data(species="LOT_PICEA")
#' View(input)
#' str(input)
example.data<-function(species=FALSE){
      if(species!="LOT_PICEA"&species!="SIB_LARIX"&species!="LOW_PINUS"&species!="MOUNT_PINUS")stop('dataset not present')
      y<-anatomy.data
      y<-y[which(y[,"ID"]==species),]
      return(y)}
