#' @title Species-specific anatomy input
#'
#' @description This function provides a \code{\link{data.frame}} containing standard anatomical measurements from xylem cross-sections images analysed with ROXAS (version 3.0). From the dataset described in \code{\link{anatomy.data}}, multiple input examples can be provided, including; 1) Norway Spruce (\emph{Picea abies}) from the Loetschental, Switzerland, 2) Siberian larch (\emph{Larix siberica}) from Siberia, Russia, 3) Stone pine (\emph{Pinus cembra}) from the Dolomite mountains, Italy, and 4) Scots pine (\emph{Pinus sylvestris}) from the eastern lowlands, Germany.
#' @param species a character string specifying the species from which the example data should be loaded. Valid options include; \emph{Picea abies} = "LOT_PICEA", \emph{Larix siberica} = "SIB_LARIX", \emph{Pinus cembra} = "MOUNT_PINUS", \emph{Pinus sylvestris} = "LOW_PINUS".
#' @details This datasets can be applied as an example to test functions provided in the package. Other examples of ROXAS output can be obtained via: \href{http://www.wsl.ch//roxas/}{WSL - ROXAS}.
#' @export
#' @usage example.data(species = "LOT_PICEA")
#' @return A \code{\link{data.frame}} containing anatomical measurements from the selected species.
#' @examples
#' #loading example data
#' input<-example.data(species="LOT_PICEA")
#' input
#' str(input)
example.data<-function(species="LOT_PICEA"){
      if(species!="LOT_PICEA"&species!="SIB_LARIX"&species!="LOW_PINUS"&species!="MOUNT_PINUS")stop('dataset not present')
      #y<-anatomy.data
      y<-RAPTOR::anatomy.data
      y<-y[which(y[,"ID"]==species),]
      return(y)}
