#' @title Testing and preparing input data
#'
#' @description Testing if the structure of the input matches the requirements for the RAPTOR functions. The input has to be presented in a \code{\link{data.frame}} composed by column with 1) sample id \emph{<\code{\link{as.character}}/\code{\link{as.factor}}>}, 2) tracheid id \emph{<\code{\link{as.integer}}>}, 3) tree-ring year \emph{<\code{\link{as.numeric}}>}, 4) lumen size \emph{<\code{\link{as.numeric}}>}, 5) the x coordinate of the cell \emph{<\code{\link{as.numeric}}>} and 6) the y coordinate of the cell \emph{<\code{\link{as.numeric}}>}. The ring is oriented in order to have the latewood cells on the upper section of the image. For this function either the order or the number of columns have to be respected or the following column names have to be present within the \code{\link{data.frame}}: "ID" = sample id, "CID" = tracheid id, "YEAR" = year, "CA" = lumen size (micron), "XCAL" = x coordinate, "YCAL" = y coordinate. Additionally, at least 50 tracheid's (or cells) have to be present in each year.
#' @param data a \code{\link{data.frame}} where tracheids are ordered in rows and the columns contain the variable sample id, cell id, year, tracheid lumen area, x coordinates and y coordinates. The name of the columns (\code{\link{colnames}}) have to either be named as " ID","CID","YEAR","CA","XCAL" and "YCAL" or properly ordered.
#' @param str a logical flag. If \code{\link{TRUE}} the structure of the data will be printed (default = \code{\link{FALSE}}).
#' @details To prevent errors occurring in the other reported functions, it is advised to run this function for checking the data structure and preparing it for further analyses. This function also installs all required packages (gam, mgcv) to run the RAPTOR functions.
#' @import
#' gam
#' mgcv
#' @export
#' @seealso \code{\link{anatomy.data}}
#' @return A \code{\link{data.frame}} in the appropriate format for other functionalities.
#' @examples
#' #validating example data
#' input<-example.data(species="LOT_PICEA")
#' input<-is.raptor(input, str=TRUE)
#' View(input)
is.raptor<-function(data,str=TRUE){
      list.of.packages <- c("mgcv","gam","base")
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
      if(length(new.packages)) install.packages(new.packages)
      require("gam")
      require("mgcv")
      require("base")

      left = function(string, char){substr(string, 1,char)}
      right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}
      if(ncol(data)==6){
            if((is.character(unique(data[,1]))|is.factor(unique(data[,1])))==FALSE)stop('missing ID, value not as character or factor')
            if((is.numeric(unique(data[,2]))|is.integer(unique(data[,2])))==FALSE)stop('missing CID, value not as integer')
            if(is.numeric(unique(data[,3]))==FALSE)stop('missing YEAR, value not as numeric')
            if(is.numeric(unique(data[,4]))==FALSE)stop('missing CA, value not as numeric')
            if(is.numeric(unique(data[,5]))==FALSE)stop('missing XCAL, value not as numeric')
            if(is.numeric(unique(data[,6]))==FALSE)stop('missing YCAL, value not as numeric')
            colnames(data)<-c("ID","CID","YEAR","CA","XCAL","YCAL")
      }else{
            if(length(which(colnames(data)=="ID"))==0)stop('missing ID')
            if(length(which(colnames(data)=="CID"))==0)stop('missing CID')
            if(length(which(colnames(data)=="YEAR"))==0)stop('missing YEAR')
            if(length(which(colnames(data)=="CA"))==0)stop('missing CA')
            if(length(which(colnames(data)=="XCAL"))==0)stop('missing XCAL')
            if(length(which(colnames(data)=="YCAL"))==0)stop('missing YCAL')
      }
      for(c in c(1:length(unique(data[,"YEAR"])))){if(length(which(data[,"YEAR"]==unique(data[c,"YEAR"])))<50)stop(paste("year ",unique(data[c,"YEAR"])," has less than 50 rows",sep=""))}
      data<-data.frame(data)
      select<-FALSE
      if(missing(str)){str<-FALSE}
      if(str==TRUE){select<-TRUE}
      if(select==TRUE){print(str(data))}
      if(length(which(is.na(data[,"YEAR"])==TRUE))!=0)stop('year column contains NA')
      return(data)}
