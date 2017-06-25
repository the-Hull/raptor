#' @title Data visualization
#'
#' @description This function uses \code{\link{is.raptor}} files to create a two panel \code{\link{plot}} including; 1) a schematic overview of the ring width sequence and 2) a scheme representing the size and position of cells within a specific year, highlighted in the ring width sequence.
#' @param input an \code{\link{is.raptor}} file.
#' @param year a numerical value specifying the year of interest. Default starts with the first year and plots the other years in sequence.
#' @param interact a logical flag. If \code{\link{TRUE}}, the user can interactively cycle through plots of annual rings (default = \code{\link{FALSE}}).
#' @details This graphical interface aids in exploring the cell position and cell size. The upper \code{\link{plot}} provides and overview of the available years within the \code{\link{data.frame}}. Grey shading indicates the year that is presented in the lower panel. The italic value in the upper panel presents the number of cells within the selected year. The lower panel shows the position of the cells with their unique "CID". XCAL and YCAL positions are standardized to the minimum occurring coordinates. Within the lower panel, the grey boxes represent the cells, derived from the lumen area ("CA") assuming a square shape. When "interact = \code{\link{TRUE}}", \code{\link{readline}} messages will be presented with multiple options to create new \code{\link{plot}} while moving along the years (including selecting the previous, next year or selecting a specific year). Stopping the interact function is done by typing "x". Terminate this function before continuing with other functions.
#' @import graphics
#' grDevices
#' @export
#' @usage plot_cells(input,year=FALSE,interact=FALSE)
#' @examples
#' \dontrun{
#' #plotting example data
#' input<-example.data(species="LOT_PICEA")
#' input<-is.raptor(input, str=TRUE)
#' plot_cells(input, interact=TRUE)
#' #2010
#' #x}
#3.plot.cells----
plot_cells<-function(input,year=FALSE,interact=FALSE){
      opar <- graphics::par()
      requireNamespace('base')
      if(missing(year)){year<-FALSE}
      if(missing(interact)){interact<-FALSE}
      if(missing(year)){year<-FALSE}
      if(missing(interact)){interact<-FALSE}
      if(missing(year)==FALSE){ if(is.numeric(year)==FALSE & year!=FALSE | is.numeric(year)==FALSE & is.na(year)==TRUE)stop('year is not as.numeric')}
      if(missing(interact)==FALSE){ if((interact==TRUE|interact==FALSE)==FALSE)stop('interact is not TRUE/FALSE')}
      if( (is.numeric(year)==FALSE&interact==FALSE)| (missing(year))&missing(interact) | ((year==FALSE)==TRUE&interact==FALSE) | ((year==FALSE)==TRUE&missing(interact)) ){
            for(c in c(1:length(unique(input[,"YEAR"])))){
                  iso<-input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),]
                  if(nrow(iso)==0){next}
                  graphics::layout(matrix(c(1,2,2,2),ncol = 1, byrow = TRUE))
                  graphics::par(mar=c(3,5,3,1))
                  graphics::plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
                  axis(side=1,at=unique(input[,"YEAR"]))
                  abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
                  iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
                  iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
                  graphics::polygon(c(unique(input[,"YEAR"])[c]-0.5,unique(input[,"YEAR"])[c]+0.5,unique(input[,"YEAR"])[c]+0.5,unique(input[,"YEAR"])[c]-0.5),c(0,0,2,2),col="grey")
                  for(c in c(1:length(unique(input[,"YEAR"])))){graphics::text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
                  graphics::par(mar=c(5,5,0,1))
                  graphics::plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
                  nrcells<-nrow(iso)
                  iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
                  for(i in c(1:nrcells)){
                        graphics::polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                                ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
                        graphics::text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}}}
      if(is.numeric(year)==TRUE&interact==FALSE | is.numeric(year)==TRUE & missing(interact)==TRUE){
            if(nrow(input[which(input[,"YEAR"]==year),])==0)stop('year is not present in data.frame')
            iso<-input[which(input[,"YEAR"]==year),]
            graphics::layout(matrix(c(1,2,2,2),ncol=1, byrow = TRUE))
            graphics::par(mar=c(3,5,3,1))
            graphics::plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
            axis(side=1,at=unique(input[,"YEAR"]))
            abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
            iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
            iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
            graphics::polygon(c(year-0.5,year+0.5,year+0.5,year-0.5),c(0,0,2,2),col="grey")
            for(c in c(1:length(unique(input[,"YEAR"])))){graphics::text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
            graphics::par(mar=c(5,5,0,1))
            graphics::plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
            nrcells<-nrow(iso)
            iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
            for(i in c(1:nrcells)){
                  graphics::polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                          ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
                  graphics::text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}}
      if(interact==TRUE){
            if(is.numeric(year)==FALSE|missing(year)==TRUE){year_select<-unique(input[,"YEAR"])[1]}else{
                  if(nrow(input[which(input[,"YEAR"]==year),])==0)stop('year is not present in data.frame')
                  year_select<-year}
            iso<-input[which(input[,"YEAR"]==year_select),]
            graphics::layout(matrix(c(1,2,2,2),ncol = 1, byrow = TRUE))
            graphics::par(mar=c(3,5,3,1))
            graphics::plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
            axis(side=1,at=unique(input[,"YEAR"]))
            abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
            iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
            iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
            graphics::polygon(c(year_select-0.5,year_select+0.5,year_select+0.5,year_select-0.5),c(0,0,2,2),col="grey")
            for(c in c(1:length(unique(input[,"YEAR"])))){graphics::text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
            graphics::par(mar=c(5,5,0,1))
            graphics::plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
            nrcells<-nrow(iso)
            iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
            for(i in c(1:nrcells)){
                  graphics::polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                          ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
                  graphics::text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}
            repeat{
                  repeat{
                        option<-readline("SELECT - next [n] / previous [p] / specific year [yyyy] / end [x] : ")
                        list<-unique(input[,"YEAR"])
                        if( option != "n" & option != "p" & option != "x" & length(list[which(list==option)])== 0 )print('Option is not available')
                        if( option != "n" & option != "p" & option != "x" & length(list[which(list==option)])== 0 ){next}
                        if(option == "n"){
                              if( is.na(list[which(list==year_select)+1])==TRUE )print('Out of bounds')
                              if( is.na(list[which(list==year_select)+1])==TRUE ){next}
                              year_select<-list[which(list==year_select)+1]
                              print(year_select)
                              break}
                        if(option == "p"){
                              if( identical(list[which(list==year_select)-1],integer(0))==TRUE )print('Out of bounds')
                              if( identical(list[which(list==year_select)-1],integer(0))==TRUE ){next}
                              year_select<-list[which(list==year_select)-1]
                              print(year_select)
                              break
                        }
                        if(option=="x"){
                              year_select<-"x"
                              print("End plot.cells")
                              break}
                        if(length(list[which(list==option)])!=0){
                              year_select<-option
                              print(year_select)
                              break}
                  }
                  if(year_select=="x"){break}
                  year_select<-as.numeric(year_select)
                  iso<-input[which(input[,"YEAR"]==year_select),]
                  graphics::layout(matrix(c(1,2,2,2),ncol=1, byrow = TRUE))
                  graphics::par(mar=c(3,5,3,1))
                  graphics::plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
                  axis(side=1,at=unique(input[,"YEAR"]))
                  abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
                  iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
                  iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
                  graphics::polygon(c(year_select-0.5,year_select+0.5,year_select+0.5,year_select-0.5),c(0,0,2,2),col="grey")
                  for(c in c(1:length(unique(input[,"YEAR"])))){graphics::text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
                  graphics::par(mar=c(5,5,0,1))
                  graphics::plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
                  nrcells<-nrow(iso)
                  iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
                  for(i in c(1:nrcells)){
                        graphics::polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                                ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
                        graphics::text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}
            }}
      graphics::par(opar)
}
