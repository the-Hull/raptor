#' @title Output generation and visualization
#'
#' @description Generating final output graphs and files for the row and position detection. Input data should be provided as produced by \code{\link{pos.det}}.
#' @param input a \code{\link{data.frame}} produced by \code{\link{pos.det}}.
#' @param location an optional character string containing the location where the output .pdf and .txt file should be stored. See \code{\link{setwd}} for formatting. Location should provide \code{\link{as.character}}.
#' @param flip logical flag indicating in which direction to plot the cells, i.e. with earlywood at the bottom (default; flip = \code{\link{FALSE}}) or at the top (flip = \code{\link{TRUE}}).
#' @details Function that aids in graphing the output and writing output tables. The \code{\link{plot}} provides an overview of the detected cells, rows, and the position of the cells within each radial file. The output table provides the standard output table with two additional columns containing the "ROW" number and "POSITION" within the row. This output can provide crucial information that can be used with other packages to generate tracheidograms (cf. de la Cruz and DeSoto, 2015) or link the output to xylogenesis data (cf. Rathgeber et al., 2011).
#' @export
#' @return Plots the detected radial files and writes output according the the \code{\link{is.raptor}} format.
#' @usage write.output(input, location = c("./"), flip = FALSE)
#' @references de la Cruz, M., and DeSoto, L. (2015) tgram: Functions to compute and plot tracheidograms. CRAN: https://cran.r-project.org/web/packages/tgram/tgram.pdf.\cr
#' \cr
#' Rathgeber, C.B.K., Longuetaud, F., Mothe, F., Cuny, H., Le Moguedec, G. (2011) Phenology of wood formation: Data processing, analysis and visualisation using R (package CAVIAR). Dendrochronologia 29, 139-149.
#' @examples
#' \dontrun{
#' #example to write output
#' input<-is.raptor(example.data(species="SIB_LARIX"), str = FALSE)
#' aligned<-align(input)
#' first<-first.cell(aligned, frac.small = 0.5, yrs = FALSE, make.plot = FALSE)
#' output<-pos.det(first, swe = 0.3, sle = 3, ec = 1.5, swl = 0.5, sll = 5, lc = 15,
#'                 prof.co =4, max.cells = 0.5, yrs = FALSE, aligning = FALSE, make.plot = FALSE)
#' sib_larix<-write.output(output)
#'
#' #removing rows which are unsuitable
#' corrections<-data.frame(year=c(2010,2010,2010,2009,2009,2009,2009,2008,2008,
#'                         2008,2008,2008,2008,2007,2007,2007),
#'                         row=c(19,15,9,6,11,14,17,5,6,14,17,24,15,2,8,14))
#' for(i in c(1:nrow(corrections))){
#' sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]  &
#'           sib_larix[,"ROW"]==corrections[i,2] ),"POSITION"]<-rep(NA,length(sib_larix[
#'           which(sib_larix[,"YEAR"]==corrections[i,1]& sib_larix[,"ROW"]==corrections[i,2] ),
#'           "POSITION"]))
#' sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]  &
#'           sib_larix[,"ROW"]==corrections[i,2] ),"ROW"]<-rep(NA,length(sib_larix[
#'           which(sib_larix[,"YEAR"]==corrections[i,1]&sib_larix[,"ROW"]==corrections[i,2] ),
#'           "POSITION"]))}
#' SIB_LARIX<-write.output(sib_larix)
#' for(i in c(1:length(unique(SIB_LARIX[,"YEAR"])))){
#' row_id<-unique(SIB_LARIX[which(SIB_LARIX[,"YEAR"]==unique(SIB_LARIX[,"YEAR"])[i]),"ROW"],
#'                na.rm=TRUE)
#' row_id<-na.omit(row_id[order(row_id)])
#' for(j in c(1:length(row_id))){
#' SIB_LARIX[which(SIB_LARIX[,"YEAR"]==unique(SIB_LARIX[,"YEAR"])[i] &
#'           SIB_LARIX[,"ROW"]==row_id[j]), "ROW"]<-j
#' }}
#' }
write.output<-function(input,location=c("./"),flip=FALSE){

      opar <- graphics::par(no.readonly=T)
      on.exit(graphics::par(opar))

      time_start <- Sys.time()
      outlist <- list()

      #input<-output
      if(missing(flip)){flip<-FALSE}
      if(flip!=TRUE&flip!=FALSE)stop('flip need to be TRUE/FALSE')

      if(missing(location)){location<-FALSE}
      sample<-unique(input[,"ID"])
      if(location!=FALSE){
            setwd(location)
            pdf(file=paste(sample,".pdf",sep=""),height=210/25.4,width=297/25.4,paper="A4r")}

      years<-unique(input[,"YEAR"])
      for(u in c(1:length(years)) ){
            #u<-1
            data_year<-input[which(input[,"YEAR"]==years[u]),]
            year<-years[u]

            if(flip==FALSE){
                  plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
                       ylim=c(0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01,max(data_year[,"YCAL"],na.rm=TRUE)),
                       main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")}
            if(flip==TRUE){
                  plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
                       ylim=c(max(data_year[,"YCAL"],na.rm=TRUE),0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01),
                       main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")
            }

            data_year[,"SQRLENGTH"] <-sqrt(data_year[,"CA"])

            nrcells<-nrow(data_year)
            rows<-max(unique(data_year[,"ROW"])[which(is.na(unique(data_year[,"ROW"]))==FALSE)])
            #col_code<-rep(c("orange","blue","red","green","purple"),ceiling(rows/5))
            #col_code<-rep(c("#FFA500 ","#FF3300","#C71585","#191970 ","#20B2AA ","#00CC33","#006633"),ceiling(rows/7))
            col_code<-rep(c("#FFA500","#FF3300","#C71585","#191970","#20B2AA","#00CC33","#006633"),ceiling(rows/7))
            for(i in c(1:nrcells)){
                  length<-data_year[i,"SQRLENGTH"]/2
                  x     <-data_year[i,"XCAL"]
                  y     <-data_year[i,"YCAL"]
                  x_cor<-c((x-length),(x+length),(x+length),(x-length))
                  y_cor<-c((y+length),(y+length),(y-length),(y-length))

                  if(is.na(data_year[i,"ROW"])==TRUE){
                        polygon(x_cor,y_cor)
                  }else{
                        polygon(x_cor,y_cor,col=col_code[data_year[i,"ROW"]])
                  }
                  if(is.na(data_year[i,"ROW"])==FALSE){
                        label_point<-data_year[i,"POSITION"]
                        text(x,y,label=label_point,col='black',cex=0.5)
                  }else{
                        next
                  }
            }
            first_cells<-data_year[which(data_year[,"POSITION"]==1),]
            for(i in c(1:nrow(first_cells))){
                  x<-first_cells[i,"XCAL"]
                  y<-first_cells[i,"YCAL"]
                  length<-first_cells[i,"SQRLENGTH"]/1.2
                  label_point<-first_cells[i,"ROW"]
                  text(x,y-length,label=label_point,col='black',cex=0.8,font=2)
            }
            a<-which(colnames(data_year)=="SQRLENGTH")
            data_year<-data_year[,-a]
            name<-paste(sample,"-",as.character(year),sep="")
            assign(name,data_year)
      }

      if(length(years)==1){
            output_all_years<-get(paste(sample,"-",as.character(years[1]),sep=""))
      }else{
            output_all_years<-get(paste(sample,"-",as.character(years[1]),sep=""))
            for(t in c(2:(length(years)))){
                  output_all_years<-rbind(output_all_years,get(paste(sample,"-",as.character(years[t]),sep="")))
            }
      }
      if(location!=FALSE){
            write.table(output_all_years,file=paste(sample,"_output.txt",sep=""),row.names=FALSE,sep="\t")
            dev.off()}
      return(output_all_years)
      print(Sys.time() - time_start)
}
