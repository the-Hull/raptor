#' @title Automatic rows and position detection
#'
#' @description Batch.mode applies all functionalities described in \code{\link{is.raptor}}, \code{\link{align}}, \code{\link{first.cell}}, \code{\link{pos.det}} and \code{\link{write.output}}. Multiple input datasets (sample specific) can be provided in a folder and automatically used within this function. Input data should be checked for all requirements described in \code{\link{is.raptor}} and preferably adjusted with \code{\link{align}}.
#' @param location a character string containing the location of input files and where the outputs (.pdf and .txt files) should be stored.
#' @param files a vector of files for the analyses (defaults to all files present in the folder). Text files should agree with all criteria presented in \code{\link{is.raptor}}.
#' @param interact a logical flag. If \code{\link{TRUE}}, the user will have the options to manually assign the degree of rotation for each annual ring. See \code{\link{align}} for rotation options. If \code{\link{FALSE}}, the rotation is optimized automatically using a simple linear regression through all points along the horizontal axis (default = \code{\link{FALSE}}).
#' @param make.plot logical flag indicating whether to make a plot (default =  \code{\link{FALSE}}).
#' @param aligning logical flag indicating whether a second alignment has to be performed based upon the cells detected within \code{\link{first.cell}} (default = \code{\link{TRUE}}).
#' @param list a \code{\link{data.frame}} with the individual "ID" and in sequence the slopes as described in \code{\link{pos.det}}. Cannot be activated when interact = \code{\link{TRUE}}.
#' @param frac.small a numeric value (between 0 and 1) that is multiplied by the average cell lumen size of the ring, determining the minimal threshold used to filter out too small first row tracheids (default = 0.5).
#' @param swe a numeric value that is multiplied by the square-rooted cell lumen area (l) of the target cell and used to determine the width of the rectangular search area which locates the next earlywood cell in the row (default = 0.5).
#' @param sle a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the length of the rectangle search area which locates the next earlywood cell in the row (default = 3).
#' @param ec threshold ratio between the lumen area of two consecutive earlywood cells to determine the end of the earlywood search (default = 1.75). The default setting indicates that the earlywood search ends when the next cell lumen area is at least 1.75 times smaller than the target cell.
#' @param swl a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the width of the rectangle search area which locates the next latewood cell in the row (default = 0.25).
#' @param sll a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the length of the rectangle search area which locates the next earlywood cell in the row (default = 5).
#' @param lc threshold ratio between the lumen area of two consecutive latewood cells to determine the end of the radial file (default = 10). The default setting indicates that the latewood search ends when the next cell lumen area is at least 10 times smaller than the target cell.
#' @param prof.co threshold ratio between the distance to the previous and consecutive cell to determine if the row (or radial file) should be excluded (default = 6).
#' @param max.cells threshold proportion of the maximum number of cells to determine if the radial file has to be excluded (default = 0.6).
#' @param flip logical flag indicating whether to plot the data with earlywood downwards (default; flip = \code{\link{FALSE}}) or upwards (flip = \code{\link{TRUE}}).
#' @param ... see \code{\link{pos.det}} for arguments.
#' @details This function aids in applying all described functions on a large dataset composed of multiple files, including multiple individuals and years. It will generate output graphs as described in \code{\link{is.raptor}}, \code{\link{align}}, \code{\link{first.cell}}, \code{\link{pos.det}} and \code{\link{write.output}} in a .pdf file. Additionally, output text files are written. All output files are exported into the directory specified in the location argument.
#' @import graphics
#' @export
#' @return Plots the detected radial files and writes output according the the
#' \code{\link{write.output}} format.
#' @usage batch.mode(location, files = FALSE,
#'            interact = TRUE, make.plot = TRUE,
#'            aligning = TRUE, frac.small=0.5, swe=0.5, sle=3,
#'            ec=3, swl=0.5, sll=5, lc=10, prof.co=6, max.cells=0.5,
#'            list = FALSE, flip = FALSE)
batch.mode<-function(location,
                     files=FALSE,
                     interact=TRUE,
                     make.plot=TRUE,
                     aligning=TRUE,
                     frac.small=0.5,
                     swe=0.5,
                     sle=3,
                     ec=3,
                     swl=0.5,
                     sll=5,
                     lc=10,
                     prof.co=6,
                     max.cells=0.5,
                     list=FALSE,
                     flip=FALSE){


      if(missing(flip)){flip<-FALSE}
      if(flip!=TRUE&flip!=FALSE)stop('flip need to be TRUE/FALSE')
      if(missing(interact)){interact<-FALSE}
      if(missing(make.plot)){make.plot<-TRUE}
      if(missing(frac.small)){frac.small<-0.5}
      if(missing(aligning)){aligning<-TRUE}
      if(missing(swe)){swe<-0.5}
      if(missing(sle)){sle<-3}
      if(missing(ec)){ec<-3}
      if(missing(swl)){swl<-0.5}
      if(missing(sll)){sle<-5}
      if(missing(lc)){ec<-10}
      if(missing(prof.co)){prof.co<-6}
      if(missing(max.cells)){max.cells<-0.5}
      if(missing(list)){list<-FALSE}

      if(!is.null(location) & !is.character(location)){
            stop("Please provide a character string containing a path to an output folder.")
      } else {

            if(missing(files)){

                  files<-list.files(path = location, pattern = "*.txt")
            }else{
                  for(h in c(1:length(files))){
                        if(length(which(list.files(path = location)==files[h]))==0)stop(paste(files[h],' is not present in setwd()',sep=""))
                  }
            }

            a.<-interact
            b.<-make.plot
            c.<-list
            d.<-frac.small
            e.<-aligning
            f.<-swe
            g.<-sle
            h.<-ec
            i.<-swl
            j.<-sll
            k.<-lc
            l.<-prof.co
            m.<-max.cells

            for(file in c(1:length(files))){
                  file<-1
                  interact<-a.
                  a.<-interact
                  make.plot<-b.
                  b.<-make.plot
                  list<-c.
                  c.<-list
                  frac.small<-d.
                  d.<-frac.small
                  aligning<-e.
                  e.<-aligning
                  swe<-f.
                  f.<-swe
                  sle<-g.
                  g.<-sle
                  ec<-h.
                  h.<-ec
                  swl<-i.
                  i.<-swl
                  sll<-j.
                  j.<-sll
                  lc<-k.
                  k.<-lc
                  prof.co<-l.
                  l.<-prof.co
                  max.cells<-m.
                  m.<-max.cells

                  input<-RAPTOR::is.raptor(read.table(files[file],header=TRUE,sep="\t"))

                  if(a.==TRUE){c.<-FALSE}
                  if(missing(list)){c.<-FALSE}
                  if(interact==FALSE & list!=FALSE){
                        c.<-list[which(list[,1]==unique(input[,"ID"])),2]}

                  sample<-unique(input[,"ID"])
                  if(b.==TRUE&a.==FALSE){pdf(file=paste(sample,".pdf",sep=""),height=210/25.4,width=297/25.4,paper="A4r")}
                  output1<-RAPTOR::align(input,interact=a.,make.plot=b.,list=c.,year=FALSE)
                  if(b.==TRUE&a.==TRUE){pdf(file=paste(sample,".pdf",sep=""),height=210/25.4,width=297/25.4,paper="A4r")}
                  output2<-RAPTOR::first.cell(output1, frac.small = d., yrs = FALSE, make.plot = b.)
                  output3<-RAPTOR::pos.det(output2, swe = f., sle = g., ec = h., swl = i., sll = j., lc = k.,
                                           prof.co = l., max.cells = m., yrs = FALSE , aligning = e. , make.plot = b.)

                  years<-unique(output3[,"YEAR"])
                  for(u in c(1:length(years)) ){
                        data_year<-output3[which(output3[,"YEAR"]==years[u]),]
                        year<-years[u]
                        if(b.==TRUE){

                              if(flip==FALSE){
                                    graphics::plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
                                                   ylim=c(0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01,max(data_year[,"YCAL"],na.rm=TRUE)),
                                                   main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")}
                              if(flip==TRUE){
                                    graphics::plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
                                                   ylim=c(max(data_year[,"YCAL"],na.rm=TRUE),0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01),
                                                   main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")
                              }
                        }
                        data_year[,"SQRLENGTH"] <-sqrt(data_year[,"CA"])
                        nrcells<-nrow(data_year)
                        rows<-max(unique(data_year[,"ROW"])[which(is.na(unique(data_year[,"ROW"]))==FALSE)])
                        col_code<-rep(c("#FFA500","#FF3300","#C71585","#191970","#20B2AA","#00CC33","#006633"),ceiling(rows/7))

                        for(i in c(1:nrcells)){
                              length<-data_year[i,"SQRLENGTH"]/2
                              x     <-data_year[i,"XCAL"]
                              y     <-data_year[i,"YCAL"]
                              x_cor<-c((x-length),(x+length),(x+length),(x-length))
                              y_cor<-c((y+length),(y+length),(y-length),(y-length))

                              if(b.==TRUE){
                                    if(is.na(data_year[i,"ROW"])==TRUE){
                                          graphics::polygon(x_cor,y_cor)
                                    }else{
                                          graphics::polygon(x_cor,y_cor,col=col_code[data_year[i,"ROW"]])
                                    }
                                    if(is.na(data_year[i,"ROW"])==FALSE){
                                          label_point<-data_year[i,"POSITION"]
                                          graphics::text(x,y,label=label_point,col='black',cex=0.5)
                                    }else{
                                          next
                                    }
                              }
                        }
                        first_cells<-data_year[which(data_year[,"POSITION"]==1),]
                        for(i in c(1:nrow(first_cells))){
                              x<-first_cells[i,"XCAL"]
                              y<-first_cells[i,"YCAL"]
                              length<-first_cells[i,"SQRLENGTH"]/1.2
                              label_point<-first_cells[i,"ROW"]
                              if(b.==TRUE){
                                    graphics::text(x,y-length,label=label_point,col='black',cex=0.8,font=2)}
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
                        for(t in c(2:(length(years)-1))){
                              output_all_years<-rbind(output_all_years,get(paste(sample,"-",as.character(years[t]),sep="")))
                        }
                  }
                  if(is.character(location)==TRUE){
                        write.table(output_all_years,file=paste(sample,"_output.txt",sep=""),row.names=FALSE,sep="\t")
                        if(b.==TRUE){dev.off()}
                  }
            }

      }
}
