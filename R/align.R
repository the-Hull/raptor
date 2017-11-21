#' @title Data alignment
#'
#' @description Provides a re-aligned \code{\link{is.raptor}} file, where new x- and y-coordinates (XCAL and YCAL values, respectively) are calculated according to a specified rotation. Appropriate alignment is achieved when the first row of earlywood cells are positioned along an approximately horizontal axis and the radial files of tracheids are positioned along a vertical axis.
#' @param input an \code{\link{is.raptor}} file.
#' @param year a numeric vector specifying the year of interest. The default (\code{\link{FALSE}}) applies the function on all years present in the input.
#' @param interact a logical flag. If \code{\link{TRUE}}, the user can manually assign the degree of rotation for each annual ring in an interactive session. The user will have to select among the options (along the horizontal axis, along the vertical axis, manually define the degree of reorientation based on a set of slopes overlaid on the plot). If \code{\link{FALSE}}, the rotation is optimized automatically using a simple linear regression through all points along the horizontal axis (default = \code{\link{FALSE}}).
#' @param list a \code{\link{vector}} of numeric values indicating the rotation slope for each year.
#' @param make.plot logical flag indicating whether to make a plot (default =  \code{\link{FALSE}}).
#' @details Correct alignment of cells/tracheids is important for detecting the first cells and assigning cells to a radial file. This function provides the option to align your cell data (XCAL and YCAL). This function provides and interactive procedure to improve the alignment by presenting a crosshair with slopes with which the sample should be rotated. Proper alignment is obtain when the first cells are approximately aligned on a horizontal line and the upward cells are propagated vertically. Either interactively or with predefined slopes, the user can rotate the sample orientation. An automatic option is also available where a simple linear regression is fitted through all points to adjust the rotation. The plot shows the original position of the cells (black dots) and lines that can be selected to rotate the sample. After correction the new position of the cells is presented (red dots).
#' @export
#' @import graphics
#' grDevices
#' utils
#' stats
#' @return An aligned \code{\link{is.raptor}} file.
#' @usage align(input, year = FALSE, list = FALSE, interact = TRUE, make.plot = TRUE)
#' @examples
#' #rotating example data
#' input<-is.raptor(example.data(species="LOT_PICEA"), str=FALSE)
#' input<-align(input, year=2007, list=FALSE,interact=FALSE, make.plot=TRUE)
align<-function(input,year=FALSE,list=FALSE,interact=TRUE,make.plot=TRUE){



      if(make.plot == TRUE){
            opar <- graphics::par(no.readonly=T)
            on.exit(graphics::par(opar))}
      else { }


      if(missing(year)){year<-FALSE}
      if(missing(interact)){interact<-FALSE}
      if(missing(list)){list<-FALSE}
      if(missing(make.plot)){make.plot<-FALSE}
      if(length(which(is.na(input[,"YEAR"])==TRUE))!=0)stop('year column contains NA')
      if(interact==TRUE & list[1]==FALSE){
            if(year==FALSE){
                  year<-unique(input[,"YEAR"])
            }else{
                  if(length(year)==1){
                        if(length(which(unique(input[,"YEAR"])==year))==0)stop('year is not present in data.frame')
                  }else{
                        if(length(which(unique(input[,"YEAR"])==year))!=length(year))stop('not all years are present in data.frame')
                  }}

            for(i in c(1:length(year))){

                  iso<-input[which(input[,"YEAR"]==year[i]),]
                  iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
                  iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1
                  repeat{
                        layout(matrix(c(1),ncol = 1, byrow = TRUE))
                        par(mar=c(5,5,3,1))
                        plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
                        points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
                        for(c in c(1:10)){
                              graphics::abline(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
                              text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
                        lines(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
                        text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
                        lines(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
                        text(as.numeric(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
                        option<-readline("SELECT - align cells to vertical line [v] / horizontal line [h] / slope [x.xx] / end [x] : ")
                        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
                        colnames(output)<-c("X_CAL","Y_CAL")

                        if(option!="x"&option!="v"&option!="h"& is.na(suppressWarnings(as.numeric(option)))==TRUE){
                              message('Option is not available')
                              next}
                        if(option=="x"){
                              message("end align")
                              break}
                        if(option=="v"){
                              model<-stats::lm(iso[,"XCAL"]~iso[,"YCAL"])
                              y1<-mean(iso[,"YCAL"],na.rm=TRUE)
                              x1<-mean(iso[,"XCAL"],na.rm=TRUE)
                              y2<-max(iso[,"YCAL"],na.rm=TRUE)
                              x2<-summary(model)$coefficient[1]+summary(model)$coefficient[2]*y2
                              r<-y2-y1
                              radians<-c(0:360)*pi/180
                              x_line<-r*sin(radians)+x1
                              y_line<-r*cos(radians)+y1
                              new_angle<-atan( (x2-x1)/ (y2-y1) )*(180/pi)+90
                              x2<-r*sin(new_angle*(pi/180))+x1
                              y2<-r*cos(new_angle*(pi/180))+y1
                              model<-(stats::lm(c(y1,y2)~c(x1,x2)))
                        }
                        if(option=="h"){model<-stats::lm(iso[,"YCAL"]~iso[,"XCAL"])}
                        if(option!="v"&option!="h"){
                              option<-as.numeric(option)
                              model<-stats::lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
                        graphics::abline(model,col="red",lwd=2)
                        y_model<-as.numeric(c(model$coefficients[2]*0+model$coefficients[1],model$coefficients[2]*100+model$coefficients[1]))
                        change_angle<-atan((y_model[2]-y_model[1])/(100-0))*(180/pi)
                        for(p in c(1:length(iso[,"XCAL"]))){
                              r<-sqrt(iso[p,"XCAL"]^2+iso[p,"YCAL"]^2)
                              radians<-c(0:360)*pi/180
                              x_line<-r*sin(radians)
                              y_line<-r*cos(radians)
                              current_angle<-atan(iso[p,"XCAL"]/iso[p,"YCAL"])*(180/pi)
                              new_angle<-current_angle+change_angle
                              x_new<-r*sin(new_angle*(pi/180))
                              y_new<-r*cos(new_angle*(pi/180))
                              points(x_new,y_new,pch=16,col="red",cex=1)
                              output[p,"X_CAL"]<-x_new
                              output[p,"Y_CAL"]<-y_new}
                        check<-readline("SELECT - is alignment correct yes [y] / no [n] : ")
                        if(check!="y"&check!="n"){
                              message('should be Y/N')
                              next}
                        if(check=="n"){next}
                        if(check=="y"){break}
                  }
                  if(option=="x"){break}

                  subtract_Y<-min(output[,"Y_CAL"],na.rm=TRUE)
                  if(subtract_Y<0){
                        output[,"Y_CAL"]<-output[,"Y_CAL"]-subtract_Y
                  }else{
                        output[,"Y_CAL"]<-output[,"Y_CAL"]
                  }
                  subtract_X<-min(output[,"X_CAL"],na.rm=TRUE)
                  if(subtract_X<0){
                        output[,"X_CAL"]<-output[,"X_CAL"]-subtract_X
                  }else{
                        output[,"X_CAL"]<-output[,"X_CAL"]
                  }
                  input[which(input[,"YEAR"]==year[i]),"XCAL"]<-output[,"X_CAL"]
                  input[which(input[,"YEAR"]==year[i]),"YCAL"]<-output[,"Y_CAL"]
            }
      }

      if(interact==TRUE & is.numeric(list)==TRUE)stop('this is not an option')
      if(interact==FALSE){
            if(year[1]==FALSE){
                  year<-unique(input[,"YEAR"])
            }else{
                  if(length(year)==1){
                        if(length(which(unique(input[,"YEAR"])==year))==0)stop('year is not present in data.frame')
                  }else{
                        if(length(which(unique(input[,"YEAR"])==year))!=length(year))stop('not all years are present in data.frame')
                  }}

            if(list[1]==FALSE){
                  for(i in c(1:length(year))){

                        iso<-input[which(input[,"YEAR"]==year[i]),]
                        iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
                        iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1

                        if(make.plot==TRUE){
                              layout(matrix(c(1),ncol = 1, byrow = TRUE))
                              par(mar=c(5,5,3,1))
                              plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
                              points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
                              for(c in c(1:10)){
                                    graphics::abline(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
                                    text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
                              lines(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
                              text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
                              lines(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
                              text(as.numeric(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
                        }

                        option<-'h'
                        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
                        colnames(output)<-c("X_CAL","Y_CAL")
                        if(option=="h"){model<-stats::lm(iso[,"YCAL"]~iso[,"XCAL"])}
                        if(option!="v"&option!="h"){
                              option<-as.numeric(option)
                              model<-stats::lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
                        if(make.plot==TRUE){
                              graphics::abline(model,col="red",lwd=2)}
                        y_model<-as.numeric(c(model$coefficients[2]*0+model$coefficients[1],model$coefficients[2]*100+model$coefficients[1]))
                        change_angle<-atan((y_model[2]-y_model[1])/(100-0))*(180/pi)
                        for(p in c(1:length(iso[,"XCAL"]))){
                              r<-sqrt(iso[p,"XCAL"]^2+iso[p,"YCAL"]^2)
                              radians<-c(0:360)*pi/180
                              x_line<-r*sin(radians)
                              y_line<-r*cos(radians)
                              current_angle<-atan(iso[p,"XCAL"]/iso[p,"YCAL"])*(180/pi)
                              new_angle<-current_angle+change_angle
                              x_new<-r*sin(new_angle*(pi/180))
                              y_new<-r*cos(new_angle*(pi/180))
                              if(make.plot==TRUE){points(x_new,y_new,pch=16,col="red",cex=1)}
                              output[p,"X_CAL"]<-x_new
                              output[p,"Y_CAL"]<-y_new}

                        subtract_Y<-min(output[,"Y_CAL"],na.rm=TRUE)
                        if(subtract_Y<0){
                              output[,"Y_CAL"]<-output[,"Y_CAL"]-subtract_Y
                        }else{
                              output[,"Y_CAL"]<-output[,"Y_CAL"]
                        }
                        subtract_X<-min(output[,"X_CAL"],na.rm=TRUE)
                        if(subtract_X<0){
                              output[,"X_CAL"]<-output[,"X_CAL"]-subtract_X
                        }else{
                              output[,"X_CAL"]<-output[,"X_CAL"]
                        }
                        input[which(input[,"YEAR"]==year[i]),"XCAL"]<-output[,"X_CAL"]
                        input[which(input[,"YEAR"]==year[i]),"YCAL"]<-output[,"Y_CAL"]
                  }
            }else{

                  if(length(list)!=length(year))stop('length of list is not equal to years in data.frame')
                  for(i in c(1:length(year))){

                        iso<-input[which(input[,"YEAR"]==year[i]),]
                        iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
                        iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1

                        if(make.plot==TRUE){
                              layout(matrix(c(1),ncol = 1, byrow = TRUE))
                              par(mar=c(5,5,3,1))
                              plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
                              points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
                              for(c in c(1:10)){
                                    graphics::abline(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
                                    text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(stats::lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
                              lines(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
                              text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+stats::lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
                              lines(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
                              text(as.numeric(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+stats::lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
                        }
                        horiz<-cbind(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],stats::predict(stats::lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2])
                        verti<-cbind(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(stats::predict(stats::lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2])

                        option<-list[i]
                        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
                        colnames(output)<-c("X_CAL","Y_CAL")
                        if(option!="v"&option!="h"){
                              option<-as.numeric(option)
                              model<-stats::lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
                        if(option=="v"){
                              option<-option
                              model<-stats::lm(verti[,2]~verti[,1])
                              if(make.plot==TRUE){ graphics::abline(model,col="red",lwd=2)}
                              model<-stats::lm(iso[,"XCAL"]~iso[,"YCAL"])
                              y1<-mean(iso[,"YCAL"],na.rm=TRUE)
                              x1<-mean(iso[,"XCAL"],na.rm=TRUE)
                              y2<-max(iso[,"YCAL"],na.rm=TRUE)
                              x2<-summary(model)$coefficient[1]+summary(model)$coefficient[2]*y2
                              r<-y2-y1
                              radians<-c(0:360)*pi/180
                              x_line<-r*sin(radians)+x1
                              y_line<-r*cos(radians)+y1
                              new_angle<-atan( (x2-x1)/ (y2-y1) )*(180/pi)+90
                              x2<-r*sin(new_angle*(pi/180))+x1
                              y2<-r*cos(new_angle*(pi/180))+y1
                              model<-(stats::lm(c(y1,y2)~c(x1,x2)))
                        }
                        if(option=="h"){
                              option<-option
                              model<-stats::lm(horiz[,2]~horiz[,1])}

                        if(make.plot==TRUE&option!="v"){
                              graphics::abline(model,col="red",lwd=2)}
                        y_model<-as.numeric(c(model$coefficients[2]*0+model$coefficients[1],model$coefficients[2]*100+model$coefficients[1]))
                        change_angle<-atan((y_model[2]-y_model[1])/(100-0))*(180/pi)
                        for(p in c(1:length(iso[,"XCAL"]))){
                              r<-sqrt(iso[p,"XCAL"]^2+iso[p,"YCAL"]^2)
                              radians<-c(0:360)*pi/180
                              x_line<-r*sin(radians)
                              y_line<-r*cos(radians)
                              current_angle<-atan(iso[p,"XCAL"]/iso[p,"YCAL"])*(180/pi)
                              new_angle<-current_angle+change_angle
                              x_new<-r*sin(new_angle*(pi/180))
                              y_new<-r*cos(new_angle*(pi/180))
                              if(make.plot==TRUE){points(x_new,y_new,pch=16,col="red",cex=1)}
                              output[p,"X_CAL"]<-x_new
                              output[p,"Y_CAL"]<-y_new}

                        subtract_Y<-min(output[,"Y_CAL"],na.rm=TRUE)
                        if(subtract_Y<0){
                              output[,"Y_CAL"]<-output[,"Y_CAL"]-subtract_Y
                        }else{
                              output[,"Y_CAL"]<-output[,"Y_CAL"]
                        }
                        subtract_X<-min(output[,"X_CAL"],na.rm=TRUE)
                        if(subtract_X<0){
                              output[,"X_CAL"]<-output[,"X_CAL"]-subtract_X
                        }else{
                              output[,"X_CAL"]<-output[,"X_CAL"]
                        }
                        input[which(input[,"YEAR"]==year[i]),"XCAL"]<-output[,"X_CAL"]
                        input[which(input[,"YEAR"]==year[i]),"YCAL"]<-output[,"Y_CAL"]
                  }
            }
      }
      return(input)
}
