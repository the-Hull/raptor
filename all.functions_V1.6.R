###FUNCTIONS###
#1.example.data (SETWD in this function to make the dataset operational)----
example.data<-function(species=FALSE, interact = FALSE, path = NULL){


      if(!is.null(path) & is.character(path) & interact == F){
            # interact <- FALSE
            fullpath <- paste0(path, '/', "example.data.txt") #for now you need to set the wd


      } else if(is.null(path) & interact == F){
            message('Searching for example data in current working directory')
            if('example.data.txt' %in% list.files()) {
                  fullpath <- 'example.data.txt'
            }


      } else if(interact == T){
            temp_path <- choose.dir(default = './',
                                    caption = 'Choose Folder Containing Example Data')

            if(is.na(temp_path)) stop('Please choose a directory.')


            fullpath <- paste0(temp_path,
                               '/',
                               'example.data.txt')
      }

      if(species!="LOT_PICEA"&species!="SIB_LARIX"&species!="LOW_PINUS"&species!="MOUNT_PINUS")stop('dataset not present')
      y<-read.table(fullpath,sep="\t",header=TRUE)
      y<-y[which(y[,"ID"]==species),]
      return(y)}


#2.is.raptor----
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
  return(data)}

#3.plot.cells----
plot.cells<-function(input,year=FALSE,interact=FALSE){
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
      layout(matrix(c(1,2,2,2),nc=1, byrow = TRUE))
      par(mar=c(3,5,3,1))
      plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
      axis(side=1,at=unique(input[,"YEAR"]))
      abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
      iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
      iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
      polygon(c(unique(input[,"YEAR"])[c]-0.5,unique(input[,"YEAR"])[c]+0.5,unique(input[,"YEAR"])[c]+0.5,unique(input[,"YEAR"])[c]-0.5),c(0,0,2,2),col="grey")
      for(c in c(1:length(unique(input[,"YEAR"])))){text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
      par(mar=c(5,5,0,1))
      plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
      nrcells<-nrow(iso)
      iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
      for(i in c(1:nrcells)){
        polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
        text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}}}
  if(is.numeric(year)==TRUE&interact==FALSE | is.numeric(year)==TRUE & missing(interact)==TRUE){
    if(nrow(input[which(input[,"YEAR"]==year),])==0)stop('year is not present in data.frame')
    iso<-input[which(input[,"YEAR"]==year),]
    layout(matrix(c(1,2,2,2),nc=1, byrow = TRUE))
    par(mar=c(3,5,3,1))
    plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
    axis(side=1,at=unique(input[,"YEAR"]))
    abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
    iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
    iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
    polygon(c(year-0.5,year+0.5,year+0.5,year-0.5),c(0,0,2,2),col="grey")
    for(c in c(1:length(unique(input[,"YEAR"])))){text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
    par(mar=c(5,5,0,1))
    plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
    nrcells<-nrow(iso)
    iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
    for(i in c(1:nrcells)){
      polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
              ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
      text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}}
  if(interact==TRUE){
    if(is.numeric(year)==FALSE|missing(year)==TRUE){year_select<-unique(input[,"YEAR"])[1]}else{
      if(nrow(input[which(input[,"YEAR"]==year),])==0)stop('year is not present in data.frame')
      year_select<-year}
    iso<-input[which(input[,"YEAR"]==year_select),]
    layout(matrix(c(1,2,2,2),nc=1, byrow = TRUE))
    par(mar=c(3,5,3,1))
    plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
    axis(side=1,at=unique(input[,"YEAR"]))
    abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
    iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
    iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
    polygon(c(year_select-0.5,year_select+0.5,year_select+0.5,year_select-0.5),c(0,0,2,2),col="grey")
    for(c in c(1:length(unique(input[,"YEAR"])))){text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
    par(mar=c(5,5,0,1))
    plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
    nrcells<-nrow(iso)
    iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
    for(i in c(1:nrcells)){
      polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
              ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
      text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}
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
      layout(matrix(c(1,2,2,2),nc=1, byrow = TRUE))
      par(mar=c(3,5,3,1))
      plot(unique(input[,"YEAR"]),rep(1,length(unique(input[,"YEAR"]))),yaxt="n",xaxt="n",ylab="Years",xlab="",col="white",main=paste("plot.cells: ",unique(input[,"ID"]),sep=""),xlim=c(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5))
      axis(side=1,at=unique(input[,"YEAR"]))
      abline(v=seq(min(unique(input[,"YEAR"]),na.rm=TRUE)-0.5,max(unique(input[,"YEAR"]),na.rm=TRUE)+0.5,1))
      iso[,"XCAL"]<-iso[,"XCAL"]-min(iso[,"XCAL"],na.rm=TRUE)
      iso[,"YCAL"]<-iso[,"YCAL"]-min(iso[,"YCAL"],na.rm=TRUE)
      polygon(c(year_select-0.5,year_select+0.5,year_select+0.5,year_select-0.5),c(0,0,2,2),col="grey")
      for(c in c(1:length(unique(input[,"YEAR"])))){text(unique(input[,"YEAR"])[c],1,paste(length(which(is.na(input[which(input[,"YEAR"]==unique(input[,"YEAR"])[c]),][,"CA"])==FALSE)),sep=""),font=3,cex=0.8)}
      par(mar=c(5,5,0,1))
      plot(iso[,"XCAL"],iso[,"YCAL"],ylab="Rel. Y-coordinates (micron)",xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
      nrcells<-nrow(iso)
      iso[,"SQRLENGTH"]<-sqrt(iso[,"CA"])
      for(i in c(1:nrcells)){
        polygon(c((iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"XCAL"]-iso[i,"SQRLENGTH"]/2))
                ,c((iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]+iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2),(iso[i,"YCAL"]-iso[i,"SQRLENGTH"]/2)),col="grey")
        text(iso[i,"XCAL"],iso[i,"YCAL"],iso[i,"CID"],cex=0.8)}
    }}}

#4.align----
align<-function(input,year=FALSE,list=FALSE,interact=TRUE,make.plot=TRUE){

  #year<-2008
  #list<-c("v","v","v","h")
  #interact=FALSE
  #make.plot=FALSE
  #input <- prep(example.data(roxas = TRUE))
  #input<-prep(read.table(files[file],header=TRUE,sep="\t"))

  if(missing(year)){year<-FALSE}
  if(missing(interact)){interact<-FALSE}
  if(missing(list)){list<-FALSE}
  if(missing(make.plot)){make.plot<-FALSE}

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
      #i<-1
      iso<-input[which(input[,"YEAR"]==year[i]),]
      iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
      iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1
      repeat{
        layout(matrix(c(1),nc=1, byrow = TRUE))
        par(mar=c(5,5,3,1))
        plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
        points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
        for(c in c(1:10)){
          abline(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
          text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
        lines(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
        text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
        lines(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
        text(as.numeric(lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
        option<-readline("SELECT - align cells to vertical line [v] / horizontal line [h] / slope [x.xx] / end [x] : ")
        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
        colnames(output)<-c("X_CAL","Y_CAL")
        #still need to tranfors it to a horizontal line
        if(option!="x"&option!="v"&option!="h"& is.na(suppressWarnings(as.numeric(option)))==TRUE){
          print('Option is not available')
          next}
        if(option=="x"){
          print("end align")
          break}
        if(option=="v"){
          model<-lm(iso[,"XCAL"]~iso[,"YCAL"])
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
          model<-(lm(c(y1,y2)~c(x1,x2)))
        }
        if(option=="h"){model<-lm(iso[,"YCAL"]~iso[,"XCAL"])}
        if(option!="v"&option!="h"){
          option<-as.numeric(option)
          model<-lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
        abline(model,col="red",lwd=2)
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
          print('should be Y/N')
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
        #i<-1
        iso<-input[which(input[,"YEAR"]==year[i]),]
        iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
        iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1

        if(make.plot==TRUE){
          layout(matrix(c(1),nc=1, byrow = TRUE))
          par(mar=c(5,5,3,1))
          plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
          points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
          for(c in c(1:10)){
            abline(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
            text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
          lines(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
          text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
          lines(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
          text(as.numeric(lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
        }

        option<-'h'
        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
        colnames(output)<-c("X_CAL","Y_CAL")
        if(option=="h"){model<-lm(iso[,"YCAL"]~iso[,"XCAL"])}
        if(option!="v"&option!="h"){
          option<-as.numeric(option)
          model<-lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
        if(make.plot==TRUE){
          abline(model,col="red",lwd=2)}
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
      #if(is.numeric(list)!=TRUE)stop('list is not numeric')
      if(length(list)!=length(year))stop('length of list is not equal to years in data.frame')
      for(i in c(1:length(year))){
        #i<-1
        iso<-input[which(input[,"YEAR"]==year[i]),]
        iso[,"XCAL"]<-iso[,"XCAL"]-(min(iso[,"XCAL"],na.rm=TRUE))+1
        iso[,"YCAL"]<-iso[,"YCAL"]-(min(iso[,"YCAL"],na.rm=TRUE))+1

        if(make.plot==TRUE){
          layout(matrix(c(1),nc=1, byrow = TRUE))
          par(mar=c(5,5,3,1))
          plot(0,0,ylab="Y-coordinates (micron)",xlab="X-coordinates (micron)",xlim=c(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,max(iso[,"XCAL"],na.rm=TRUE)),ylim=c(0,max(iso[,"YCAL"],na.rm=TRUE)),col="white",main=paste(unique(iso[,"ID"]),unique(iso[,"YEAR"]),sep=" - "))
          points(iso[,"XCAL"],iso[,"YCAL"],pch=16,cex=0.5)
          for(c in c(1:10)){
            abline(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE))),lty=1,col="black")
            text(0-max(iso[,"XCAL"],na.rm=TRUE)*0.01,seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],round(lm(c(seq(from=min(iso[,"YCAL"],na.rm=TRUE),to=max(iso[,"YCAL"],na.rm=TRUE),length.out=10)[c],mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"],na.rm=TRUE)))$coefficients[2],2),cex=0.8,pos=3)}
          lines(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2],lwd=2,col="grey")
          text(max(iso[,"XCAL"],na.rm=TRUE),as.numeric(lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[1]+lm(iso[,"YCAL"]~iso[,"XCAL"])$coefficients[2]*max(iso[,"XCAL"],na.rm=TRUE)),"H",col="grey",pos=3)
          lines(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2],lwd=2,col="grey")
          text(as.numeric(lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[1]+lm(iso[,"XCAL"]~iso[,"YCAL"])$coefficients[2]*max(iso[,"YCAL"],na.rm=TRUE)),max(iso[,"YCAL"],na.rm=TRUE),"V",col="grey",pos=4)
        }
        horiz<-cbind(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),1],cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[order(cbind(iso[,"XCAL"],predict(lm(iso[,"YCAL"]~iso[,"XCAL"])))[,1]),2])
        verti<-cbind(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),1],cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[order(cbind(predict(lm(iso[,"XCAL"]~iso[,"YCAL"])),iso[,"YCAL"])[,1]),2])

        option<-list[i]
        output<-matrix(nrow=length(iso[,"XCAL"]),ncol=2)
        colnames(output)<-c("X_CAL","Y_CAL")
        if(option!="v"&option!="h"){
          option<-as.numeric(option)
          model<-lm(c(mean(iso[,"YCAL"],na.rm=TRUE)-mean(iso[,"XCAL"],na.rm=TRUE)*option,mean(iso[,"YCAL"],na.rm=TRUE))~c(0,mean(iso[,"XCAL"])))}
        if(option=="v"){
          option<-option
          model<-lm(verti[,2]~verti[,1])
          if(make.plot==TRUE){ abline(model,col="red",lwd=2)}
          model<-lm(iso[,"XCAL"]~iso[,"YCAL"])
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
          model<-(lm(c(y1,y2)~c(x1,x2)))
        }
        if(option=="h"){
          option<-option
          model<-lm(horiz[,2]~horiz[,1])}

        if(make.plot==TRUE&option!="v"){
          abline(model,col="red",lwd=2)}
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

#5.first.cell----
first.cell<-function(input,frac.small,yrs,make.plot=TRUE){
  #input<-input
  #frac.small<-0.5
  #yrs<-FALSE
  #make.plot<-TRUE

  if(missing(yrs)){yrs<-unique(input[,"YEAR"])}
  if(missing(frac.small)){frac.small<-0.5}
  if(yrs[1]!="FALSE"&is.numeric(yrs)!=TRUE)stop('year is not present in data.frame')
  if(is.numeric(yrs)==TRUE&length(which(unique(input[,"YEAR"])==yrs))==0)stop('year is not present in data.frame')
  if(yrs[1]=="FALSE"){yrs<-unique(input[,"YEAR"])}

  input[,"ROW"]<-NA
  if(is.numeric(frac.small)!=TRUE)stop('frac.small is not numeric')
  fraction_smallest<-frac.small
  data<-input
  for(u in c(1:length(yrs))){
    #u<-1
    year<-yrs[u]
    data_year<-data[which(data[,"YEAR"]==year),]
    sample<-unique(data_year[,"ID"])
    data_year[,"XCAL"]<-data_year[,"XCAL"]-(min(data_year[,"XCAL"]))+1
    data_year[,"YCAL"]<-data_year[,"YCAL"]-(min(data_year[,"YCAL"]))+1

    if(make.plot==TRUE){
      layout(matrix(c(1),nc=1, byrow = TRUE))
      par(mar=c(5,5,3,1))
      plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",main=paste(sample,as.character(year),sep=" - "),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
    }
    data_year[,"SQRLENGTH"]<-sqrt(data_year[,"CA"])
    data_year[which(is.na(data_year[,"XCAL"])==TRUE),"XCAL"]<-0
    data_year[which(is.na(data_year[,"YCAL"])==TRUE),"YCAL"]<-0
    nrcells<-nrow(data_year)
    if(make.plot==TRUE){
      for(i in c(1:nrcells)){
        length<-data_year[i,"SQRLENGTH"]/2
        x     <-data_year[i,"XCAL"]
        y     <-data_year[i,"YCAL"]
        x_cor<-c((x-length),(x+length),(x+length),(x-length))
        y_cor<-c((y+length),(y+length),(y-length),(y-length))
        polygon(x_cor,y_cor,col="grey")}
    }
    data_year[,"ORIGLENGTH"]<-NA
    for(i in c(1:nrcells)){
      data_year[i,"ORIGLENGTH"]<-sqrt((data_year[i,"YCAL"]^2)+(data_year[i,"XCAL"]^2))
    }
    data_year[,"ROW"]<-NA
    min_value<-(min(data_year[,"ORIGLENGTH"],na.rm=TRUE))
    data_year[which(data_year[,"ORIGLENGTH"]==min_value),"ROW"]<-1
    data_one<-data_year[which(data_year[,"ROW"]==1),]
    length<-data_one[1,"SQRLENGTH"]/2
    x     <-data_one[1,"XCAL"]
    y     <-data_one[1,"YCAL"]
    if(make.plot==TRUE){
      x_cor<-c((x-length),(x+length),(x+length),(x-length))
      y_cor<-c((y+length),(y+length),(y-length),(y-length))
      polygon(x_cor,y_cor,border="green")}
    data_year[,"ENDLENGTH"]<-NA
    maximum                <-max(data_year[,"XCAL"],na.rm=TRUE)
    for(i in c(1:nrcells)){
      data_year[i,"ENDLENGTH"]<-sqrt((data_year[i,"YCAL"]^2)+((maximum-data_year[i,"XCAL"])^2))
    }
    end_cell_data<-data_year[which(data_year[,"ENDLENGTH"]==min(data_year[,"ENDLENGTH"],na.rm=TRUE)),]
    end_cell<-end_cell_data[,"CID"]
    data_last<-data_year[which(data_year[,"CID"]==end_cell),]
    length   <-data_last[1,"SQRLENGTH"]/2
    xmin     <-data_last[1,"XCAL"]+length
    ymin     <-data_last[1,"YCAL"]-length
    ymax    <-data_last[1,"YCAL"]+length
    selected_last_cells<-data_year[which(data_year[,"YCAL"]<ymax & data_year[,"XCAL"]>xmin & data_year[,"YCAL"]>ymin),]
    if(nrow(selected_last_cells)==0){
      "no cell"
    }else{
      end_cell<-selected_last_cells[which(selected_last_cells[,"XCAL"]==max(selected_last_cells[,"XCAL"],na.rm=TRUE)),"CID"]
    }
    for(k in c(1:nrow(data_year))){
      data_last<-data_year[which(data_year[,"CID"]==end_cell),]
      length   <-data_last[1,"SQRLENGTH"]/2
      xmax     <-data_last[1,"XCAL"]+length*1.1
      xmin     <-data_last[1,"XCAL"]-length*1.1
      ymin    <-data_last[1,"YCAL"]-length*1.1
      selected_last_cells<-data_year[which(data_year[,"YCAL"]<ymin & data_year[,"XCAL"]>xmin & data_year[,"XCAL"]<xmax),]
      if(nrow(selected_last_cells)==0){
        break
      }else{
        selected_last_cells[,"LENGTH_LAST"] <-NA
        for(s in c(1:nrow(selected_last_cells))){
          #s<-1
          x1<-data_last[1,"XCAL"]
          y1<-data_last[1,"YCAL"]
          x2<-selected_last_cells[s,"XCAL"]
          y2<-selected_last_cells[s,"YCAL"]
          difference<- sqrt(((sqrt((x2-x1)^2)^2))+((sqrt((y2-y1)^2)^2)))
          selected_last_cells[s,"LENGTH_LAST"]<-difference
        }
        min<-min(selected_last_cells[,"LENGTH_LAST"],na.rm=TRUE)
        selected_last_cells<-selected_last_cells[which(selected_last_cells[,"LENGTH_LAST"]==min),]
        end_cell<-selected_last_cells[,"CID"]
      }
    }
    data_last<-data_year[which(data_year[,"CID"]==end_cell),]
    length<-data_last[1,"SQRLENGTH"]/2
    x     <-data_last[1,"XCAL"]
    y     <-data_last[1,"YCAL"]
    x_cor<-c((x-length),(x+length),(x+length),(x-length))
    y_cor<-c((y+length),(y+length),(y-length),(y-length))
    if(make.plot==TRUE){polygon(x_cor,y_cor,border="red")}
    cut_off     <-max(data_year[,"YCAL"],na.rm=TRUE)-((max(data_year[,"YCAL"],na.rm=TRUE)-min(data_year[,"YCAL"],na.rm=TRUE))/3)
    data_isolate<-data_year[which(data_year[,"YCAL"]<cut_off),]
    if(nrow(data_isolate[which(data_isolate[,"ROW"]==1),])==0){
      data_isolate<-data_year}

    for (t in c(1:(nrcells))){
      data_select<-data_isolate[which(data_isolate[,"ROW"]==t),]
      x     <-data_select[1,"XCAL"]
      y     <-data_select[1,"YCAL"]
      length<-max(data_year[,"SQRLENGTH"],na.rm=TRUE)
      xmin  <-x+length/2
      ymax  <-y+2*length
      isolate<-data_isolate[which(data_isolate[,"XCAL"]>xmin & data_isolate[,"YCAL"]<ymax),]
      isolate<-isolate[order(isolate$XCAL),]
      if(nrow(isolate)==0){
        break
      }else{
        if(nrow(isolate)==0){
          isolate<-data_isolate[which(data_isolate[,"XCAL"]>xmin),]
        }else{
          isolate<-isolate
        }
        if(nrow(isolate)<10){
          isolate<-isolate
        }else{
          isolate<-isolate[c(1:10),]
        }
        isolate[,"DISTANCE"]<-NA
        xorig<-data_select[,"XCAL"]
        yorig<-data_select[,"YCAL"]
        for(p in c(1:nrow(isolate))){
          isolate[p,"DISTANCE"]<-sqrt((2*((isolate[p,"XCAL"]-xorig)^2))+((isolate[p,"YCAL"]-yorig)^2))
        }
        if(nrow(isolate)>3){
          isolate<-isolate[order(isolate$DISTANCE),]
          isolate<-isolate[c(1:3),]
        }else{
          isolate<-isolate[order(isolate$DISTANCE),]
        }
        isolate     <-isolate[order(isolate$XCAL),]
        first_select<-isolate[1,"CID"]
        xstart      <-isolate[1,"XCAL"]-((isolate[1,"SQRLENGTH"]/2)*1)
        xend        <-isolate[1,"XCAL"]+((isolate[1,"SQRLENGTH"]/2)*1)
        ystart      <-isolate[1,"YCAL"]-(isolate[1,"SQRLENGTH"]/2)
        above_cells <-data_isolate[which(data_isolate["XCAL"]>=xstart & data_isolate["XCAL"]<=xend & data_isolate["YCAL"]<=ystart),]
        size        <-data_isolate[which(data_isolate[,"CID"]==first_select),"CA"]/4
        above_cells <-above_cells[which(above_cells[,"CA"]>size),]
        if(nrow(above_cells)==0){
          final_select<-first_select
        }else{
          select_above_cell     <-above_cells[order(above_cells$YCAL),]
          final_select          <-select_above_cell[1,"CID"]
        }
        data_year[which(data_year[,"CID"]==final_select),"ROW"]<-t+1
        data_isolate[which(data_isolate[,"CID"]==final_select),"ROW"]<-t+1
        data_one<-data_year[which(data_year[,"ROW"]==t+1),]
        length<-data_one[1,"SQRLENGTH"]/2
        x     <-data_one[1,"XCAL"]
        y     <-data_one[1,"YCAL"]
        x_cor<-c((x-length/1),(x+length/1),(x+length/1),(x-length/1))
        y_cor<-c((y+length*1),(y+length*1),(y-length*1),(y-length*1))
        if(final_select==end_cell){break}
      }
    }
    cutoff_size <-as.numeric(quantile(data_year[,"CA"])[3])*fraction_smallest
    smaller  <-data_year[which(data_year[,"CA"]<cutoff_size),]
    smaller  <-smaller[which(is.na(smaller[,"ROW"])==FALSE),]
    data_year[which(data_year[,"CA"]<cutoff_size),"ROW"]<-NA
    data_model<-data_year[which(is.na(data_year[,"ROW"])==FALSE),]
    x                      <-data_model[,"XCAL"]
    y                      <-data_model[,"YCAL"]
    input_m                  <-data.frame(cbind(x,y))
    colnames(input_m)        <-c("x","y")

    list.of.packages <- c("mgcv","gam","base")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    require("gam")
    require("mgcv")
    require("base")


    error.test <- try(gam(y ~ s(x),data=input_m),silent =TRUE)
    if("try-error" %in% class(error.test)){print("no gam applied due to low number of rows")
      data_years <-data_year[which(is.na(data_year[,"ROW"])==FALSE),]
      data_years <-data_years[order(data_years$XCAL),]
    }else{

      Model                  <-gam(y ~ s(x),data=input_m)
      x                      <-c(0:max(data_year[,"XCAL"],na.rm=TRUE))
      predict                <-predict(Model,newdata=data.frame(x))
      data_isolate[,"BOUNDARY_UPP"] <-NA
      data_isolate[,"BOUNDARY_DOWN"]<-NA
      x                        <-data_isolate[,"XCAL"]
      predict                  <-predict(Model,newdata=data.frame(x))+(max(data_year[,"SQRLENGTH"],na.rm=TRUE)*1)
      data_isolate[,"BOUNDARY_UPP"]<-predict
      x                        <-data_isolate[,"XCAL"]
      predict                  <-predict(Model,newdata=data.frame(x))-(max(data_year[,"SQRLENGTH"],na.rm=TRUE)*1)
      data_isolate[,"BOUNDARY_DOWN"]<-predict
      data_isolate_select      <-data_isolate[which(data_isolate[,"YCAL"]<data_isolate[,"BOUNDARY_UPP"] & data_isolate[,"YCAL"]>data_isolate[,"BOUNDARY_DOWN"]),]
      selected_CID             <-data_isolate_select[,"CID"]

      for(j in c(1:length(selected_CID))){
        data_selected <-data_year[which(data_year[,"CID"]==selected_CID[j]),]
        xstart      <-data_selected[1,"XCAL"]-((data_selected[1,"SQRLENGTH"]/2)*1)
        xend        <-data_selected[1,"XCAL"]+((data_selected[1,"SQRLENGTH"]/2)*1)
        ystart      <-data_selected[1,"YCAL"]-(data_selected[1,"SQRLENGTH"]/2)
        above_cells <-data_isolate[which(data_isolate["XCAL"]>=xstart & data_isolate["XCAL"]<=xend & data_isolate["YCAL"]<=ystart),]
        size        <-data_selected[,"CA"]/4
        above_cells <-above_cells[which(above_cells[,"CA"]>size),]

        if(nrow(above_cells)==0){
          data_year[which(data_year[,"CID"]==selected_CID[j]),"ROW"]<-999
        }else{
          select_above_cell     <-above_cells[order(above_cells$YCAL),]
          data_year[which(data_year[,"CID"]==select_above_cell[1,"CID"]),"ROW"]<-999
        }
      }
      adding_data<-data_year[which(data_year[,"ROW"]==999),]
      for(c in c(1:nrow(adding_data))){
        length<-adding_data[c,"SQRLENGTH"]/2
        x     <-adding_data[c,"XCAL"]
        y     <-adding_data[c,"YCAL"]
        x_cor<-c((x-length),(x+length),(x+length),(x-length))
        y_cor<-c((y+length),(y+length),(y-length),(y-length))
        #polygon(x_cor,y_cor,border="blue")
      }
      data_year[which(data_year[,"CA"]<cutoff_size),"ROW"]<-NA
      data_years <-data_year[which(is.na(data_year[,"ROW"])==FALSE),]
      data_years <-data_years[order(data_years$XCAL),]

      for (p in c(1:nrow(data_years))){
        data_year[which(data_year[,"CID"]==data_years[p,"CID"]),"ROW"]<-p
      }
      for (q in c(1:nrow(data_year))){
        #q<-1
        data_one<-data_year[which(data_year[,"ROW"]==q),]
        length<-data_one[1,"SQRLENGTH"]/2
        x     <-data_one[1,"XCAL"]
        y     <-data_one[1,"YCAL"]
        x_cor<-c((x-length/1),(x+length/1),(x+length/1),(x-length/1))
        y_cor<-c((y+length*1),(y+length*1),(y-length*1),(y-length*1))
      }
      smaller  <-data_year[which(data_year[,"CA"]<cutoff_size),]
      data_model             <-data_year[which(is.na(data_year[,"ROW"])==FALSE),]
      x                      <-data_model[,"XCAL"]
      y                      <-data_model[,"YCAL"]
      input_m                  <-data.frame(cbind(x,y))
      colnames(input_m)        <-c("x","y")
      Model                  <-gam(y ~ s(x),data=input_m)
      x                      <-c(1:(max(data_model[,"XCAL"],na.rm=TRUE)))
      predict                <-predict(Model,newdata=data.frame(x))
      x                      <-(data_model[,"XCAL"])
      predict                <-predict(Model,newdata=data.frame(x))
      predict_upper          <-predict+max(data_year[,"SQRLENGTH"],na.rm=TRUE)*1
      removal                <-cbind(data_model[,"CID"],data_model[,"YCAL"],predict_upper)
      removed_CID            <-removal[which(removal[,2]>removal[,3]),1]
      data_year[which(data_year[,"CID"]%in% removed_CID),"ROW"]<-NA
      row_code<-sort(unique(data_year[,"ROW",]))
      for(r in c(1:length(row_code))){
        #r<-1
        data_year[which(data_year["ROW"]==row_code[r]),"ROW"]<-r
      }

    }

    graph<-data_years[which(is.na(data_years[,"ROW"])==FALSE),]
    if(nrow(graph)==0){print("no cells detected")
      break}
    for(c in c(1:nrow(graph))){
      #c<-1
      length<-graph[c,"SQRLENGTH"]/2
      x     <-graph[c,"XCAL"]
      y     <-graph[c,"YCAL"]
      x_cor<-c((x-length),(x+length),(x+length),(x-length))
      y_cor<-c((y+length),(y+length),(y-length),(y-length))
      if(make.plot==TRUE){
        text(x,y,c,cex=0.8)}
    }
    if(u==1){output<-data_year}else{output<-rbind(output,data_year)}
  }

  col_nr<-ncol(output)
  output<-output[,c(-col_nr+2,-col_nr+1,-col_nr)]
  return(output)
}

#6.pos.det----
pos.det<-function(input,swe,sle,ec,swl,sll,lc,prof.co,max.cells,aligning=TRUE,list=FALSE,yrs=FALSE,make.plot=TRUE){

  #input<-first
  #swe = 1.2
  #sle = 3
  #ec = 1.75
  #swl = 0.25
  #sll = 5
  #lc = 5
  #prof.co = 6
  #max.cells = 0.5
  #yrs = 2009
  #aligning = FALSE
  #make.plot = TRUE
  #list<-cbind(c(2008,2008,2008),c(1,2,3))
  #list<-FALSE
  #yrs<-FALSE

  if(missing(list)==TRUE){list<-FALSE}else{
    list<-list
  }
  if(missing(aligning)==TRUE){turn<-TRUE}else{
    if(aligning!=TRUE&aligning!=FALSE)stop('aligning is not TRUE/FALSE')
    if(aligning==FALSE){turn<-FALSE}else{
      turn<-TRUE
    }
  }
  if(missing(make.plot)==TRUE){make.plot<-FALSE}else{
    if(make.plot!=TRUE&make.plot!=FALSE)stop('make.plot is not TRUE/FALSE')
  }
  if(missing(max.cells)==TRUE){percentage_max_nr_cells<-0.6}else{
    if(is.numeric(max.cells)==FALSE)stop('max.cells is not numeric')
    percentage_max_nr_cells<-max.cells
  }
  if(missing(prof.co)==TRUE){profile_cutoff<-2}else{
    if(is.numeric(prof.co)==FALSE)stop('prof.co is not numeric')
    profile_cutoff<-prof.co
  }
  #if(missing(gam.wd)==TRUE){
  gam_width<-0.5
  #}else{
  #  if(is.numeric(gam.wd)==FALSE)stop('gam.wd is not numeric')
  #  gam_width<-gam.wd
  #}
  if(missing(lc)==TRUE){latewood_cutoff<-10}else{
    if(is.numeric(lc)==FALSE)stop('lc is not numeric')
    latewood_cutoff<-lc
  }
  if(missing(sll)==TRUE){search_length_latewood<-5}else{
    if(is.numeric(sll)==FALSE)stop('sll is not numeric')
    search_length_latewood<-sll
  }
  if(missing(swl)==TRUE){search_width_latewood<-0.5}else{
    if(is.numeric(swl)==FALSE)stop('swl is not numeric')
    search_width_latewood<-swl
  }
  if(missing(ec)==TRUE){earlywood_cutoff<-3}else{
    if(is.numeric(ec)==FALSE)stop('ec is not numeric')
    earlywood_cutoff<-ec
  }
  if(missing(sle)==TRUE){search_length_earlywood<-3}else{
    if(is.numeric(sle)==FALSE)stop('sle is not numeric')
    search_length_earlywood<-sle
  }
  if(missing(swe)==TRUE){search_width_earlywood<-0.5}else{
    if(is.numeric(swe)==FALSE)stop('swe is not numeric')
    search_width_earlywood<-swe
  }

  if(missing(yrs)){yrs<-unique(input[,"YEAR"])}
  if(yrs[1]!="FALSE"&is.numeric(yrs)!=TRUE)stop('year is not present in data.frame')
  if(is.numeric(yrs)==TRUE&length(which(unique(input[,"YEAR"])==yrs))==0)stop('year is not present in data.frame')
  if(yrs[1]=="FALSE"){yrs<-unique(input[,"YEAR"])}

  #ROW IDENTIFICATION EARLYWOOD----
  input[,"POSITION"]<-NA
  input[,"MARKER"]<-NA
  for(u in c(1:length(yrs))){
    #u<-1
    year<-yrs[u]
    data_year<-input[which(input[,"YEAR"]==year),]
    data_year[,"XCAL"]<-data_year[,"XCAL"]-(min(data_year[,"XCAL"]))+1
    data_year[,"YCAL"]<-data_year[,"YCAL"]-(min(data_year[,"YCAL"]))+1
    sample<-unique(data_year[,"ID"])
    data_year[,"SQRLENGTH"]<-sqrt(data_year[,"CA"])

    if(turn==TRUE){
      rotation<-function(x_data,y_data,x_first,y_first){
        max_y<-max(y_data,na.rm=TRUE)
        max_x<-max(x_data,na.rm=TRUE)
        output<-matrix(nrow=length(x_data),ncol=2)
        colnames(output)<-c("X_CAL","Y_CAL")
        model<-lm(y_first~x_first)
        b<-model$coefficients[1]
        a<-model$coefficients[2]
        y_model<-as.numeric(c(a*0+b,a*100+b))
        change_angle<-atan((y_model[2]-y_model[1])/(100-0))*(180/pi)
        for(p in c(1:length(x_data))){
          r<-sqrt(x_data[p]^2+y_data[p]^2)
          radians<-c(0:360)*pi/180
          x_line<-r*sin(radians)
          y_line<-r*cos(radians)
          current_angle<-atan(x_data[p]/y_data[p])*(180/pi)
          new_angle<-current_angle+change_angle
          x_new<-r*sin(new_angle*(pi/180))
          y_new<-r*cos(new_angle*(pi/180))
          output[p,"X_CAL"]<-x_new
          output[p,"Y_CAL"]<-y_new
        }
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
        return(output)
      }
      XCAL_ORIG<-data_year[,"XCAL"]
      YCAL_ORIG<-data_year[,"YCAL"]
      data_year<-cbind(data_year,XCAL_ORIG,YCAL_ORIG)

      input_r<-data_year
      x_first<-as.numeric(as.character(input_r[which(is.na(input_r[,"ROW"])==FALSE),"XCAL"]))
      y_first<-as.numeric(as.character(input_r[which(is.na(input_r[,"ROW"])==FALSE),"YCAL"]))
      x_data <-data_year[,"XCAL"]
      y_data <-data_year[,"YCAL"]

      output_new<-rotation(x_data,y_data,x_first,y_first)
      data_year[,"XCAL"]<-output_new[,"X_CAL"]
      data_year[,"YCAL"]<-output_new[,"Y_CAL"]
    }

    if(length(which(colnames(data_year)=="ROW"))==1){
      data_year<-data_year
    }else{
      list_y<-list[which(list[,1]==year),]
      if(missing(list_y))stop('no list present or ROW values')
      if(is.numeric(list_y)==FALSE)stop('list is not numeric')
      data_year[,"ROW"]<-NA
      for(a in c(1:length(list_y))){
        data_year[which(data_year[,"CID"]==list_y[a]),"ROW"]<-a
      }
    }

    if(make.plot==TRUE){
      layout(matrix(c(1),nc=1, byrow = TRUE))
      par(mar=c(5,5,3,1))
      plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",main=paste(sample,as.character(year),sep=" - "),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
      nrcells<-nrow(data_year)
      if(make.plot==TRUE){
        for(i in c(1:nrcells)){
          length<-data_year[i,"SQRLENGTH"]/2
          x     <-data_year[i,"XCAL"]
          y     <-data_year[i,"YCAL"]
          x_cor<-c((x-length),(x+length),(x+length),(x-length))
          y_cor<-c((y+length),(y+length),(y-length),(y-length))
          polygon(x_cor,y_cor,col="grey")
          #text(x,y,data_year[i,"ROW"])
        }
      }
    }

    for(c in c(1:max(data_year[,"ROW"],na.rm=TRUE))){
      CID_row<-data_year[which(data_year[,"ROW"]==c),"CID"]
      data_year[which(data_year[,"CID"]==CID_row),"POSITION"]<-1
      nrcells<-nrow(data_year)
      for(k in c(1:nrow(data_year))){
        interest<-data_year[which(data_year[,"ROW"]==c & data_year[,"POSITION"]==k),]
        length<- data_year[which(data_year[,"ROW"]==c & data_year[,"POSITION"]==k),"SQRLENGTH"] #we take the length from the first cell as this is most likely the biggest cell
        xmin<- interest[,"XCAL"]-length*search_width_earlywood
        xmax<- interest[,"XCAL"]+length*search_width_earlywood
        ymin<- interest[,"YCAL"]+(length/2)
        ymax<- interest[,"YCAL"]+length*search_length_earlywood
        x<-c(xmin,xmin,xmax,xmax)
        y<-c(ymin,ymax,ymax,ymin)
        if(make.plot==TRUE){polygon(x,y,lty=2,border="orange")}
        selected_cells<-data_year[which(data_year[,"XCAL"]>=xmin & data_year[,"XCAL"]<=xmax & data_year[,"YCAL"]>=ymin & data_year[,"YCAL"]<=ymax),]
        if(nrow(selected_cells)==0){
          data_year[which(data_year[,"ROW"]==c & data_year[,"POSITION"]==k),"MARKER"]<-1
          MARK1<-data_year[which(data_year$MARKER==1),]
          if(make.plot==TRUE){
          points(MARK1$XCAL,MARK1$YCAL, pch=16, col="orange",cex=1.3)
          points(MARK1$XCAL,MARK1$YCAL, pch=1, col="black",cex=1.3)
          }
          break
        }else{
          selected_cells[,"LENGTH"]<-NA
          ax<-interest[,"XCAL"]
          ay<-interest[,"YCAL"]
          cut_off_size<-interest[,"CA"]/earlywood_cutoff
          selected_cells<-selected_cells[which(selected_cells[,"CA"]>=cut_off_size),]
          if(nrow(selected_cells)==0){
            if(is.na(data_year[which(data_year[,"ROW"]==c & data_year[,"POSITION"]==k),"MARKER"])) {
              data_year[which(data_year[,"ROW"]==c & data_year[,"POSITION"]==k),"MARKER"]<-2
              MARK2<-data_year[which(data_year$MARKER==2),]
              if(make.plot==TRUE){
              points(MARK2$XCAL,MARK2$YCAL, pch=16, col="orange",cex=1.3)
              points(MARK2$XCAL,MARK2$YCAL, pch=1, col="black",cex=1.3)
              }
            }
            break
          }else{
            for (z in c(1:nrow(selected_cells))){
              #z<-2
              bx<-selected_cells[z,"XCAL"]
              by<-selected_cells[z,"YCAL"]
              selected_cells[z,"LENGTH"]<-(sqrt(2*((sqrt((bx-ax)^2)))^2+(sqrt((by-ay)^2))^2))
            }
            size<-interest[1,"CA"]
            selected_cells                <-selected_cells[which(selected_cells[,"CA"]>(size/3)),]
            selected_cells                <-selected_cells[which(is.na(selected_cells[,"ROW"])==TRUE),]
            CID_new<-selected_cells[order(selected_cells$LENGTH),]
            CID_new<-CID_new[1,"CID"]
            data_year[which(data_year[,"CID"]==CID_new),"POSITION"]<-k+1
            data_year[which(data_year[,"CID"]==CID_new),"ROW"]     <-c
            data_one<-data_year[which(data_year[,"CID"]==CID_new),]
            length<-data_one[1,"SQRLENGTH"]/2
            x     <-data_one[1,"XCAL"]
            y     <-data_one[1,"YCAL"]
            x_cor<-c((x-length/1),(x+length/1),(x+length/1),(x-length/1))
            y_cor<-c((y+length*1),(y+length*1),(y-length*1),(y-length*1))
          }
        }
      }
    }

    backup<-data_year
    data_year<-backup

    #ROW IDENTIFICATION LATEWOOD----
    for (r in c(1:max(data_year[,"ROW"],na.rm=TRUE))){
      data_select<-data_year[which(data_year[,"ROW"]==r),]
      max_width  <-max(data_select[,"SQRLENGTH"],na.rm=TRUE)
      mean_length<-mean(data_select[,"SQRLENGTH"],na.rm=TRUE)
      last_ID    <-data_select[which(data_select[,"POSITION"]==max(data_select[,"POSITION"],na.rm=TRUE)),]

      for (g in c(1:nrow(data_year))){
        run          <-last_ID[,"POSITION"]+g-1
        data_select  <-data_year[which(data_year[,"ROW"]==r),]
        data_interest<-data_select[which(data_select[,"POSITION"]==run),]
        min_length   <-data_interest[,"SQRLENGTH"]
        xmin         <-data_interest[,"XCAL"]-mean_length*search_width_latewood
        xmax         <-data_interest[,"XCAL"]+mean_length*search_width_latewood
        ymin         <-data_interest[,"YCAL"]+(min_length/2)
        ymax         <-data_interest[,"YCAL"]+mean_length*search_length_latewood
        x<-c(xmin,xmin,xmax,xmax)
        y<-c(ymin,ymax,ymax,ymin)
        if(make.plot==TRUE){polygon(x,y,lty=2,border="red")}
        selected_cells<-data_year[which(data_year[,"XCAL"]>=xmin & data_year[,"XCAL"]<=xmax & data_year[,"YCAL"]>=ymin & data_year[,"YCAL"]<=ymax),]
        if(nrow(selected_cells)==0){
          data_year[which(data_year[,"POSITION"]==run & data_year[,"ROW"]==r ),"MARKER"]<-3
          MARK3<-data_year[which(data_year$MARKER==3),]
          if(make.plot==TRUE){
          points(MARK3$XCAL,MARK3$YCAL, pch=16, col="red",cex=1.3)
          points(MARK3$XCAL,MARK3$YCAL, pch=1, col="black",cex=1.3)
          }
          break
        }else{
          selected_cells[,"LENGTH"]<-NA
          ax         <-data_interest[,"XCAL"]
          ay         <-data_interest[,"YCAL"]

          #here we exclude cells that are 3 times smaller than the original ones
          cut_off_size<-data_interest[,"CA"]/latewood_cutoff
          selected_cells<-selected_cells[which(selected_cells[,"CA"]>=cut_off_size),]
          if(nrow(selected_cells)==0){
            data_year[which(data_year[,"POSITION"]==run & data_year[,"ROW"]==r ),"MARKER"]<-4
            MARK4<-data_year[which(data_year$MARKER==4),]
            if(make.plot==TRUE){
            points(MARK4$XCAL,MARK4$YCAL, pch=16, col="red",cex=1.3)
            points(MARK4$XCAL,MARK4$YCAL, pch=1, col="black",cex=1.3)
            }
            break
          }else{
            for (z in c(1:nrow(selected_cells))){
              bx<-selected_cells[z,"XCAL"]
              by<-selected_cells[z,"YCAL"]
              selected_cells[z,"LENGTH"]<-(sqrt(5*((sqrt((bx-ax)^2)))^2+(sqrt((by-ay)^2))^2))
            }
            selected_cells                <-selected_cells[which(is.na(selected_cells[,"ROW"])==TRUE),]
            CID_new<-selected_cells[order(selected_cells$LENGTH),]
            CID_new<-CID_new[1,"CID"]
            data_year[which(data_year[,"CID"]==CID_new),"POSITION"]<-last_ID[,"POSITION"]+g
            data_year[which(data_year[,"CID"]==CID_new),"ROW"]     <-r
            data_one<-data_year[which(data_year[,"CID"]==CID_new),]
            length<-data_one[1,"SQRLENGTH"]/2
            x     <-data_one[1,"XCAL"]
            y     <-data_one[1,"YCAL"]
            x_cor<-c((x-length/1),(x+length/1),(x+length/1),(x-length/1))
            y_cor<-c((y+length*1),(y+length*1),(y-length*1),(y-length*1))
          }
        }
      }
    }

    backup<-data_year
    data_year<-backup

    list.of.packages <- c("mgcv","gam","base")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    require("gam")
    require("mgcv")
    require("base")

    #FINAL GAMM FIT FOR THE CELL POSITION (WITH PADDING OF 0.5 micron)----
    #error.test <- try(gam(y ~ s(x),data=input_m),silent =TRUE)
    #if("try-error" %in% class(error.test)){print("no gam applied due to low number of rows")
    #  data_years <-data_year[which(is.na(data_year[,"ROW"])==FALSE),]
    #  data_years <-data_years[order(data_years$XCAL),]

    for (r in c(1:max(data_year[,"ROW"],na.rm=TRUE))){
      data_select<-data_year[which(data_year[,"ROW"]==r),]
      if(length(data_select[,"POSITION"])<=3){
        next
      }else{
        x                      <-data_select[,"XCAL"]
        y                      <-data_select[,"YCAL"]
        x_new                  <-(c(x,x,x))#padding the cells
        y_new                  <-(c(y,(y-0.5),(y+0.5)))#padding the cells
        input_m                  <-data.frame(cbind(y_new,x_new))
        colnames(input_m)        <-c("y","x")
        Model                  <-gam(x ~ s(y),data=input_m)
        y                      <-c(min(data_select[,"YCAL"],na.rm=TRUE):max(data_select[,"YCAL"],na.rm=TRUE))
        predict                <-predict(Model,newdata=data.frame(y))
        mean_width             <-mean(data_select[,"SQRLENGTH"],na.rm=TRUE)
        #if(make.plot==TRUE){#lines((predict),y,lty=1)
        #  lines((as.numeric(predict)+mean_width*gam_width),y,lty=2)
        #  lines((as.numeric(predict)-mean_width*gam_width),y,lty=2)
        #}
        data_na                <-data_year[which(is.na(data_year[,"ROW"])==TRUE),]
        y                      <-data_na[,"YCAL"]
        value                  <-predict(Model,newdata=data.frame(y))
        upper                  <-value+mean_width*gam_width
        lower                  <-value-mean_width*gam_width
        selection_cells        <-cbind(data_na[,"CID"],data_na[,"XCAL"],upper,lower)
        selection_CID          <-selection_cells[which(selection_cells[,2]>=selection_cells[,"lower"] & selection_cells[,2]<=selection_cells[,"upper"]),1]
        data_year[which(data_year[,"CID"]%in% selection_CID),"ROW"]<-r
      }
    }

    backup<-data_year
    data_year<-backup

    #mistake when little rows are present
    row_code<-sort(unique(data_year[,"ROW",]))
    for(j in c(1:length(row_code))){
      #j<-1
      selected_data                  <-data_year[which(data_year[,"ROW"]==row_code[j]),]
      selected_data                  <-selected_data[order(selected_data$YCAL),]
      selected_data[,"DISTANCE"]     <-NA

      for(k in c(1:nrow(selected_data))){
        one<-selected_data[k,]
        two<-selected_data[(k+1),]
        #if(nrow(two)==0){
        if(is.na(two$CID)){
          data_year[which(data_year$CID==one$CID),"MARKER"]<-5
          MARK5<-data_year[which(data_year$MARKER==5),]
          if(make.plot==TRUE){
          points(MARK5$XCAL,MARK5$YCAL, pch=15, col="red",cex=1.3)
          points(MARK5$XCAL,MARK5$YCAL, pch=0, col="black",cex=1.3)
          }
          break
        }else{
          selected_data[k+1,"DISTANCE"]<-sqrt(((sqrt((one[,"XCAL"]-two[,"XCAL"])^2))^2)+((sqrt((one[,"YCAL"]-two[,"YCAL"])^2))^2))
        }
        selected_data[1,"DISTANCE"] <-0
      }
      selected_data[,"TIMES"]<-NA
      for(k in c(2:nrow(selected_data))){
        #k<-2
        one<-selected_data[k,"DISTANCE"]
        two<-selected_data[(k+1),"DISTANCE"]
        if(length(two)==0){
          break
        }else{
          selected_data[k,"TIMES"]<- two/one
        }
      }

      if(suppressWarnings(max(selected_data[,"TIMES"],na.rm=TRUE))=="-Inf"){
        data_year[which(data_year[,"ROW"]==row_code[j]),"ROW"]<-NA
      }else{
        if(max(selected_data[,"TIMES"],na.rm=TRUE)>profile_cutoff){
          data_year[which(data_year[,"ROW"]==row_code[j]),"MARKER"]<-6
          MARK6<-data_year[which(data_year$MARKER==6),]
          if(make.plot==TRUE){
          points(MARK6$XCAL,MARK6$YCAL, pch=3, col="black",cex=0.8)
          }
          data_year[which(data_year[,"ROW"]==row_code[j]),"ROW"]<-NA
        }else{
          data_year[which(data_year[,"ROW"]==row_code[j]),"ROW"]<-row_code[j]
        }
      }
    }

    backup<-data_year
    data_year<-backup

    #ROWS WITH LESS THEN 0.60 PERCENTAGE OF THE TOTAL AMOUNT OF CELLS SHOULD BE EXCLUDED-----
    row_code<-sort(unique(data_year[,"ROW",]))
    if(length(row_code)==0){
      data_year[,"POSITION"]<-NA
    }else{
      cut_off<-max(data_year[,"POSITION"],na.rm=TRUE)*percentage_max_nr_cells
      for (r in c(1:length(row_code))){
        selected_data<-data_year[which(data_year[,"ROW"]==row_code[r]),]
        if(nrow(selected_data)<cut_off){
          data_year[which(data_year[,"ROW"]==row_code[r]),"MARKER"]<-7
          MARK7<-data_year[which(data_year$MARKER==7),]
          if(make.plot==TRUE){
          points(MARK7$XCAL,MARK7$YCAL, pch=4, col="black",cex=0.8)
          }
            data_year[which(data_year[,"ROW"]==row_code[r]),"ROW"]<-NA
        }else{
          data_year[which(data_year[,"ROW"]==row_code[r]),"ROW"]<-row_code[r]
        }
      }}

    #ALL CELLS AND ROWS ARE RENUMBERED FOR THE FINAL OUTPUT----
    data_year[,"POSITION"]<-NA
    row_code<-sort(unique(data_year[,"ROW",]))
    if(length(row_code)!=0){
      for (r in c(1:length(row_code))){
        isolated_data<-data_year[which(data_year[,"ROW"]==row_code[r]),]
        CID<-isolated_data[order(isolated_data$YCAL),"CID"]
        data_year[which(data_year[,"CID"]%in%CID),"ROW"]<-r
        data_year[which(data_year[,"CID"]%in%CID),"POSITION"]<-c(1:nrow(isolated_data))
      }}

    if(make.plot==TRUE){
      for(q in c(1:nrow(data_year))){
        text(data_year[q,"XCAL"],data_year[q,"YCAL"],data_year[q,"POSITION"],cex=0.5,col="black")}}
    if(nrow(data_year[which(data_year[,"MARKER"]==1|data_year[,"MARKER"]==2),])!=0){
      data_year[which(data_year[,"MARKER"]==1|data_year[,"MARKER"]==2),"MARKER"]<-1}
    if(nrow(data_year[which(data_year[,"MARKER"]==3|data_year[,"MARKER"]==4),])!=0){
      data_year[which(data_year[,"MARKER"]==3|data_year[,"MARKER"]==4),"MARKER"]<-2}
    if(nrow(data_year[which(data_year[,"MARKER"]==5),])!=0){
      data_year[which(data_year[,"MARKER"]==5),"MARKER"]<-3}
    if(nrow(data_year[which(data_year[,"MARKER"]==6),])!=0){
      data_year[which(data_year[,"MARKER"]==6),"MARKER"]<-4}
    if(nrow(data_year[which(data_year[,"MARKER"]==7),])!=0){
      data_year[which(data_year[,"MARKER"]==7),"MARKER"]<-5}
    if(u==1){output<-data_year}else{output<-rbind(output,data_year)}
  }
  col_nr<-ncol(output)
  weird<-which(colnames(output)=="SQRLENGTH")
  output<-output[,c(-weird)]
  return(output)
}

#7.write.output----
write.output<-function(input,location=c("D:\\Documents"),flip=FALSE){
      time_start <- Sys.time()



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
  print(Sys.time() - time_start)

  return(output_all_years)
}

#8.batch.mode----
batch.mode<-function(location=c("..."),files=FALSE,interact=TRUE,make.plot=TRUE,aligning=TRUE,frac.small=0.5,swe=0.5,sle=3,
                     ec=3,swl=0.5,sll=5,lc=10,prof.co=6,max.cells=0.5,list=FALSE,flip=FALSE){
  #test
  #location = c("D:\\Documents\\WSL\\07_work_documents\\2_results_excel\\Chapter 2 - Anatomical analysis\\RAPTOR\\Test-Georg")
  #files = c("17_3_15_A_a_Output_Cells.txt")
  #swe = 0.5
  #sle = 3
  #ec = 1.75
  #swl = 0.25
  #sll = 5
  #lc = 5
  #frac.small<-0.5
  #prof.co =  6
  #max.cells = 0.5
  #aligning = TRUE
  #interact<-FALSE
  #make.plot<-TRUE
  #list<-FALSE
  ###

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

  setwd(location)
  if(missing(files)){
    left                <- function(string, char){
      substr(string, 1,char)}
    right               <- function (string, char){
      substr(string,nchar(string)-(char-1),nchar(string))
    }
    files<-list.files(path = location)[which(right(list.files(path = location),3)=="txt")]
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

    input<-is.raptor(read.table(files[file],header=TRUE,sep="\t"))

    if(a.==TRUE){c.<-FALSE}
    if(missing(list)){c.<-FALSE}
    if(interact==FALSE & list!=FALSE){
      c.<-list[which(list[,1]==unique(input[,"ID"])),2]}

    sample<-unique(input[,"ID"])
    if(b.==TRUE&a.==FALSE){pdf(file=paste(sample,".pdf",sep=""),height=210/25.4,width=297/25.4,paper="A4r")}
    output1<-align(input,interact=a.,make.plot=b.,list=c.,year=FALSE)
    if(b.==TRUE&a.==TRUE){pdf(file=paste(sample,".pdf",sep=""),height=210/25.4,width=297/25.4,paper="A4r")}
    output2<-first.cell(output1, frac.small = d., yrs = FALSE, make.plot = b.)
    output3<-pos.det(output2, swe = f., sle = g., ec = h., swl = i., sll = j., lc = k.,
                     prof.co = l., max.cells = m., yrs = FALSE , aligning = e. , make.plot = b.)

    years<-unique(output3[,"YEAR"])
    for(u in c(1:length(years)) ){
      data_year<-output3[which(output3[,"YEAR"]==years[u]),]
      year<-years[u]
      if(b.==TRUE){

        if(flip==FALSE){
          plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
               ylim=c(0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01,max(data_year[,"YCAL"],na.rm=TRUE)),
               main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")}
        if(flip==TRUE){
          plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",
               ylim=c(max(data_year[,"YCAL"],na.rm=TRUE),0-max(data_year[,"YCAL"],na.rm=TRUE)*0.01),
               main=paste(sample," - ",as.character(year)," - Rows: ",max(data_year[,"ROW"],na.rm=TRUE),sep=""),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2,col="white")
        }
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

        if(b.==TRUE){
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
      }
      first_cells<-data_year[which(data_year[,"POSITION"]==1),]
      for(i in c(1:nrow(first_cells))){
        x<-first_cells[i,"XCAL"]
        y<-first_cells[i,"YCAL"]
        length<-first_cells[i,"SQRLENGTH"]/1.2
        label_point<-first_cells[i,"ROW"]
        if(b.==TRUE){
          text(x,y-length,label=label_point,col='black',cex=0.8,font=2)}
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

