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
