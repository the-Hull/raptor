#1.example.data (SETWD in this function to make the dataset operational)----
example.data<-function(species=FALSE){
      setwd("D:\\Documents\\WSL\\07_work_documents\\2_results_excel\\Chapter 2 - Anatomical analysis\\RAPTOR\\Example datasets") #for now you need to set the wd
      if(species!="LOT_PICEA"&species!="SIB_LARIX"&species!="LOW_PINUS"&species!="MOUNT_PINUS")stop('dataset not present')
      y<-read.table(paste("example.data.txt",sep=""),sep="\t",header=TRUE)
      y<-y[which(y[,"ID"]==species),]
      return(y)}
