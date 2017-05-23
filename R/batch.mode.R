
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
