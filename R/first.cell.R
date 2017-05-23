
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
