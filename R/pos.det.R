#' @title Determining the cell position
#'
#' @description Returns a \code{\link{data.frame}}, i.e. the same as produced by \code{\link{first.cell}}, but with added columns showing the ROW and POSITION which indicate the radial files number (ROW, from left to right) and radial cell position (from earlywood to latewood) of each tracheid. \code{\link{NA}} is assigned to cells not belonging to recognized radial files. A MARKER column is added to indicate the last detected cell in the earlywood search (indicated with 1), latewood search (2), the last detected cell (3), rows that are removed due to gaps (4, see prof.co) and rows that are removed due to limited amount of cells (5, see max.cells).
#' @param input an input file as produced by \code{\link{first.cell}}.
#' @param swe a numeric value that is multiplied by the square-rooted cell lumen area (l) of the target cell and used to determine the width of the rectangular search area which locates the next earlywood cell in the row (default = 0.5).
#' @param sle a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the length of the rectangle search area which locates the next earlywood cell in the row (default = 3).
#' @param ec threshold ratio between the lumen area of two consecutive earlywood cells to determine the end of the earlywood search (default = 1.75). The default setting indicates that the earlywood search ends when the next cell lumen area is at least 1.75 times smaller than the target cell.
#' @param swl a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the width of the rectangle search area which locates the next latewood cell in the row (default = 0.25).
#' @param sll a numeric value that is multiplied by the square rooted cell lumen area (l) of the target cell to determine the length of the rectangle search area which locates the next earlywood cell in the row (default = 5).
#' @param lc threshold ratio between the lumen area of two consecutive latewood cells to determine the end of the radial file (default = 10). The default setting indicates that the latewood search ends when the next cell lumen area is at least 10 times smaller than the target cell.
#' @param prof.co threshold ratio between the distance to the previous and consecutive cell to determine if the row (or radial file) should be excluded (default = 6).
#' @param max.cells threshold proportion of the maximum number of cells to determine if the radial file has to be excluded (default = 0.6).
#' @param aligning logical flag indicating whether a second alignment has to be performed based upon the cells detected within \code{\link{first.cell}} (default = \code{\link{TRUE}}).
#' @param list a \code{\link{data.frame}} of "CID" (unique cell ID) from cells which should be considered as first row cells. Should be filled if \code{\link{first.cell}} output is not used as input. The vector should be ordered to years and present specific "CID" which is considered the first cell.
#' @param yrs either a vector providing the year(s) of interest or \code{\link{FALSE}} to select all years included in input (default = \code{\link{FALSE}}).
#' @param make.plot logical flag indicating whether to make a plot (default =  \code{\link{FALSE}}).
#' @details After the identification of the first cells (with \code{\link{first.cell}}) within a radial file, this function assigns remaining cells to a corresponding radial file based on various search criteria. A local search algorithm is applied which searches cells above a specified target cell (n; starts with first cell of the radial file detected by \code{\link{first.cell}}). First the "earlywood" search process projects a search area from the selected cell with a specific width (x-axis) and length (y-axis). The search length and width is based on the length of the target cell (l_n) determined with size of the cell (where l_n= \code{\link{sqrt}}(CA_n) ) and multiplied by a factor (sle = search length earlywood, for the search length and swe = search width earlywood, for the search width). This initial search grid is presented in "orange"(if make.plot = \code{\link{TRUE}}), where the width can be adjusted by changing swe and the length by sle. When no cells are detected (last detected cells are indicated with orange circles), caused in most cases by the small size of latewood cells, a second search grid is established with an altered length and width (sll = search length latewood and swl = search width latewood; presented in "red" if make.plot = \code{\link{TRUE}}; last detected cell are indicates with red circles). Due to smaller fragments of erroneously detected cells, for both search grids a cut-off value is added where the next cell should not be smaller than the lumen area of the target cell (CA_n) times a factor (ec = earlywood cut off and lc = latewood cut off). Finally, a flexible spline is fitted through the selected cells within a row to detect missing cells. Once all cells are detected (last cell is indicated with a red square), the distance between the cells is analysed and depending upon a profile cut-off factor (prof.co) the row is omitted, if the distance difference between n+1 and n+2 times prof.co is bigger than the distance of n and n+1 (which could present a gap often caused by resin ducts; omitted rows are indicated with a "+" symbol). Also, rows that have less cells then the maximum cell count times a factor (max.cells) are removed, as the row might be incomplete (omitted rows are indicated with a "x" symbol).
#' @import mgcv
#' @export
#' @return An \code{\link{is.raptor}} file with an added column describing the position within the radial file.
#' @usage pos.det(input,
#' swe = 0.5,
#' sle = 3,
#' ec = 1.75 ,
#' swl = 0.25,
#' sll = 5,
#' lc = 5,
#' prof.co = 6,
#' max.cells = 0.5,
#' list=FALSE,
#' yrs = FALSE,
#' aligning = TRUE,
#' make.plot = TRUE)
#' @examples
#' #example of position detection
#' input<-is.raptor(example.data(species="MOUNT_PINUS"), str = FALSE)
#' aligned<-align(input, list=c("h", "h", "h", 0.03), make.plot = FALSE)
#' first<-first.cell(aligned, frac.small = 0.2, yrs = FALSE, make.plot = FALSE)
#' output<-pos.det(first, swe = 0.7, sle = 3, ec = 1.75, swl = 0.5, sll = 5, lc = 10,
#'                 prof.co = 1.7, max.cells = 0.7, yrs = FALSE, aligning = FALSE, make.plot = TRUE)
pos.det<-function(input,swe = 0.5,sle = 3,ec = 1.75,swl = 0.25,sll = 5,lc = 5,prof.co =6,
                  max.cells = 0.5,list = FALSE,yrs = FALSE, aligning=TRUE,make.plot=TRUE){

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
                  graphics::layout(matrix(c(1),ncol=1, byrow = TRUE))
                  graphics::par(mar=c(5,5,3,1))
                  graphics::plot(data_year[,"XCAL"],data_year[,"YCAL"],ylab="Rel. Y-coordinates (micron)",main=paste(sample,as.character(year),sep=" - "),xlab="Rel. X-coordinates (micron)",pch=16,cex=0.2)
                  nrcells<-nrow(data_year)
                  if(make.plot==TRUE){
                        for(i in c(1:nrcells)){
                              length<-data_year[i,"SQRLENGTH"]/2
                              x     <-data_year[i,"XCAL"]
                              y     <-data_year[i,"YCAL"]
                              x_cor<-c((x-length),(x+length),(x+length),(x-length))
                              y_cor<-c((y+length),(y+length),(y-length),(y-length))
                              graphics::polygon(x_cor,y_cor,col="grey")
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
                                    graphics::points(MARK1$XCAL,MARK1$YCAL, pch=16, col="orange",cex=1.3)
                                    graphics::points(MARK1$XCAL,MARK1$YCAL, pch=1, col="black",cex=1.3)
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
                                                graphics::points(MARK2$XCAL,MARK2$YCAL, pch=16, col="orange",cex=1.3)
                                                graphics::points(MARK2$XCAL,MARK2$YCAL, pch=1, col="black",cex=1.3)
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
                                    graphics::points(MARK3$XCAL,MARK3$YCAL, pch=16, col="red",cex=1.3)
                                    graphics::points(MARK3$XCAL,MARK3$YCAL, pch=1, col="black",cex=1.3)
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
                                          graphics::points(MARK4$XCAL,MARK4$YCAL, pch=16, col="red",cex=1.3)
                                          graphics::points(MARK4$XCAL,MARK4$YCAL, pch=1, col="black",cex=1.3)
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

            # list.of.packages <- c("mgcv","gam","base")
            # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
            # if(length(new.packages)) install.packages(new.packages)
            # require("gam")
            # require("mgcv")
            # require("base")

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
                        Model                  <-mgcv::gam(x ~ mgcv::s(y),data=input_m)
                        y                      <-c(min(data_select[,"YCAL"],na.rm=TRUE):max(data_select[,"YCAL"],na.rm=TRUE))
                        predict                <-mgcv::predict.gam(Model,newdata=data.frame(y))
                        mean_width             <-mean(data_select[,"SQRLENGTH"],na.rm=TRUE)
                        #if(make.plot==TRUE){#lines((predict),y,lty=1)
                        #  lines((as.numeric(predict)+mean_width*gam_width),y,lty=2)
                        #  lines((as.numeric(predict)-mean_width*gam_width),y,lty=2)
                        #}
                        data_na                <-data_year[which(is.na(data_year[,"ROW"])==TRUE),]
                        y                      <-data_na[,"YCAL"]
                        value                  <-mgcv::predict.gam(Model,newdata=data.frame(y))
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
                                    graphics::points(MARK5$XCAL,MARK5$YCAL, pch=15, col="red",cex=1.3)
                                    graphics::points(MARK5$XCAL,MARK5$YCAL, pch=0, col="black",cex=1.3)
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

