###EXAMPLES###
#R script of RAPTOR examples----
#explaining general input handling
?data.frame
?str
#here the dataset is uploaded
input<-example.data(species="LOT_PICEA") 
View(input)
str(input)
#here the cell visualization is provided
input<-is.raptor(input,str=TRUE)
plot.cells(input,interact=TRUE)
2010
x
#here the data is aligned and the first cell within the row is detected
input<-align(input,year=2007,list=FALSE,interact=TRUE,make.plot=TRUE)
0.1
y
output <- first.cell(input, frac.small = 0.5, yrs = 2007, make.plot = TRUE)
#here the row and position within the row is detected
first <- first.cell(input, frac.small = 0.5, yrs = 2011, make.plot = FALSE)
output <-pos.det(first, swe = 0.5, sle = 3, ec = 1.75, swl = 0.25, sll = 5, lc = 10,prof.co = 6, max.cells = 0.5, yrs = FALSE , aligning = TRUE , make.plot = TRUE)
write.output(output)

#R script for generating output presented in table 2 from RAPTOR----
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(5,5,5,5))
#load example data, prepare data, detect first cells and establish the position within the row
input<-is.raptor(example.data(species="LOT_PICEA") , str = FALSE)
first<-first.cell(input, frac.small = 0.5, yrs = FALSE, make.plot = TRUE)
output<-pos.det(first, swe = 0.5, sle = 3, ec = 1.75, swl = 0.25, sll = 5, lc = 10,prof.co = 1.5, max.cells = 0.5, yrs = FALSE , aligning = FALSE , make.plot = TRUE)
final<-write.output(output)
#generating output statistics
for(i in c(1:length(unique(final[,"YEAR"])))){
id<-as.character(unique(final[,"ID"]))
year<-unique(final[,"YEAR"])[i]
ncell<-nrow(final[which(final[,"YEAR"]==unique(final[,"YEAR"])[i]),])
row<-max(final[which(final[,"YEAR"]==unique(final[,"YEAR"])[i]),"ROW"],na.rm=TRUE)
pos<-max(final[which(final[,"YEAR"]==unique(final[,"YEAR"])[i]),"POSITION"],na.rm=TRUE)
select<-final[which(final[,"YEAR"]==unique(final[,"YEAR"])[i]),]
mean_ca<-NA
mean_cwt<-NA
for(j in c(1:length(unique(na.omit(select[,"ROW"]))) )){
file<-select[which(select[,"ROW"]==unique(na.omit(select[,"ROW"]))[j]),]
mean_ca[j]<-mean(file[which(file[,"POSITION"]<4),"CA"],na.rm=TRUE)
mean_cwt[j]<-mean(file[which(file[,"POSITION"]> max(file[,"POSITION"],na.rm=TRUE)-5 ),"CWTALL"],na.rm=TRUE)
}
ca<-mean(mean_ca,na.rm=TRUE)
cwt<-mean(mean_cwt,na.rm=TRUE)
output<-data.frame(ID=id,YEAR=year,Nall=ncell,Nrows=row,MAXcell=pos,CA3=ca,CWT5=cwt)
if(i==1){final.output<-output}else{final.output<-rbind(final.output,output)}
}
View(final.output)

#R script for generating output presented in figure 5 and 6 from RAPTOR----
#download required packages
list.of.packages <- c("tgram")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require("tgram")

#run script for three specific years per species
#process lowland pinus sylverstris
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(5,5,5,5))
input<-is.raptor(example.data(species="LOW_PINUS") , str = FALSE)
aligned<-align(input,list=c("v","v",-0.26,"h"),make.plot = FALSE)
first<-first.cell(aligned, frac.small = 0.2, yrs = FALSE, make.plot = FALSE)
output<-pos.det(first, swe = 0.7, sle = 3, ec = 1.75, swl = 0.5, sll = 5, lc = 10,prof.co = 6, max.cells = 0.5, yrs = FALSE , aligning = FALSE , make.plot = FALSE)
LOW_PINUS<-write.output(output)

#process mountain pinus cembra
input<-is.raptor(example.data(species="MOUNT_PINUS") , str = FALSE)
aligned<-align(input,list=c("h","h","h",0.03),make.plot = FALSE)
first<-first.cell(aligned, frac.small = 0.2, yrs = FALSE, make.plot = FALSE)
output<-pos.det(first, swe = 0.7, sle = 3, ec = 1.75, swl = 0.5, sll = 5, lc = 10,prof.co = 1.7, max.cells = 0.7, yrs = FALSE , aligning = FALSE , make.plot = FALSE)
MOUNT_PINUS<-write.output(output)

#processsing siberian larix siberica
input<-is.raptor(example.data(species="SIB_LARIX") , str = FALSE)
aligned<-align(input)
first<-first.cell(aligned, frac.small = 0.5, yrs = FALSE, make.plot = FALSE)
output<-pos.det(first, swe = 0.3, sle = 3, ec = 1.5, swl = 0.5, sll = 5, lc = 15,prof.co =4, max.cells = 0.5, yrs = FALSE , aligning = FALSE , make.plot = FALSE)
sib_larix<-write.output(output)

#removing rows which are unsuitable
#1. input
corrections<-data.frame(year=c(2010,2010,2010,2009,2009,2009,2009,2008,2008,2008,2008,2008,2008,2007,2007,2007),
             row=c(19,15,9,6,11,14,17,5,6,14,17,24,15,2,8,14))
View(corrections)
#2. removing rows
for(i in c(1:nrow(corrections))){
sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]  &sib_larix[,"ROW"]==corrections[i,2] ),"POSITION"]<-rep(NA,length(sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]&sib_larix[,"ROW"]==corrections[i,2] ),"POSITION"]))
sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]  &sib_larix[,"ROW"]==corrections[i,2] ),"ROW"]<-rep(NA,length(sib_larix[which(sib_larix[,"YEAR"]==corrections[i,1]&sib_larix[,"ROW"]==corrections[i,2] ),"POSITION"]))}
#3. renumbering
SIB_LARIX<-write.output(sib_larix)
for(i in c(1:length(unique(SIB_LARIX[,"YEAR"])))){
row_id<-unique(SIB_LARIX[which(SIB_LARIX[,"YEAR"]==unique(SIB_LARIX[,"YEAR"])[i]),"ROW"],na.rm=TRUE)
row_id<-na.omit(row_id[order(row_id)])
for(j in c(1:length(row_id))){
SIB_LARIX[which(SIB_LARIX[,"YEAR"]==unique(SIB_LARIX[,"YEAR"])[i] & SIB_LARIX[,"ROW"]==row_id[j]),"ROW"]<-j
}}
View(SIB_LARIX)

#processsing lotschental picea abies
input<-is.raptor(example.data(species="LOT_PICEA") , str = FALSE)
input<-input[which(input[,"YEAR"]>2006 &input[,"YEAR"]<2011),] #select years 2007-2010
aligned<-align(input,list=c(0.04,0.04,0,0))
first<-first.cell(aligned, frac.small = 0.5, yrs = FALSE, make.plot = FALSE)
output<-pos.det(first, swe = 0.5, sle = 3, ec = 1.75, swl = 0.25, sll = 5, lc = 10,prof.co = 1.5, max.cells = 0.5, yrs = FALSE , aligning = FALSE , make.plot = FALSE)
LOT_PICEA<-write.output(output)

#generate tracheidograms for all species
sample<-c("LOT_PICEA","SIB_LARIX","MOUNT_PINUS","LOW_PINUS")
for(i in c(1:length(sample))){
#i<-1
input<-get(sample[i])
years<-unique(as.numeric(input$YEAR))
for(j in c(1:length(years))){
#j<-1
select<-input[which(input["YEAR"]==years[j]),]
#prepraring data
lumen<-na.omit(data.frame(gram=select[,"ROW"],lumen.wall="l",order=select[,"POSITION"],area=select[,"CA"]))
wall<-na.omit(data.frame(gram=select[,"ROW"],lumen.wall="w",order=select[,"POSITION"],area=select[,"CWTALL"]))
gram<-rbind(lumen,wall)
gram<-gram[order(gram[,"gram"]),]
gram[,"gram"]<-as.integer(gram[,"gram"])
#determining mean maximum cells
max.cells<-NA
for(z in c(1:max(gram[,"gram"]))){
gram[which(gram[,"gram"]==unique(gram[,"gram"])[z]),]
max.cells[z]<-max(gram[which(gram[,"gram"]==unique(gram[,"gram"])[z]),"order"])
}
mean.cells<-round(mean(max.cells))
#apply standardization
output<-with(gram,standz.all(traq=area, series=gram,wl=lumen.wall, w.char="w", G=mean.cells))
lumen.mean<-colMeans(output$data.stdz[output$which.l,])
wall.mean<-colMeans(output$data.stdz[output$which.w,])
par(mfrow=c(2,1))
par(oma=c(5,0,0,0))
par(mar=c(0,5,1,1))
plot(gram[which(gram[,"lumen.wall"]=="l"),"order"],gram[which(gram[,"lumen.wall"]=="l"),"area"],pch=16,ylab="Lumen area (micron)",xlab="",yaxt="n",xaxt="n",col="grey")
axis(side=1,labels=FALSE)
plot(output,add=TRUE,which="l",type="l")
lines(c(1:mean.cells),lumen.mean,col="black",lwd=2)
legend("topright",c(paste(sample[i],years[j]),"RAPTOR output","Stand. measurement","Mean tracheidogram"),
lty=c(1,NA,1,1),pch=c(16,16,16,16),pt.cex=c(0,2,0,0,0),lwd=c(0,0,1,2),col=c("white","grey","black","black"), 
text.font=c(2,1,1,1),bty="n")
axis(side=2,las=2)
plot(gram[which(gram[,"lumen.wall"]=="w"),"order"],gram[which(gram[,"lumen.wall"]=="w"),"area"],pch=16,ylab="Cell wall thickness (micron)",xlab="",yaxt="n",col="grey")
plot(output,add=TRUE,which="w",type="l")
axis(side=2,las=2)
lines(c(1:mean.cells),wall.mean,col="black",lwd=2)
mtext(side=1,"Cell position (#)",padj=4)

#generate output and store data
output<-data.frame(ID=sample[i],YEAR=years[j],POSITION=c(1:mean.cells) ,LUMEN= lumen.mean,WALL= wall.mean)
if(i==1&j==1){final<-output}else{final<-rbind(final,output)}}}

#inter-species comparison on multi-annual tracheidograms
layout(matrix(c(1,3,5,7,2,4,6,8),nc=4, byrow = TRUE))
par(oma=c(5,5,1,1))
par(mar=c(0,1,1,0))
lumen.lim<-2000
wall.lim<-11
colour<-c("darkgreen","darkorange","palegreen","greenyellow")
thickness<-c(1,1,2,2)
type<-c(2,1,2,1)
for(i in c(1:length(sample))){
select<-final[which(final[,"ID"]==sample[i]),]  
plot(1,1,type="l",ylab="",col="white",xlab="",yaxt="n",xaxt="n",ylim=c(0,lumen.lim),xlim=c(1,max(select[,"POSITION"])))
axis(side=1,labels=FALSE)
axis(side=2,las=2,labels=FALSE)
if(i==4){abline(v=seq(0,100,10),col="grey",lty=3)}else{abline(v=seq(0,100,5),col="grey",lty=3)}
box()
if(i==1){mtext(side=2,"Lumen area (micron)",padj=-4)
  axis(side=2,las=2)
  legend("topleft",c("2007","2008","2009","2010"),
         lty=c(2,1,2,1),lwd=c(1,1,2,2),col=c("black","black","black","black"),bty="n")}
for(j in c(1:length(unique(select[,"YEAR"])) )){
lines(select[which(select[,"YEAR"]==unique(select[,"YEAR"])[j]),"POSITION"],select[which(select[,"YEAR"]==unique(select[,"YEAR"])[j]),"LUMEN"],col=colour[i],lwd=thickness[j],lty=type[j])
}
plot(1,1,type="l",ylab="",col="white",xlab="",yaxt="n",xaxt="n",ylim=c(1.5,wall.lim),xlim=c(1,max(select[,"POSITION"])))
axis(side=1)
axis(side=2,las=2,labels=FALSE)
if(i==4){abline(v=seq(0,100,10),col="grey",lty=3)}else{abline(v=seq(0,100,5),col="grey",lty=3)}
box()
if(i==1){mtext(side=2,"Cell wall thickness (micron)",padj=-4)
  axis(side=2,las=2)
  legend("topleft",c("Picea abies","Larix sibirica","Pinus cembra","Pinus sylvestris"),
         lty=c(1,1,1,1),lwd=c(2,2,2,2),col=colour,bty="n",text.font=c(3,3,3,3))}
for(j in c(1:length(unique(select[,"YEAR"])) )){
  lines(select[which(select[,"YEAR"]==unique(select[,"YEAR"])[j]),"POSITION"],select[which(select[,"YEAR"]==unique(select[,"YEAR"])[j]),"WALL"],col=colour[i],lwd=thickness[j],lty=type[j])
}}
mtext(side=1,"Cell position (#)",padj=3,outer=TRUE)