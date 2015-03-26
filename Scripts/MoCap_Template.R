#----Packages Required (No Entry Required)----
library(zoo)
setwd("/Volumes/Storage/freylab_projects/exp62_transplant/62-02_optotrak/DATA_edit_N_B/Scripts")
source("MoCapFunctions.R")
##Reads in .txt file for a subject and creates two data
##frames. master is complete with columns names.
#keepers removes the na columns.

#----Setup-----
#Enter the participant number... "03_32", etc. Make sure you have set your working directory to the participants folder in the InProgress directory
optoimport("03_25")

#creates a Hz vector and add it to the data frame for interpolation purposes
Hz=rep(c(1:500),102)
keepers=data.frame(Hz,keepers)
rm(Hz)

#clean, replaces 100000 missing data values with "NA"
clean=keepers
clean[clean==100000]<-"NA"

#import presentation output log to determine good trials
optoLog=read.table("03_25_optoReplant.txt",header=F,sep="")
x=length(optoLog$V1)
translation=c(rep(0,x))
optoLog=cbind(translation,optoLog)
optoLog$V1 = factor(optoLog$V1)
optoLog$translation=as.character(optoLog$translation)


y=c(3,4,5,6,10,11,12,13,20,21,30,31,32,33,40,41,50,61,62,63,70,80,90,91,92,93,94,100)#list of all code values
z=c("LH press","RH press","LH lift", "RH lift", "LH Block Start","RH Block Start","Visible Block","Hidden Block","load epoch: (includes signal to close eyes)","load epoch: no-eye close signal","ERROR: lifted during loading","ERROR: no lift within 3 seconds after onset","ERROR: wrong hand lifted on move","ERROR: wrong hand lifted during swtimulus present","preview epoch", "stimulus (ready) epoch","wait for reach epoch","MOVE: 1cm cube","MOVE: 2cm cube","MOVE: 4cm cube","wait for optotrak to reset","trial finished ok","CONFIRMATION: confirmed","CONFIRMATION: denied","CONFIRMATION: aborted","CONFIRMATION: trial ended before experimenter could respond","CONFIRMATION: auto-accept","BLOCK END")#interpretation of that value
#y and z should be the same length
for (i in 1:x){
if(optoLog$V1[i]==10){
optoLog$translation[i] <- "LH Block Start"}
if(optoLog$V1[i]==11){
optoLog$translation[i] <- "RH Block Start"}
if(optoLog$V1[i]==12){
optoLog$translation[i] <- "Visible Block Start"}
if(optoLog$V1[i]==13){
optoLog$translation[i] <- "Hidden Block Start"}
if(optoLog$V1[i]==30){
optoLog$translation[i] <- "ERROR: lifted during loading/checking"}
if(optoLog$V1[i]==31){
optoLog$translation[i] <- "ERROR: no lift in 3s after stimulus"}
if(optoLog$V1[i]==32){
optoLog$translation[i] <- "ERROR: lifted wrong hand during move"}
if(optoLog$V1[i]==33){
optoLog$translation[i] <- "ERROR: lifted wrong hand during stimulus presentation"}
if(optoLog$V1[i]==61){
optoLog$translation[i] <- "move epoch: 1cm cube"}
if(optoLog$V1[i]==62){
optoLog$translation[i] <- "move epoch: 2cm cube"}
if(optoLog$V1[i]==63){
optoLog$translation[i] <- "move epoch: 3cm cube"}
if(optoLog$V1[i]==80){
optoLog$translation[i] <- "trial finished okay"}
if(optoLog$V1[i]==90){
optoLog$translation[i] <- "experimenter confirmed"}
if(optoLog$V1[i]==91){
optoLog$translation[i] <- "experimenter denied"}
if(optoLog$V1[i]==92){
optoLog$translation[i] <- "experimenter aborted"}
if(optoLog$V1[i]==93){
optoLog$translation[i] <- "auto denial"}
if(optoLog$V1[i]==91){
optoLog$translation[i] <- "experimenter denied"}
if(optoLog$V1[i]==100){
optoLog$translation[i] <- "end of block"}
}


optoReport=optoLog[optoLog$V1 == 10|optoLog$V1 == 11|optoLog$V1 == 12|optoLog$V1 == 13|optoLog$V1 == 30|optoLog$V1 == 31|optoLog$V1 == 32|optoLog$V1 == 33|optoLog$V1 == 61|optoLog$V1 == 62|optoLog$V1 == 63|optoLog$V1 == 90|optoLog$V1 == 91|optoLog$V1 == 92|optoLog$V1 == 93|optoLog$V1 == 94|optoLog$V1 == 100|optoLog$V1 == 101|optoLog$V1 == 80|optoLog$V1 == ,]
optoReport=optoLog[optoLog$V1 !=  ,]



#interpolate the missing values based on
#Hz for all trials, outputs a data frame called inter
inter=clean
N=length(clean$optotrak.pulse.number)/500
S=1   #the trial to start interpolating for
M=500 #sets the max number of consecutive NA's to interpolate values for

#----Convert coordinate data into numeric for interpolation----
inter$x1=as.numeric(inter$x1)
inter$x2=as.numeric(inter$x2)
inter$x3=as.numeric(inter$x3)
inter$x4=as.numeric(inter$x4)
inter$x5=as.numeric(inter$x5)
inter$x6=as.numeric(inter$x6)
inter$x7=as.numeric(inter$x7)
inter$x8=as.numeric(inter$x8)
inter$x9=as.numeric(inter$x9)
inter$x10=as.numeric(inter$x10)
inter$x11=as.numeric(inter$x11)
inter$x12=as.numeric(inter$x12)

inter$y1=as.numeric(inter$y1)
inter$y2=as.numeric(inter$y2)
inter$y3=as.numeric(inter$y3)
inter$y4=as.numeric(inter$y4)
inter$y5=as.numeric(inter$y5)
inter$y6=as.numeric(inter$y6)
inter$y7=as.numeric(inter$y7)
inter$y8=as.numeric(inter$y8)
inter$y9=as.numeric(inter$y9)
inter$y10=as.numeric(inter$y10)
inter$y11=as.numeric(inter$y11)
inter$y12=as.numeric(inter$y12)

inter$z1=as.numeric(inter$z1)
inter$z2=as.numeric(inter$z2)
inter$z3=as.numeric(inter$z3)
inter$z4=as.numeric(inter$z4)
inter$z5=as.numeric(inter$z5)
inter$z6=as.numeric(inter$z6)
inter$z7=as.numeric(inter$z7)
inter$z8=as.numeric(inter$z8)å`
inter$z9=as.numeric(inter$z9)
inter$z10=as.numeric(inter$z10)
inter$z11=as.numeric(inter$z11)
inter$z12=as.numeric(inter$z12)

#----Interpolate one marker at a time----
#Will through and error if on any of the runs a marker starts out missing (NA).
for (i in S:N){
  inter$x1[inter$optotrak.pulse.number==i] <- na.approx(inter$x1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y1[inter$optotrak.pulse.number==i] <- na.approx(inter$y1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z1[inter$optotrak.pulse.number==i] <- na.approx(inter$z1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x2[inter$optotrak.pulse.number==i] <- na.approx(inter$x2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y2[inter$optotrak.pulse.number==i] <- na.approx(inter$y2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z2[inter$optotrak.pulse.number==i] <- na.approx(inter$z2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x3[inter$optotrak.pulse.number==i] <- na.approx(inter$x3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y3[inter$optotrak.pulse.number==i] <- na.approx(inter$y3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z3[inter$optotrak.pulse.number==i] <- na.approx(inter$z3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x4[inter$optotrak.pulse.number==i] <- na.approx(inter$x4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y4[inter$optotrak.pulse.number==i] <- na.approx(inter$y4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z4[inter$optotrak.pulse.number==i] <- na.approx(inter$z4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x5[inter$optotrak.pulse.number==i] <- na.approx(inter$x5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y5[inter$optotrak.pulse.number==i] <- na.approx(inter$y5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z5[inter$optotrak.pulse.number==i] <- na.approx(inter$z5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x6[inter$optotrak.pulse.number==i] <- na.approx(inter$x6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y6[inter$optotrak.pulse.number==i] <- na.approx(inter$y6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z6[inter$optotrak.pulse.number==i] <- na.approx(inter$z6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x7[inter$optotrak.pulse.number==i] <- na.approx(inter$x7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y7[inter$optotrak.pulse.number==i] <- na.approx(inter$y7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z7[inter$optotrak.pulse.number==i] <- na.approx(inter$z7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x8[inter$optotrak.pulse.number==i] <- na.approx(inter$x8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y8[inter$optotrak.pulse.number==i] <- na.approx(inter$y8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$z8[inter$optotrak.pulse.number==i] <- na.approx(inter$z8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
}
for (i in S:N){
  inter$x9[inter$optotrak.pulse.number==i] <- na.approx(inter$x9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$y9[inter$optotrak.pulse.number==i] <- na.approx(inter$y9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$z9[inter$optotrak.pulse.number==i] <- na.approx(inter$z9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M
}
for (i in S:N){
  inter$x10[inter$optotrak.pulse.number==i] <- na.approx(inter$x10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$y10[inter$optotrak.pulse.number==i] <- na.approx(inter$y10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$z10[inter$optotrak.pulse.number==i] <- na.approx(inter$z10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
}
for (i in S:N){
  inter$x11[inter$optotrak.pulse.number==i] <- na.approx(inter$x11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
  inter$y11[inter$optotrak.pulse.number==i] <- na.approx(inter$y11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$z11[inter$optotrak.pulse.number==i] <- na.approx(inter$z11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
}
for (i in S:N){
  inter$x12[inter$optotrak.pulse.number==i] <- na.approx(inter$x12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$y12[inter$optotrak.pulse.number==i] <- na.approx(inter$y12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
  inter$z12[inter$optotrak.pulse.number==i] <- na.approx(inter$z12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE, maxgap=M)
}

#----PostProcessing----
##creates .csv files for each trial##
##within the home directory##
optotrial=function(){
  
  z=length(levels(clean$optotrak.pulse.number))
  for(i in 1:z){
    write.csv(clean[clean$optotrak.pulse.number==i,],file=paste("Trial",i,".csv",sep=""),row.names=F)
  }
}

##removes bad trials for testing
clean=clean[clean$optotrak.pulse.number!= 1,]
clean=clean[clean$optotrak.pulse.number!= 2,]

##output for testing purposes
write.csv(keepers,file="keepers.csv",row.names=F)
write.csv(clean,file="clean.csv",row.names=F)
write.csv(inter,file="inter.csv",row.names=F)

#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]