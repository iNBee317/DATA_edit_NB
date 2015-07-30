##Reads in .txt file for a subject and creates two data
##frames. master is complete with columns names.
#keepers removes the na columns. Adds a Hz column.

optoimport=function(name){
  setwd(paste("/Volumes/Swap/Opto_data/InProgress/", name, sep= ""))
  master=read.table(paste(name,".txt",sep=""),header=F,sep="")
  newnames=c("optotrak.pulse.number",paste(c("x","y","z","na","naa"),rep(1:12,each=5),sep=""))
  names(master)=newnames
  max=max(master$optotrak.pulse.number)
  master$optotrak.pulse.number=as.factor(master$optotrak.pulse.number)
  master<<-master
  keeps=c("optotrak.pulse.number",paste(c("x","y","z"),rep(1:12,each=3),sep=""))
  keepers<<-master[,keeps,drop=F]
  #creates a Hz vector and add it to the data frame for interpolation purposes
  Hz=rep(c(1:500),max)
  keepers<<-data.frame(Hz,keepers)
  #clean, replaces 100000 missing data values with "NA"
  keepers[keepers==100000]<<-"NA"
  
}

#import presentation output log to determine good trials
optosleuthe=function(name){
  optoLog<<-read.table("03_31_optoReplant.txt",header=F,sep="")
  x=length(optoLog$V1)
  translation=c(rep(0,x))
  TimeSec=c(rep(0,x))
  TimeAbsolute=c(rep(0,x))
  optoLog<<-cbind(translation,optoLog,TimeSec,TimeAbsolute)
  optoLog$V1 <<- factor(optoLog$V1)
  optoLog$translation<<-as.character(optoLog$translation)
  optoLog$TimeSec<<-optoLog$V2/1000

y=c(3,4,5,6,10,11,12,13,20,21,30,31,32,33,40,41,50,61,62,63,70,80,90,91,92,93,94,100)#list of all code values
z=c("LH press","RH press","LH lift", "RH lift", "LH Block Start","RH Block Start","Visible Block","Hidden Block","load epoch: (includes signal to close eyes)","load epoch: no-eye close signal","ERROR: lifted during loading","ERROR: no lift within 3 seconds after onset","ERROR: wrong hand lifted on move","ERROR: wrong hand lifted during swtimulus present","preview epoch", "stimulus (ready) epoch","wait for reach epoch","MOVE: 1cm cube","MOVE: 2cm cube","MOVE: 4cm cube","wait for optotrak to reset","trial finished ok","CONFIRMATION: confirmed","CONFIRMATION: denied","CONFIRMATION: aborted","CONFIRMATION: trial ended before experimenter could respond","CONFIRMATION: auto-accept","BLOCK END")#keeperspretation of that value
#y and z should be the same length
for (i in 1:x){
  if(optoLog$V1[i]==10){
    optoLog$translation[i] <<- "LH Block Start"}
  if(optoLog$V1[i]==11){
    optoLog$translation[i] <<- "RH Block Start"}
  if(optoLog$V1[i]==12){
    optoLog$translation[i] <<- "Visible Block Start"}
  if(optoLog$V1[i]==13){
    optoLog$translation[i] <<- "Hidden Block Start"}
  if(optoLog$V1[i]==30){
    optoLog$translation[i] <<- "ERROR: lifted during loading/checking"}
  if(optoLog$V1[i]==31){
    optoLog$translation[i] <<- "ERROR: no lift in 3s after stimulus"}
  if(optoLog$V1[i]==32){
    optoLog$translation[i] <<- "ERROR: lifted wrong hand during move"}
  if(optoLog$V1[i]==33){
    optoLog$translation[i] <<- "ERROR: lifted wrong hand during stimulus presentation"}
  if(optoLog$V1[i]==61){
    optoLog$translation[i] <<- "move epoch: 1cm cube"}
  if(optoLog$V1[i]==62){
    optoLog$translation[i] <<- "move epoch: 2cm cube"}
  if(optoLog$V1[i]==63){
    optoLog$translation[i] <<- "move epoch: 3cm cube"}
  if(optoLog$V1[i]==80){
    optoLog$translation[i] <<- "trial finished okay"}
  if(optoLog$V1[i]==90){
    optoLog$translation[i] <<- "experimenter confirmed"}
  if(optoLog$V1[i]==91){
    optoLog$translation[i] <<- "experimenter denied"}
  if(optoLog$V1[i]==92){
    optoLog$translation[i] <<- "experimenter aborted"}
  if(optoLog$V1[i]==93){
    optoLog$translation[i] <<- "auto denial"}
  if(optoLog$V1[i]==91){
    optoLog$translation[i] <<- "experimenter denied"}
  if(optoLog$V1[i]==100){
    optoLog$translation[i] <<- "end of block"}
}}

interpolate=function(N,S,M){
inter<<-keepers
inter$optotrak.pulse.number<<-factor(inter$optotrak.pulse.number)


#----Convert coordinate data into numeric for interpolation----
inter$x1<<-as.numeric(inter$x1)
inter$x2<<-as.numeric(inter$x2)
inter$x3<<-as.numeric(inter$x3)
inter$x4<<-as.numeric(inter$x4)
inter$x5<<-as.numeric(inter$x5)
inter$x6<<-as.numeric(inter$x6)
inter$x7<<-as.numeric(inter$x7)
inter$x8<<-as.numeric(inter$x8)
inter$x9<<-as.numeric(inter$x9)
inter$x10<<-as.numeric(inter$x10)
inter$x11<<-as.numeric(inter$x11)
inter$x12<<-as.numeric(inter$x12)

inter$y1<<-as.numeric(inter$y1)
inter$y2<<-as.numeric(inter$y2)
inter$y3<<-as.numeric(inter$y3)
inter$y4<<-as.numeric(inter$y4)
inter$y5<<-as.numeric(inter$y5)
inter$y6<<-as.numeric(inter$y6)
inter$y7<<-as.numeric(inter$y7)
inter$y8<<-as.numeric(inter$y8)
inter$y9<<-as.numeric(inter$y9)
inter$y10<<-as.numeric(inter$y10)
inter$y11<<-as.numeric(inter$y11)
inter$y12<<-as.numeric(inter$y12)

inter$z1<<-as.numeric(inter$z1)
inter$z2<<-as.numeric(inter$z2)
inter$z3<<-as.numeric(inter$z3)
inter$z4<<-as.numeric(inter$z4)
inter$z5<<-as.numeric(inter$z5)
inter$z6<<-as.numeric(inter$z6)
inter$z7<<-as.numeric(inter$z7)
inter$z8<<-as.numeric(inter$z8)
inter$z9<<-as.numeric(inter$z9)
inter$z10<<-as.numeric(inter$z10)
inter$z11<<-as.numeric(inter$z11)
inter$z12<<-as.numeric(inter$z12)

  #----Interpolate one marker at a time----#
  #Will through and error if on any of the runs a marker starts out missing (NA).
for (i in S:N){
  if(!is.na(inter$x1[inter$optotrak.pulse.number==i][1])){
    inter$x1[inter$optotrak.pulse.number==i] <<- na.approx(inter$x1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$y1[inter$optotrak.pulse.number==i][1])){
      inter$y1[inter$optotrak.pulse.number==i] <<- na.approx(inter$y1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
        if(!is.na(inter$z1[inter$optotrak.pulse.number==i][1])){
        inter$z1[inter$optotrak.pulse.number==i] <<- na.approx(inter$z1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}

for (i in S:N){
  if(!is.na(inter$x2[inter$optotrak.pulse.number==i][1])){
    inter$x2[inter$optotrak.pulse.number==i] <<- na.approx(inter$x2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y2[inter$optotrak.pulse.number==i][1])){
      inter$y2[inter$optotrak.pulse.number==i] <<- na.approx(inter$y2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z2[inter$optotrak.pulse.number==i][1])){
        inter$z2[inter$optotrak.pulse.number==i] <<- na.approx(inter$z2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x3[inter$optotrak.pulse.number==i][1])){
    inter$x3[inter$optotrak.pulse.number==i] <<- na.approx(inter$x3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y3[inter$optotrak.pulse.number==i][1])){
      inter$y3[inter$optotrak.pulse.number==i] <<- na.approx(inter$y3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z3[inter$optotrak.pulse.number==i][1])){
        inter$z3[inter$optotrak.pulse.number==i] <<- na.approx(inter$z3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x4[inter$optotrak.pulse.number==i][1])){
    inter$x4[inter$optotrak.pulse.number==i] <<- na.approx(inter$x4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y4[inter$optotrak.pulse.number==i][1])){
      inter$y4[inter$optotrak.pulse.number==i] <<- na.approx(inter$y4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z4[inter$optotrak.pulse.number==i][1])){
        inter$z4[inter$optotrak.pulse.number==i] <<- na.approx(inter$z4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x5[inter$optotrak.pulse.number==i][1])){
    inter$x5[inter$optotrak.pulse.number==i] <<- na.approx(inter$x5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y5[inter$optotrak.pulse.number==i][1])){
      inter$y5[inter$optotrak.pulse.number==i] <<- na.approx(inter$y5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z5[inter$optotrak.pulse.number==i][1])){
        inter$z5[inter$optotrak.pulse.number==i] <<- na.approx(inter$z5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x6[inter$optotrak.pulse.number==i][1])){
    inter$x6[inter$optotrak.pulse.number==i] <<- na.approx(inter$x6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y6[inter$optotrak.pulse.number==i][1])){
      inter$y6[inter$optotrak.pulse.number==i] <<- na.approx(inter$y6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z6[inter$optotrak.pulse.number==i][1])){
        inter$z6[inter$optotrak.pulse.number==i] <<- na.approx(inter$z6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x7[inter$optotrak.pulse.number==i][1])){
    inter$x7[inter$optotrak.pulse.number==i] <<- na.approx(inter$x7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y7[inter$optotrak.pulse.number==i][1])){
      inter$y7[inter$optotrak.pulse.number==i] <<- na.approx(inter$y7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z7[inter$optotrak.pulse.number==i][1])){
        inter$z7[inter$optotrak.pulse.number==i] <<- na.approx(inter$z7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x8[inter$optotrak.pulse.number==i][1])){
    inter$x8[inter$optotrak.pulse.number==i] <<- na.approx(inter$x8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y8[inter$optotrak.pulse.number==i][1])){
      inter$y8[inter$optotrak.pulse.number==i] <<- na.approx(inter$y8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z8[inter$optotrak.pulse.number==i][1])){
        inter$z8[inter$optotrak.pulse.number==i] <<- na.approx(inter$z8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x9[inter$optotrak.pulse.number==i][1])){
    inter$x9[inter$optotrak.pulse.number==i] <<- na.approx(inter$x9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y9[inter$optotrak.pulse.number==i][1])){
      inter$y9[inter$optotrak.pulse.number==i] <<- na.approx(inter$y9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z9[inter$optotrak.pulse.number==i][1])){
        inter$z9[inter$optotrak.pulse.number==i] <<- na.approx(inter$z9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x10[inter$optotrak.pulse.number==i][1])){
    inter$x10[inter$optotrak.pulse.number==i] <<- na.approx(inter$x10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y10[inter$optotrak.pulse.number==i][1])){
      inter$y10[inter$optotrak.pulse.number==i] <<- na.approx(inter$y10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z10[inter$optotrak.pulse.number==i][1])){
        inter$z10[inter$optotrak.pulse.number==i] <<- na.approx(inter$z10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x11[inter$optotrak.pulse.number==i][1])){
    inter$x11[inter$optotrak.pulse.number==i] <<- na.approx(inter$x11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y11[inter$optotrak.pulse.number==i][1])){
      inter$y11[inter$optotrak.pulse.number==i] <<- na.approx(inter$y11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z11[inter$optotrak.pulse.number==i][1])){
        inter$z11[inter$optotrak.pulse.number==i] <<- na.approx(inter$z11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
for (i in S:N){
  if(!is.na(inter$x12[inter$optotrak.pulse.number==i][1])){
    inter$x12[inter$optotrak.pulse.number==i] <<- na.approx(inter$x12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
    if(!is.na(inter$y12[inter$optotrak.pulse.number==i][1])){
      inter$y12[inter$optotrak.pulse.number==i] <<- na.approx(inter$y12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)}
      if(!is.na(inter$z12[inter$optotrak.pulse.number==i][1])){
        inter$z12[inter$optotrak.pulse.number==i] <<- na.approx(inter$z12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = FALSE,maxgap=M)
      }}
}

optotrialwrite=function(){
  
  z=length(levels(inter$optotrak.pulse.number))
  for(i in 1:z){
    write.csv(inter[inter$optotrak.pulse.number==i,],file=paste("Trial",i,".csv",sep=""),row.names=F)
  for(i in 1:z){
    read.csv(paste("Trial",i,".csv",sep=""))
  }}

}

graphs.full=function(num,x){
  plot(analyze.remove$d.index.thumb[analyze.remove$optotrak.pulse.number==num],col='green',main=paste0("trial ", num," ", optoLog.remove$translation[optoLog.remove$a==num]),ylim=c(0,100))
  abline(h=dis.con[x],col="black")
  par(new=TRUE)
  plot(analyze.remove$v.index.thumb[analyze.remove$optotrak.pulse.number==num],col='brown',ylim=c(0,1500),yaxt='n')
  par(new=TRUE)
  plot(analyze.remove$v1[analyze.remove$optotrak.pulse.number==num],col='blue',ylim=c(0,3000),yaxt='n')
  text( x = 100, y = 3000, labels = paste0("Cutoff: ",abc.v1[x]), cex = 1.5, col = "blue" )
  abline(v=abc.v1[x],col="blue")
  text( x = 100, y = 2700, labels = paste0("Cutoff: ",abc.v1.low[x]), cex = 1.5, col = "orange" )
  abline(v=abc.v1.low[x],col="orange")
  abline(v=abc.v1.low[x]+m.gap,col="black")
  
}

graphs.cut=function(num){
  plot(cut$d.index.thumb[cut$optotrak.pulse.number==kept.list[num]],col='green',main=paste0("trial",kept.list[i]),ylim=c(0,100))
  par(new=TRUE)
  plot(cut$v.index.thumb[cut$optotrak.pulse.number==kept.list[num]],col='brown',ylim=c(0,1500))
  par(new=TRUE)
  plot(cut$v1[cut$optotrak.pulse.number==kept.list[num]],col='blue',ylim=c(0,3000))
  par(new=TRUE)
  plot(cut$v3[cut$optotrak.pulse.number==kept.list[num]],col='red',ylim=c(0,3000))
}















