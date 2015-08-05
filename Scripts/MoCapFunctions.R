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
  optoLog<<-read.table(paste0(participant,"_","optoReplant.txt"),header=F,sep="")
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

graphs.full=function(num,x,optoLog=F){
  if(optoLog == T){ 
    plot(analyze.remove$d.index.thumb[analyze.remove$optotrak.pulse.number==num],col='green',main=paste0("trial ", num," ", analyze.remove$blocksize[analyze.remove$optotrak.pulse.number==num][1]),ylim=c(0,100))
    abline(h=dis.con[x],col="black")
    par(new=TRUE)
    plot(analyze.remove$v.index.thumb[analyze.remove$optotrak.pulse.number==num],col='brown',ylim=c(0,1500),yaxt='n')
    par(new=TRUE)
    plot(analyze.remove$v1[analyze.remove$optotrak.pulse.number==num],col='blue',ylim=c(0,3000),yaxt='n')
    text( x = 100, y = 3000, labels = paste0("Cutoff: ",abc.v1[x]), cex = 1.5, col = "blue" )
    abline(v=abc.v1[x],col="blue")
    text( x = 100, y = 2700, labels = paste0("Cutoff: ",abc.v1.low[x]), cex = 1.5, col = "orange" )
    abline(v=abc.v1.low[x],col="orange")
    abline(v=abc.v1.low[x]+m.gap,col="black")}
  
  if(optoLog == F){
    plot(analyze.remove$d.index.thumb[analyze.remove$optotrak.pulse.number==num],col='green',main=paste0("trial ", num),ylim=c(0,100))
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
}

graphs.cut=function(num){
  plot(cut$d.index.thumb[cut$optotrak.pulse.number==i],col='green',main=paste0("trial",kept.list[i]),ylim=c(0,100),xlim=c(0,200))
  par(new=TRUE)
  plot(cut$v.index.thumb[cut$optotrak.pulse.number==i],col='brown',ylim=c(0,1500),xlim=c(0,200))
  par(new=TRUE)
  plot(cut$v1[cut$optotrak.pulse.number==i],col='blue',ylim=c(0,3000),xlim=c(0,200))
  par(new=TRUE)
  plot(cut$v3[cut$optotrak.pulse.number==i],col='red',ylim=c(0,3000),xlim=c(0,200))
}

pythagorean=function(Hz){
  t=1/Hz
#creates duplicate x,y,z columns labelled xx,yy,zz an d shifts them up one in order to simplify the calculation for distance
#create "v" columns for distance measures over time at each marker i.e. velocity#
v1=c(rep(0,length(inter$x1)));v2=c(rep(0,length(inter$x1)));v3=c(rep(0,length(inter$x1)))
v4=c(rep(0,length(inter$x1)));v5=c(rep(0,length(inter$x1)));v6=c(rep(0,length(inter$x1)))
v7=c(rep(0,length(inter$x1)));v8=c(rep(0,length(inter$x1)));v9=c(rep(0,length(inter$x1)))
v10=c(rep(0,length(inter$x1)));v11=c(rep(0,length(inter$x1)));v12=c(rep(0,length(inter$x1)))
xx1=inter$x1;yy1=inter$y1;zz1=inter$z1
xx2=inter$x2;yy2=inter$y2;zz2=inter$z2
xx3=inter$x3;yy3=inter$y3;zz3=inter$z3
xx4=inter$x4;yy4=inter$y4;zz4=inter$z4
xx5=inter$x5;yy5=inter$y5;zz5=inter$z5
xx6=inter$x6;yy6=inter$y6;zz6=inter$z6
xx7=inter$x7;yy7=inter$y7;zz7=inter$z7
xx8=inter$x8;yy8=inter$y8;zz8=inter$z8
xx9=inter$x9;yy9=inter$y9;zz9=inter$z9
xx10=inter$x10;yy10=inter$y10;zz10=inter$z10
xx11=inter$x11;yy11=inter$y11;zz11=inter$z11
xx12=inter$x12;yy12=inter$y12;zz12=inter$z12

length=length(xx1)
xx1=xx1[2:length];yy1=yy1[2:length];zz1=zz1[2:length]
xx2=xx2[2:length];yy2=yy2[2:length];zz2=zz2[2:length]
xx3=xx3[2:length];yy3=yy3[2:length];zz3=zz3[2:length]
xx4=xx4[2:length];yy4=yy4[2:length];zz4=zz4[2:length]
xx5=xx5[2:length];yy5=yy5[2:length];zz5=zz5[2:length]
xx6=xx6[2:length];yy6=yy6[2:length];zz6=zz6[2:length]
xx7=xx7[2:length];yy7=yy7[2:length];zz7=zz7[2:length]
xx8=xx8[2:length];yy8=yy8[2:length];zz8=zz8[2:length]
xx9=xx9[2:length];yy9=yy9[2:length];zz9=zz9[2:length]
xx10=xx10[2:length];yy10=yy10[2:length];zz10=zz10[2:length]
xx11=xx11[2:length];yy11=yy11[2:length];zz11=zz11[2:length]
xx12=xx12[2:length];yy12=yy12[2:length];zz12=zz12[2:length]
xx1=c(xx1,0);yy1=c(yy1,0);zz1=c(zz1,0)
xx2=c(xx2,0);yy2=c(yy2,0);zz2=c(zz2,0)
xx3=c(xx3,0);yy3=c(yy3,0);zz3=c(zz3,0)
xx4=c(xx4,0);yy4=c(yy4,0);zz4=c(zz4,0)
xx5=c(xx5,0);yy5=c(yy5,0);zz5=c(zz5,0)
xx6=c(xx6,0);yy6=c(yy6,0);zz6=c(zz6,0)
xx7=c(xx7,0);yy7=c(yy7,0);zz7=c(zz7,0)
xx8=c(xx8,0);yy8=c(yy8,0);zz8=c(zz8,0)
xx9=c(xx9,0);yy9=c(yy9,0);zz9=c(zz9,0)
xx10=c(xx10,0);yy10=c(yy10,0);zz10=c(zz10,0)
xx11=c(xx11,0);yy11=c(yy11,0);zz11=c(zz11,0)
xx12=c(xx12,0);yy12=c(yy12,0);zz12=c(zz12,0)


analyze=cbind(inter,xx1,yy1,zz1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12,yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,zz1,zz2,zz3,zz4,zz5,zz6,zz7,zz8,zz9,zz10,zz11,zz12,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
rm(xx1,yy1,zz1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12,zz2,zz3,zz4,zz5,zz6,zz7,zz8,zz9,zz10,zz11,zz12,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)

#calculates distance per marker over time#
analyze$v1=sqrt((analyze$x1-analyze$xx1)^2+(analyze$y1-analyze$yy1)^2+(analyze$z1-analyze$zz1)^2)/t
analyze$v2=sqrt((analyze$x2-analyze$xx2)^2+(analyze$y2-analyze$yy2)^2+(analyze$z2-analyze$zz2)^2)/t
analyze$v3=sqrt((analyze$x3-analyze$xx3)^2+(analyze$y3-analyze$yy3)^2+(analyze$z3-analyze$zz3)^2)/t
analyze$v4=sqrt((analyze$x4-analyze$xx4)^2+(analyze$y4-analyze$yy4)^2+(analyze$z4-analyze$zz4)^2)/t
analyze$v5=sqrt((analyze$x5-analyze$xx5)^2+(analyze$y5-analyze$yy5)^2+(analyze$z5-analyze$zz5)^2)/t
analyze$v6=sqrt((analyze$x6-analyze$xx6)^2+(analyze$y6-analyze$yy6)^2+(analyze$z6-analyze$zz6)^2)/t
analyze$v7=sqrt((analyze$x7-analyze$xx7)^2+(analyze$y7-analyze$yy7)^2+(analyze$z7-analyze$zz7)^2)/t
analyze$v8=sqrt((analyze$x8-analyze$xx8)^2+(analyze$y8-analyze$yy8)^2+(analyze$z8-analyze$zz8)^2)/t
analyze$v9=sqrt((analyze$x9-analyze$xx9)^2+(analyze$y9-analyze$yy9)^2+(analyze$z9-analyze$zz9)^2)/t
analyze$v10=sqrt((analyze$x10-analyze$xx10)^2+(analyze$y10-analyze$yy10)^2+(analyze$z10-analyze$zz10)^2)/t
analyze$v11=sqrt((analyze$x11-analyze$xx11)^2+(analyze$y11-analyze$yy11)^2+(analyze$z11-analyze$zz11)^2)/t
analyze$v12=sqrt((analyze$x12-analyze$xx12)^2+(analyze$y12-analyze$yy12)^2+(analyze$z12-analyze$zz12)^2)/t

#create columns for and analyze distance between markers#
#index and thumb#
d.index.thumb=c(rep(0,length(inter$x1)))
v.index.thumb=c(rep(0,length(inter$x1)))
d.index.thumb=sqrt((analyze$x1-analyze$x3)^2+(analyze$y1-analyze$y3)^2+(analyze$z1-analyze$z3)^2)
analyze=cbind(analyze,d.index.thumb)
dd.index.thumb=analyze$d.index.thumb
dd.index.thumb=dd.index.thumb[2:length]
dd.index.thumb=c(dd.index.thumb,0)
analyze=cbind(analyze,dd.index.thumb,v.index.thumb)
analyze$v.index.thumb=sqrt((analyze$d.index.thumb-analyze$dd.index.thumb)^2)/t
rm(length)
analyze<<-analyze
}

conditions=function(){
  x=length(analyze$optotrak.pulse.number)
  vision=c(rep("?",x))
  hand=c(rep("?",x))
  blocksize=c(rep("?",x))
  block=c(rep("?",x))
  analyze<<-cbind(analyze,vision,hand,blocksize,block)
  analyze$vision<<-as.factor(analyze$vision)
  levels(analyze$vision)<<-c("?","visible","hidden")
  levels(analyze$hand)<<-c("?","left","right")
  levels(analyze$blocksize)<<-c("?","2cm","1cm","4cm")
  levels(analyze$block)<<-c("?","Block1","Block2","Block3","Block4")
  b1.1 = readline(prompt="What trials does block one start on?: ")
  b1.2 = readline(prompt="What trials does block one end on?: ")
  b2.1 = readline(prompt="What trials does block two start on?: ")
  b2.2 = readline(prompt="What trials does block two end on?: ")
  b3.1 = readline(prompt="What trials does block three start on?: ")
  b3.2 = readline(prompt="What trials does block three end on?: ")
  b4.1 = readline(prompt="What trials does block four start on?: ")
  b4.2 = readline(prompt="What trials does block four end on?: ")
  b1=as.integer(b1.1):as.integer(b1.2)
  b2=as.integer(b2.1):as.integer(b2.2)
  b3=as.integer(b3.1):as.integer(b3.2)
  b4=as.integer(b4.1):as.integer(b4.2)
  b1.vision = readline(prompt="Is block one visible or hidden?: ")
  b2.vision = readline(prompt="Is block two visible or hidden?: ")
  b3.vision = readline(prompt="Is block three visible or hidden?: ")
  b4.vision = readline(prompt="Is block four visible or hidden?: ")
  
  b1.hand = readline(prompt="Is block one and two usign the left or right hand?: ")
  b3.hand = readline(prompt="Is block three and four usign the left or right hand?: ")
  
  for(i in 1:length(b1)){
    analyze$block[analyze$optotrak.pulse.number == b1[i]] <<- "Block1"
  }
  for(i in 1:length(b2)){
    analyze$block[analyze$optotrak.pulse.number == b2[i]] <<- "Block2"
  }
  for(i in 1:length(b3)){
    analyze$block[analyze$optotrak.pulse.number == b3[i]] <<- "Block3"
  }
  for(i in 1:length(b4)){
    analyze$block[analyze$optotrak.pulse.number == b4[i]] <<- "Block4"
  }

  analyze$vision[analyze$block=="Block1"]<<-b1.vision
  analyze$vision[analyze$block=="Block2"]<<-b2.vision
  analyze$vision[analyze$block=="Block3"]<<-b3.vision
  analyze$vision[analyze$block=="Block4"]<<-b4.vision
  
  analyze$hand[analyze$block=="Block1"]<<-b1.hand
  analyze$hand[analyze$block=="Block2"]<<-b1.hand
  analyze$hand[analyze$block=="Block3"]<<-b3.hand
  analyze$hand[analyze$block=="Block4"]<<-b3.hand
}


statistics=function(){
  for(i in 1:N){
    stat_data$blocksize[i]<<-cut$blocksize[cut$optotrak.pulse.number==kept.list[i]][1]
  }
  for(i in 1:N){
    stat_data$vision[i]<<-cut$vision[cut$optotrak.pulse.number==kept.list[i]][1]
  }
  for(i in 1:N){
    stat_data$hand[i]<<-cut$hand[cut$optotrak.pulse.number==kept.list[i]][1]
  }
  
  for(i in 1:N){
    stat_data$max_ap_size[i]<<-max(cut$d.index.thumb[cut$optotrak.pulse.number==kept.list[i]],na.rm=T)
  }
  
  for(i in 1:N){
    stat_data$max_ap_time[i]<<-cut$Hz[cut$optotrak.pulse.number==kept.list[i] & cut$d.index.thumb == stat_data$max_ap_size[i]][1]
  }
  for(i in 1:N){
    stat_data$max_velocity[i]<<-max(cut$v5[cut$optotrak.pulse.number==kept.list[i]], na.rm=T)
  }
  
  
stat_data$transport_duration<<-abc.v1.low
  
}










