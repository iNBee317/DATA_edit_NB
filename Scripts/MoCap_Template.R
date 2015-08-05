#Entry Required: enter the partiicpant number with quotation marks "03_31" or "03_45", etc.
participant="03_25"
#----Packages Required (No Entry Required)----
library(zoo)

#----Call scripts to import functions (No Entry Required)----
setwd("/Volumes/Swap/Opto_data/Scripts")
source("MoCapFunctions.R")

#----Setup-----
#Reads in .txt file for a subject and creates two data frames. master is complete with columns names. keepers removes the na columns.
#Enter the participant number... "03_32", etc.
optoimport(participant)


#interpolate the missing values based on
#Hz for all trials, outputs a data frame called inter
 #N = where to iter to
 #S = the trial to start interpolating for
 #M = sets the max number of consecutive NA's to interpolate values for

#reference how many trials there are#


interpolate(N=max(as.numeric(keepers$optotrak.pulse.number)),S=1,M=50)

pythagorean(500)

##remove bad trials as determined by notes, presentation output, and graph analysis
bad=c(1,27,52,53) #trials to remove
x=length(bad)
analyze.remove=analyze

for (i in 1:x){
  analyze.remove=analyze.remove[analyze.remove$optotrak.pulse.number!=bad[i],] 
}
analyze.remove$optotrak.pulse.number=factor(analyze.remove$optotrak.pulse.number)
kept.list=levels(analyze.remove$optotrak.pulse.number)
kept.list=as.numeric(kept.list) #numeric vector of trials that have been kept


N=length(kept.list)
abc.v1=c(rep(0,N))
for(i in 1:N){
d=analyze.remove[analyze.remove$v1 > 1000 & analyze.remove$optotrak.pulse.number== kept.list[i],]
q=d$Hz[1]
x=analyze.remove[analyze.remove$v1 < 200 & analyze.remove$optotrak.pulse.number== kept.list[i] & analyze.remove$Hz > q,]
abc.v1[i]=x$Hz[1]
}

abc.v1.low=c(rep(0,N))
for(i in 1:N){
  x=analyze.remove[analyze.remove$Hz >= abc.v1[i] & analyze.remove$Hz <= abc.v1[i]+50 & analyze.remove$optotrak.pulse.number== kept.list[i],]
  z=x$Hz[x$v1==min(x$v1)]
  abc.v1.low[i]=z[1]
}

m.gap=40 # the number of frames after the cutoff at which you avg the distance between the thumb and index to determine cube size
dis.con=c(rep(0,N)) #placeholder
for(i in 1:N){
d=analyze.remove[analyze.remove$optotrak.pulse.number==kept.list[i],]
d=d$d.index.thumb[d$Hz >= abc.v1.low[i] & d$Hz <= abc.v1.low[i]+m.gap]
dis.con[i]=mean(d)
}
dis.con.cm=dis.con/10

for(i in 1:N){
  graphs.full(num=kept.list[i],x=i,optoLog=F)
}

#reports the condiitions
print(optoLog[optoLog$V1 >=10 & optoLog$V1 <=14,])
#function that creates the appropriate condition columns
conditions()

##import presentation output log to determine good trials##
optosleuthe(participant)

class(optoLog$V1)
optoLog$V1=as.character(optoLog$V1)
optoLog$V1=as.numeric(optoLog$V1)

optoLog.remove=optoLog[optoLog$V1 >= 61 & optoLog$V1 <= 63 | optoLog$V1 >= 9000 &  optoLog$V1 <= 9600,]
optoLog.remove=optoLog[optoLog$V1 >= 61 & optoLog$V1 <= 63 ,]
a=c(1:length(kept.list))
optoLog.remove=cbind(a,optoLog.remove)


optoLog.remove=cbind(optoLog.remove,dis.con.cm)
analyze.remove$optotrak.pulse.number=as.numeric(analyze.remove$optotrak.pulse.number)

for(i in 1:N){
  if(optoLog.remove$translation[i]=="move epoch: 3cm cube"){
    optoLog.remove$translation[i]<- "4cm"
  }
  if(optoLog.remove$translation[i]=="move epoch: 2cm cube"){
    optoLog.remove$translation[i]<- "2cm"
  }
  if(optoLog.remove$translation[i]=="move epoch: 1cm cube"){
    optoLog.remove$translation[i]<- "1cm"
  }
  analyze.remove$blocksize[analyze.remove$optotrak.pulse.number==kept.list[i]]<-optoLog.remove$translation[i]
}

for(i in 1:N){
  graphs.full(num=kept.list[i],x=i,optoLog=T)
}

for(i in 1:N){
  graphs.cut(num=kept.list[i])
}

plot(dis.con.cm)

one=optoLog.remove[optoLog.remove$a < 52,]
two=optoLog.remove[optoLog.remove$a >= 52,]

one.1cm=one[one$V1==61,];one.2cm=one[one$V1==62,];one.3cm=one[one$V1==63,]
two.1cm=two[two$V1==61,];two.2cm=two[two$V1==62,];two.3cm=two[two$V1==63,]

plot(one.1cm$dis.con.cm, pch=0,ylim=c(0,8),xlim=c(0,14),col="blue")
text( x = 8, y = 8, labels = paste0("Mean 1cm = ",mean(one.1cm$dis.con.cm)), cex = 1.5, col = "blue" )
abline(h=mean(one.1cm$dis.con.cm),col="blue")
par(new=TRUE)
plot(one.2cm$dis.con.cm, pch=1,ylim=c(0,8),xlim=c(0,14),col="red")
text( x = 8, y = 7.5, labels = paste0("Mean 2cm = ",mean(one.2cm$dis.con.cm)), cex = 1.5, col = "red" )
abline(h=mean(one.2cm$dis.con.cm),col="red")
par(new=TRUE)
plot(one.3cm$dis.con.cm, pch=2,ylim=c(0,8),xlim=c(0,14),col="black")
text( x = 8, y = 7, labels = paste0("Mean 3cm = ",mean(one.3cm$dis.con.cm)), cex = 1.5, col = "black" )
abline(h=mean(one.3cm$dis.con.cm),col="black")

plot(two.1cm$dis.con.cm, pch=0,ylim=c(0,9),xlim=c(0,14),col="blue")
text( x = 8, y = 2, labels = paste0("Mean 1cm = ",mean(two.1cm$dis.con.cm)), cex = 1.5, col = "blue" )
abline(h=mean(two.1cm$dis.con.cm),col="blue")
par(new=TRUE)
plot(two.2cm$dis.con.cm, pch=1,ylim=c(0,9),xlim=c(0,14),col="red")
text( x = 8, y = 1.5, labels = paste0("Mean 2cm = ",mean(two.2cm$dis.con.cm)), cex = 1.5, col = "red" )
abline(h=mean(two.2cm$dis.con.cm),col="red")
par(new=TRUE)
plot(two.3cm$dis.con.cm, pch=2,ylim=c(0,9),xlim=c(0,14),col="black")
text( x = 8, y = 1, labels = paste0("Mean 3cm = ",mean(two.3cm$dis.con.cm)), cex = 1.5, col = "black" )
abline(h=mean(two.3cm$dis.con.cm),col="black")


cut=analyze.remove[analyze.remove$optotrak.pulse.number==kept.list[1] & analyze.remove$Hz <= abc.v1.low[1],]
for(i in 2:N){
  x=analyze.remove[analyze.remove$optotrak.pulse.number== kept.list[i] & analyze.remove$Hz <= abc.v1.low[i],]
  cut=rbind(cut,x)
}


#organize the DV's into a data frame
blocksize=c(rep(0,N))
vision=c(rep(0,N))
hand=c(rep(0,N))
transport_duration=c(rep(0,N))
max_ap_size=c(rep(0,N))
max_ap_time=c(rep(0,N))
max_velocity=c(rep(0,N))
stat_data=data.frame(blocksize,vision,hand,max_ap_size,max_ap_time,transport_duration,max_velocity)
stat_data$blocksize=factor(stat_data$blocksize)
levels(stat_data$blocksize)=c("?","1cm","2cm","4cm")
stat_data$vision=factor(stat_data$vision)
levels(stat_data$vision)=c("?","visible","hidden")
stat_data$hand=factor(stat_data$hand)
levels(stat_data$hand)=c("?","left","right")
statistics()


require(psych)
#descriptive statistics for DV's based on hand used
tapply(stat_data$max_ap_size,list(stat_data$hand),describe,na.rm=T)
tapply(stat_data$max_ap_time,list(stat_data$hand),describe,na.rm=T)
tapply(stat_data$max_velocity,list(stat_data$hand),describe,na.rm=T)
tapply(stat_data$transport_duration,list(stat_data$hand),describe,na.rm=T)

#descriptive statistics for DV's based on vision
tapply(stat_data$max_ap_size,list(stat_data$vision),describe,na.rm=T)
tapply(stat_data$max_ap_time,list(stat_data$vision),describe,na.rm=T)
tapply(stat_data$max_velocity,list(stat_data$vision),describe,na.rm=T)
tapply(stat_data$transport_duration,list(stat_data$vision),describe,na.rm=T)

#descriptive statistics for DV's based on block size
tapply(stat_data$max_ap_size,list(stat_data$blocksize),describe,na.rm=T)
tapply(stat_data$max_ap_time,list(stat_data$blocksize),describe,na.rm=T)
tapply(stat_data$max_velocity,list(stat_data$blocksize),describe,na.rm=T)
tapply(stat_data$transport_duration,list(stat_data$blocksize),describe,na.rm=T)





tapply(stat_data$max_ap_size,list(stat_data$hand,stat_data$vision,stat_data$blocksize),describe,na.rm=T)





##create trial number column
distance1=c(rep("?",x))
for i in 1:x
distance1[i]=


plot(inter$x1[inter$optotrak.pulse.number==8])

#----PostProcessing----

##creates .csv files for each trial##
##within the home directory##

optotrialwrite()



##output for testing purposes
write.csv(keepers,file="keepers.csv",row.names=F)
write.csv(clean,file="clean.csv",row.names=F)
write.csv(inter,file="inter.csv",row.names=F)
write.csv(optoLog,file="optoLog.csv",row.names=F)
write.csv(analyze,file="analyze.csv",row.names=F)
write.csv(analyze.remove,file="analyzeremove.csv",row.names=F)
write.csv(cut,file="cut.csv",row.names=F)
write.csv(analyze[analyze$optotrak.pulse.number==8,],file="analyze.csv",row.names=F)


#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]
