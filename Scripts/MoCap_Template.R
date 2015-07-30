#Entry Required: enter the partiicpant number with quotation marks "03_31" or "03_45", etc.
participant="03_31"
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
max(as.numeric(keepers$optotrak.pulse.number))

interpolate(N=99,S=1,M=Inf)


#create columns for and analyze distance between markers#
#index and thumb#



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
analyze$v1=sqrt((analyze$x1-analyze$xx1)^2+(analyze$y1-analyze$yy1)^2+(analyze$z1-analyze$zz1)^2)/.002
analyze$v2=sqrt((analyze$x2-analyze$xx2)^2+(analyze$y2-analyze$yy2)^2+(analyze$z2-analyze$zz2)^2)/.002
analyze$v3=sqrt((analyze$x3-analyze$xx3)^2+(analyze$y3-analyze$yy3)^2+(analyze$z3-analyze$zz3)^2)/.002
analyze$v4=sqrt((analyze$x4-analyze$xx4)^2+(analyze$y4-analyze$yy4)^2+(analyze$z4-analyze$zz4)^2)/.002
analyze$v5=sqrt((analyze$x5-analyze$xx5)^2+(analyze$y5-analyze$yy5)^2+(analyze$z5-analyze$zz5)^2)/.002
analyze$v6=sqrt((analyze$x6-analyze$xx6)^2+(analyze$y6-analyze$yy6)^2+(analyze$z6-analyze$zz6)^2)/.002
analyze$v7=sqrt((analyze$x7-analyze$xx7)^2+(analyze$y7-analyze$yy7)^2+(analyze$z7-analyze$zz7)^2)/.002
analyze$v8=sqrt((analyze$x8-analyze$xx8)^2+(analyze$y8-analyze$yy8)^2+(analyze$z8-analyze$zz8)^2)/.002
analyze$v9=sqrt((analyze$x9-analyze$xx9)^2+(analyze$y9-analyze$yy9)^2+(analyze$z9-analyze$zz9)^2)/.002
analyze$v10=sqrt((analyze$x10-analyze$xx10)^2+(analyze$y10-analyze$yy10)^2+(analyze$z10-analyze$zz10)^2)/.002
analyze$v11=sqrt((analyze$x11-analyze$xx11)^2+(analyze$y11-analyze$yy11)^2+(analyze$z11-analyze$zz11)^2)/.002
analyze$v12=sqrt((analyze$x12-analyze$xx12)^2+(analyze$y12-analyze$yy12)^2+(analyze$z12-analyze$zz12)^2)/.002

d.index.thumb=c(rep(0,length(inter$x1)))
v.index.thumb=c(rep(0,length(inter$x1)))
d.index.thumb=sqrt((analyze$x1-analyze$x3)^2+(analyze$y1-analyze$y3)^2+(analyze$z1-analyze$z3)^2)
analyze=cbind(analyze,d.index.thumb)
dd.index.thumb=analyze$d.index.thumb
dd.index.thumb=dd.index.thumb[2:length]
dd.index.thumb=c(dd.index.thumb,0)
analyze=cbind(analyze,dd.index.thumb,v.index.thumb)
analyze$v.index.thumb=sqrt((analyze$d.index.thumb-analyze$dd.index.thumb)^2)/.002
rm(length)


##remove bad trials as determined by notes, presentation output, and graph analysis
bad=c(1:5,63,67:73) #trials to remove
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


cut=analyze.remove[analyze.remove$optotrak.pulse.number==kept.list[1] & analyze.remove$Hz <= abc.v1.low[1],]
for(i in 2:N){
x=analyze.remove[analyze.remove$optotrak.pulse.number== kept.list[i] & analyze.remove$Hz <= abc.v1.low[i],]
cut=rbind(cut,x)
}



m.gap=40 # the number of frames after the cutoff at which you avg the distance between the thumb and index to determine cube size
dis.con=c(rep(0,N)) #placeholder
for(i in 1:N){
d=analyze.remove[analyze.remove$optotrak.pulse.number==kept.list[i],]
d=d$d.index.thumb[d$Hz >= abc.v1.low[i] & d$Hz <= abc.v1.low[i]+m.gap]
dis.con[i]=mean(d)
}

for(i in 1:N){
  graphs.full(num=kept.list[i],x=i)
}

for(i in kept.list[1]:N){
  graphs.cut(i)
}

dis.con.cm=dis.con/10








#import presentation output log to determine good trials
optosleuthe(participant)

class(optoLog$V1)
optoLog$V1=as.character(optoLog$V1)
optoLog$V1=as.numeric(optoLog$V1)

optoLog.remove=optoLog[optoLog$V1 >= 61 & optoLog$V1 <= 63,]
a=c(1:99)
optoLog.remove=cbind(a,optoLog.remove)
x=length(bad)
for (i in 1:x){
  optoLog.remove=optoLog.remove[optoLog.remove$a !=bad[i],] 
}

optoLog.remove=cbind(optoLog.remove,dis.con.cm)
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






x=length(inter$optotrak.pulse.number)
vision=c(rep("?",x))
hand=c(rep("?",x))
blocksize=c(rep("?",x))
block=c(rep("?",x))
inter=data.frame(inter,vision,hand,blocksize,block)
inter$vision=as.factor(inter$vision)
levels(inter$vision)=c("?","visible","hidden")
levels(inter$hand)=c("?","left","right")
levels(inter$blocksize)=c("?","2cm","1cm","3cm")
levels(inter$block)=c("?","Block1","Block2","Block3","Block4")

inter$block[1:27]="Block1"
inter$block[28:51]="Block2"
inter$block[52:75]="Block3"
inter$block[76:99]="Block4"

inter$vision[inter$block=="Block1"]="visible"
inter$vision[inter$block=="Block2"]="hidden"
inter$vision[inter$block=="Block3"]="hidden"
inter$vision[inter$block=="Block4"]="visible"

inter$hand[inter$block=="Block1"]="left"
inter$hand[inter$block=="Block2"]="left"
inter$hand[inter$block=="Block3"]="right"
inter$hand[inter$block=="Block4"]="right"






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
write.csv(cut[cut$optotrak.pulse.number==58,],file="cut.csv",row.names=F)
write.csv(analyze[analyze$optotrak.pulse.number==8,],file="analyze.csv",row.names=F)


#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]
