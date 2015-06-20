
#interpolate the missing values based on
#Hz for all trials, outputs a data frame called inter
level3=clean
inter=level3
inter=inter[,c(1,2,3,4,5,9,10,11,15,16,17,21,22,23)]
inter$optotrak.pulse.number=factor(inter$optotrak.pulse.number)
levels(inter$optotrak.pulse.number)
length(levels(inter$optotrak.pulse.number))

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
inter$z8=as.numeric(inter$z8)
inter$z9=as.numeric(inter$z9)
inter$z10=as.numeric(inter$z10)
inter$z11=as.numeric(inter$z11)
inter$z12=as.numeric(inter$z12)

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


#settings for interpolation
N=99 #where to iter to
S=1  #the trial to start interpolating for
M=500 #sets the max number of consecutive NA's to interpolate values for

#----Interpolate one marker at a time----
#Will through and error if on any of the runs a marker starts out missing (NA).
for (i in S:N){
  inter$x1[inter$optotrak.pulse.number==i] <- na.approx(inter$x1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y1[inter$optotrak.pulse.number==i] <- na.approx(inter$y1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z1[inter$optotrak.pulse.number==i] <- na.approx(inter$z1[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x2[inter$optotrak.pulse.number==i] <- na.approx(inter$x2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y2[inter$optotrak.pulse.number==i] <- na.approx(inter$y2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z2[inter$optotrak.pulse.number==i] <- na.approx(inter$z2[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x3[inter$optotrak.pulse.number==i] <- na.approx(inter$x3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y3[inter$optotrak.pulse.number==i] <- na.approx(inter$y3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z3[inter$optotrak.pulse.number==i] <- na.approx(inter$z3[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x4[inter$optotrak.pulse.number==i] <- na.approx(inter$x4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y4[inter$optotrak.pulse.number==i] <- na.approx(inter$y4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z4[inter$optotrak.pulse.number==i] <- na.approx(inter$z4[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x5[inter$optotrak.pulse.number==i] <- na.approx(inter$x5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y5[inter$optotrak.pulse.number==i] <- na.approx(inter$y5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z5[inter$optotrak.pulse.number==i] <- na.approx(inter$z5[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x6[inter$optotrak.pulse.number==i] <- na.approx(inter$x6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y6[inter$optotrak.pulse.number==i] <- na.approx(inter$y6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z6[inter$optotrak.pulse.number==i] <- na.approx(inter$z6[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x7[inter$optotrak.pulse.number==i] <- na.approx(inter$x7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y7[inter$optotrak.pulse.number==i] <- na.approx(inter$y7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z7[inter$optotrak.pulse.number==i] <- na.approx(inter$z7[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x8[inter$optotrak.pulse.number==i] <- na.approx(inter$x8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y8[inter$optotrak.pulse.number==i] <- na.approx(inter$y8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$z8[inter$optotrak.pulse.number==i] <- na.approx(inter$z8[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
}
for (i in S:N){
  inter$x9[inter$optotrak.pulse.number==i] <- na.approx(inter$x9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$y9[inter$optotrak.pulse.number==i] <- na.approx(inter$y9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$z9[inter$optotrak.pulse.number==i] <- na.approx(inter$z9[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
}
for (i in S:N){
  inter$x10[inter$optotrak.pulse.number==i] <- na.approx(inter$x10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$y10[inter$optotrak.pulse.number==i] <- na.approx(inter$y10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$z10[inter$optotrak.pulse.number==i] <- na.approx(inter$z10[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
}
for (i in S:N){
  inter$x11[inter$optotrak.pulse.number==i] <- na.approx(inter$x11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE,maxgap=M)
  inter$y11[inter$optotrak.pulse.number==i] <- na.approx(inter$y11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$z11[inter$optotrak.pulse.number==i] <- na.approx(inter$z11[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
}
for (i in S:N){
  inter$x12[inter$optotrak.pulse.number==i] <- na.approx(inter$x12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$y12[inter$optotrak.pulse.number==i] <- na.approx(inter$y12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
  inter$z12[inter$optotrak.pulse.number==i] <- na.approx(inter$z12[inter$optotrak.pulse.number==i], inter$Hz[inter$optotrak.pulse.number==i], na.rm = TRUE, maxgap=M)
}

##create trial number column
distance1=c(rep("?",x))
for i in 1:x
distance1[i]=
  
  
  plot(inter$x1[inter$optotrak.pulse.number==8])

#----PostProcessing----
##creates .csv files for each trial##
##within the home directory##
optotrial=function(){
  
  z=length(levels(clean$optotrak.pulse.number))
  for(i in 1:z){
    write.csv(clean[clean$optotrak.pulse.number==i,],file=paste("Trial",i,".csv",sep=""),row.names=F)
  }
}




#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]

##remove bad trials as determined by notes and presentation output
bad=c(1:7,31) #trials to remove
x=length(bad)
level3=clean
for (i in 1:x){
  level3=level3[level3$optotrak.pulse.number!=bad[i],] 
}
