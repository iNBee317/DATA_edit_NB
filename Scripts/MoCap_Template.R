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

##remove bad trials as determined by notes and presentation output
bad=c(1:7,31) #trials to remove
x=length(bad)
level3=keepers
for (i in 1:x){
  level3=level3[level3$optotrak.pulse.number!=bad[i],] 
}

#interpolate the missing values based on
#Hz for all trials, outputs a data frame called inter
 #N = where to iter to
 #S = the trial to start interpolating for
 #M = sets the max number of consecutive NA's to interpolate values for
interpolate(N=99,S=1,M=500)

#import presentation output log to determine good trials
optosleuthe(participant)

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
optotrial()


##output for testing purposes
write.csv(keepers,file="keepers.csv",row.names=F)
write.csv(clean,file="clean.csv",row.names=F)
write.csv(inter,file="inter.csv",row.names=F)
write.csv(optoLog,file="optoLog.csv",row.names=F)


#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]
