#Entry Required: enter the partiicpant number with quotation marks "03_31" or "03_45", etc.
participant="03_28"
#----Packages Required (No Entry Required)----
library(zoo,psych)


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
interpolate(N=max(as.numeric(keepers$optotrak.pulse.number)),S=1,M=50)

pythagorean(500)

##remove bad trials as determined by notes, presentation output, and graph analysis
bad=c(200) #trials to remove
bad.remove(bad)

for(i in rev(1:N)){
  graphs.full(num=kept.list[i],x=i,optoLog=F)
}

##import presentation output log to determine good trials##
optosleuthe(participant)


#function that creates the appropriate condition columns
conditions()

#function that translates the block size column in the optoLog.remove file into 1cm, 2cm, or 4cm 
translate() 

blockaudit=read.csv("blockaudit.csv",header=T)
for(i in 1:N){
analyze$blocksize[analyze$optotrak.pulse.number==kept.list[i]]<-blockaudit$optoblocksize[i]
}

#function to manually alter the cutoff for certain trails after auditing each graph
bad=c(blockaudit$trial[blockaudit$good.or.bad=="BAD"],39)
bad.remove(bad)
alter()

cut=analyze.remove[analyze.remove$optotrak.pulse.number==kept.list[1] & analyze.remove$Hz <= abc.v1.low[1],]
for(i in 2:N){
  x=analyze.remove[analyze.remove$optotrak.pulse.number== kept.list[i] & analyze.remove$Hz <= abc.v1.low[i],]
  cut=rbind(cut,x)
}

for(i in rev(1:N)){
  graphs.full(num=kept.list[22],x=i,optoLog=T)
}

for(i in rev(1:N)){
  graphs.cut(num=kept.list[i])
}

plot(dis.con.cm)

#organize the DV's into a data frame
statistics()
tapply(stat_data$max_ap_size,list(stat_data$hand,stat_data$vision,stat_data$blocksize),describe,na.rm=T)
Y=stat_data
answer=anova(lm(Y$max_ap_size~Y$hand*Y$vision*Y$blocksize))


#----PostProcessing----
##output for testing purposes
write.csv(keepers,file="keepers.csv",row.names=F)
write.csv(clean,file="clean.csv",row.names=F)
write.csv(inter,file="inter.csv",row.names=F)
write.csv(optoLog.remove,file="optoLogRemove.csv",row.names=F)
write.csv(analyze,file="analyze.csv",row.names=F)
write.csv(analyze.remove,file="analyzeremove.csv",row.names=F)
write.csv(cut,file="cut.csv",row.names=F)
write.csv(analyze[analyze$optotrak.pulse.number==8,],file="analyze.csv",row.names=F)


#identifying runs of NA
rlexyz=rle(clean$x1[clean$optotrak.pulse.number==1])
zz=tapply(rlexyz$lengths, rlexyz$values, max)
zz[names(zz)=="NA"]
