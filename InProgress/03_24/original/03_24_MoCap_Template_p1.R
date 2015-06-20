#----Packages Required (No Entry Required)----

library(zoo)

##Reads in .txt file for a subject and creates two data
##frames. master is complete with columns names.
#keepers removes the na columns.

#----Setup-----

setwd("/Users/pcnnc/Documents/Opto_data/InProgress/03_24/original")
source("03_24_MoCapFunctions.R")
optoimport("03_24")

#creates a Hz vector and add it to the data frame for interpolation purposes
a=length(levels(master$optotrak.pulse.number))
Hz=rep(c(1:500),a)
rm(a)
keepers=data.frame(Hz,keepers)
rm(Hz)

#clean, replaces 100000 missing data values with "NA"
clean=keepers
clean[clean==100000]<-"NA"

#import presentation output log to determine good trials
optoLog=read.table("03_24_optoReplant.txt",header=F,sep="")
x=length(optoLog$V1)
translation=c(rep(0,x))
TimeSec=c(rep(0,x))
TimeAbsolute=c(rep(0,x))
optoLog=cbind(translation,optoLog,TimeSec,TimeAbsolute)
optoLog$V1 = factor(optoLog$V1)
optoLog$translation=as.character(optoLog$translation)

optoLog$TimeSec=optoLog$V2/1000


#Labeling translation column in optoLog 
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


##output for testing purposes
write.csv(keepers,file="keepers.csv",row.names=F)
write.csv(clean,file="clean.csv",row.names=F)
#write.csv(inter,file="inter.csv",row.names=F)
write.csv(optoLog,file="optoLog.csv",row.names=F)

#filter out 9000s
optoLog$V1=as.character(optoLog$V1)
optoLog$V1=as.numeric(optoLog$V1)
a_9=optoLog[optoLog$V1>9100 & optoLog$V1<9200,]
output91=length(unique(a_9$V1))
b_9=optoLog[optoLog$V1>9200 & optoLog$V1<9300,]
output92=length(unique(b_9$V1))
c_9=optoLog[optoLog$V1>9300 & optoLog$V1<9400,]
output93=length(unique(c_9$V1))
d_9=optoLog[optoLog$V1>9400 & optoLog$V1<9500,]
output94=length(unique(d_9$V1))

#QA
write.csv(a_9,file="a_9.csv", row.names=F)
write.csv(b_9,file="b_9.csv", row.names=F)
write.csv(c_9,file="c_9.csv", row.names=F)
write.csv(d_9,file="d_9.csv", row.names=F)

rm(a_9,b_9,c_9,d_9)

#filter out 8000s
# Set parameters bounderies for the filter (e.g., using 9100 and 9200 as the border to search for 8000s numbers)
optoLog$V1=as.character(optoLog$V1)
optoLog$V1=as.numeric(optoLog$V1)

a_92=optoLog[optoLog$V1>9100 & optoLog$V1<=9201,]
b_92=optoLog[optoLog$V1>9200 & optoLog$V1<=9301,]
c_92=optoLog[optoLog$V1>9300 & optoLog$V1<=9401,]
d_92=optoLog[optoLog$V1>9400 & optoLog$V1<=9501,]


#Define parameters to locate only 8000s numbers
first=a_92[1,4]
last=a_92[length(a_92$V1),4]
a_8=optoLog[optoLog$TimeSec>=first & optoLog$TimeSec<=last,]
a_81=a_8[a_8$V1>=8000 & a_8$V1<=9000,]
output81=length(unique(a_81$V1))

first=b_92[1,4]
last=b_92[length(b_92$V1),4]
b_8=optoLog[optoLog$TimeSec>=first & optoLog$TimeSec<=last,]
b_81=b_8[b_8$V1>=8000 & b_8$V1<=9000,]
output82=length(unique(b_81$V1))

first=c_92[1,4]
last=c_92[length(c_92$V1),4]
c_8=optoLog[optoLog$TimeSec>=first & optoLog$TimeSec<=last,]
c_81=c_8[c_8$V1>=8000 & c_8$V1<=9000,]
output83=length(unique(c_81$V1))

#Since there is no 9500s, the last boundry is the last row of the optoLog file
first=d_92[1,4]
last=optoLog[nrow(optoLog),4]
d_8=optoLog[optoLog$TimeSec>=first & optoLog$TimeSec<=last,]
d_81=d_8[d_8$V1>=8000 & d_8$V1<=9000,]
output84=length(unique(d_81$V1))

#QA
write.csv(a_81,file="a_81.csv", row.names=F)
write.csv(b_81,file="b_81.csv", row.names=F)
write.csv(c_81,file="c_81.csv", row.names=F)
write.csv(d_81,file="d_81.csv", row.names=F)

rm(a_92,b_92,c_92,d_92,a_8,b_8,c_8,d_8,a_81,b_81,c_81,d_81)

#Write numbers into csv columns
#output the result log
block = c(1,2,3,4)
output9 = c(output91,output92,output93,output94)
output8 = c(output81,output82,output83,output84)
df = data.frame(block,output9, output8)
write.csv(df,file="03_24_outputnumber.csv", row.names=F)


#Check excel
