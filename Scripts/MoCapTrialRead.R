
#Read in all Trials for a given participant
subnum={"37"}
trialnum=101

for(i in 1:trialnum){
  assign(paste("S",subnum,"T", i, sep = ""),read.table(paste("Trial", i,".csv", sep = ""),header=T,sep=","))}

#Replace 10,000 columns

for(i in 1:2){
paste("S",subnum,"T", i, sep = "")
b=a[a$x1]
}

