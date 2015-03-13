

opto=function(name){
  
  master=read.table(paste(name,".txt",sep=""),header=F,sep="")
  newnames=c("optotrak.pulse.number",paste(c("x","y","z","na","naa"),rep(1:12,each=5),sep=""))
  names(master)=newnames
  master$optotrak.pulse.number=as.factor(master$optotrak.pulse.number)
  keeps=c("optotrak.pulse.number",paste(c("x","y","z"),rep(1:12,each=3),sep=""))
  master=master[,keeps,drop=F]
  
  z=length(levels(master$optotrak.pulse.number))
  for(i in 1:z){
    write.csv(master[master$optotrak.pulse.number==i,],file=paste("Trial",i,".csv",sep=""),row.names=F)
  }
}


