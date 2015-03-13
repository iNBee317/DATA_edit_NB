##Reads in .txt file for a subject and creates two data
##frames. master is complete with columns names.
#keepers removes the na columns.
optoimport=function(name){
  master=read.table(paste(name,".txt",sep=""),header=F,sep="")
  newnames=c("optotrak.pulse.number",paste(c("x","y","z","na","naa"),rep(1:12,each=5),sep=""))
  names(master)=newnames
  master$optotrak.pulse.number=as.factor(master$optotrak.pulse.number)
  master<<-master
  keeps=c("optotrak.pulse.number",paste(c("x","y","z"),rep(1:12,each=3),sep=""))
  keepers<<-master[,keeps,drop=F]
}