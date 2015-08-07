


inter<<-inter[,c(1,2,3,4,5,9,10,11,15,16,17,21,22,23)]


##reads in each as an object##
z=length(levels(inter$optotrak.pulse.number))
for(i in 1:z){
  assign(paste0("T",i),inter[inter$optotrak.pulse.number==i,])
}



##creates .csv files for each trial##
##within the home directory##

optotrialwrite()


##create trial number column
distance1=c(rep("?",x))
for i in 1:x
distance1[i]=

  
  #used to shape the optolog.remove frame
a=c(1:length(kept.list))
optoLog.remove=cbind(a,optoLog.remove)

optoLog.remove=cbind(optoLog.remove,dis.con.cm)
analyze.remove$optotrak.pulse.number=as.numeric(analyze.remove$optotrak.pulse.number)

