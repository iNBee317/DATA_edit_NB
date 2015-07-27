##remove bad trials as determined by notes and presentation output
bad=c(1:7,31) #trials to remove
x=length(bad)
level3=keepers
for (i in 1:x){
  level3=level3[level3$optotrak.pulse.number!=bad[i],] 
}



inter<<-inter[,c(1,2,3,4,5,9,10,11,15,16,17,21,22,23)]


##reads in each as an object##
z=length(levels(inter$optotrak.pulse.number))
for(i in 1:z){
  assign(paste0("T",i),inter[inter$optotrak.pulse.number==i,])
}



