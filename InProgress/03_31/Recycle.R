


inter<<-inter[,c(1,2,3,4,5,9,10,11,15,16,17,21,22,23)]


##reads in each as an object##
z=length(levels(inter$optotrak.pulse.number))
for(i in 1:z){
  assign(paste0("T",i),inter[inter$optotrak.pulse.number==i,])
}



