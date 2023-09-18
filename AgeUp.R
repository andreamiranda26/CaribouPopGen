#AgeUp.R for Caribou Genetics 
#already tweaked !

AgeUp = function(pop){
  dead = pop[pop[,9] == 0, , drop=FALSE]                 #define dead indvs use the "alive" parameter in RunModel.R 
  
  if(nrow(dead)>= 1){
    pop = pop[-which(pop[,1]%in%dead), , drop=FALSE]     #remove dead indvs
  }
  
  if(!is.null(nrow(pop))){
    pop[,2] = pop[,2] + 1                                #add one year to all live indv's ages this is a two in RunModel.R
    
  }
  pop<- rbind(pop,dead)                                  #recall dead indvs
  remove(dead)                                           #clean up
  return(pop)
}