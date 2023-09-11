#AgeUp.R for Caribou Genetics 

AgeUp = function(pop){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #define dead indvs
  
  if(nrow(dead)>= 1){
    pop = pop[-which(pop[,1]%in%dead), , drop=FALSE]     #remove dead indvs
  }
  
  if(!is.null(nrow(pop))){
    pop[,4] = pop[,4] + 1                                #add one year to all live indv's ages
    
  }
  pop<- rbind(pop,dead)                                  #recall dead indvs
  remove(dead)                                           #clean up
  return(pop)
}