
#AgeUp.R for Caribou Genetics 
#already tweaked and debugged!!!

AgeUp = function(pop, source){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #define dead indvs use the "alive" parameter in RunModel.R 
  
  if(nrow(dead)>= 1){
    pop = pop[-which(pop[,1]%in%dead), , drop=FALSE]     #remove dead indvs
  }
  
  # if(!is.numeric(pop[, 4])) {
  #   pop[, 4] <- as.numeric(pop[, 4])
  # }
  
  if(!is.null(nrow(pop))){                                           #I added as numeric because it was giving me this error: Error in pop[, 4] + 1 : non-numeric argument to binary operator
    pop[,4] = as.numeric(pop[,4]) + 1                                #add one year to all live indv's ages this is a two in RunModel.R
    
  }
  pop<- rbind(pop,dead)                                  #recall dead indvs
  remove(dead)                                           #clean up


sdeadS = source[source[,8] == 0, , drop=FALSE]                 #define dead indvs use the "alive" parameter in RunModel.R 

if(nrow(sdeadS)>= 1){
  source = source[-which(source[,1]%in%sdeadS), , drop=FALSE]     #remove dead indvs
}

if(!is.null(nrow(source))){
  source[,4] = as.numeric(source[,4]) + 1                                #add one year to all live indv's ages this is a two in RunModel.R
  
}
source<- rbind(source,sdeadS)                                  #recall dead indvs
remove(sdeadS)                                            #clean up

return(list(pop,source))                                         #might give error at this point: Error: no function to return from, jumping to top level, ignore this error when debugging! 

}
# =======
# >>>>>>> Stashed changes
