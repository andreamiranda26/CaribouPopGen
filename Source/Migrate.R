#Migrate
#used for caribou genetics chapter from Ginas code 
Migrate = function(pop, source, y, miggy, smiggy, styr){
  
  #move indv from pop > source
  mig = miggy  #but this would be 16 individuals from 100 in the source?
  
  print(paste("there are", mig, "migrants this year"))
  
  for(m in 1:mig){
    #select migrant without replacement
    migrant = sample(1:nrow(source), 1, replace = F)
    
    source[migrant,9] <- y   #change gen born to the generation the migrant entered the pop
    #Gina wrote this as 9 as generation born
    
    #take migrant from source and put into pop
    pop = rbind(pop, source[migrant,])
    #remove migrant from source
    source = source[-migrant,]
  }

  #move indv from pop > source
  smig = smiggy  #but this would be 16 individuals from 100 in the source?
  
  print(paste("there are", smig, "smigrants this year"))
  
  for(sm in 1:smig){
    #select smigrant without replacement
    smigrant = sample(1:nrow(pop), 1, replace = F)
    
    pop[smigrant,9] <- y   #change gen born to the generation the smigrant entered the source
    #Gina wrote this as 9 as generation born
    
    #take smigrant from source and put into source
    source = rbind(source, pop[smigrant,])
    #remove smigrant from source
    pop = pop[-smigrant,]
  }
  
  return(list(pop, source))
}