<<<<<<< Updated upstream
#Age-Dependent death
#taking the place of DeathByAge and RandomDeath and Death
#used for Andrea Caribou Genetics chapter 
#code from Lamka Willoughby 2023


#this will impose an increased in probability of death with increasing age AND kill indv over the maxage
AgeDeath = function(pop, source, maxage, y){
  
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]                  #isolate alive
  
  babes = pop[pop[,4] == 0, , drop=FALSE]      #remove babies from chance of dying
  pop = pop[pop[,4] != 0, , drop=FALSE]      #isolate those that are not newly generated babies
  
  
  if(nrow(pop) > 1){
    for(ee in 1:nrow(pop)){
      age = pop[ee,4] #+0.000001 #this controls for if age = 0 in source migrants in that generation
      pop[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(age/maxage,(1-(age/maxage)))) ####error on the numeric stuff again :( #####
      
      if(pop[ee,8]==0){
        pop[ee,10] = y    #this is to put year died if I create that column
      }
    }
    nkilled = NULL
    nkilled = pop[pop[,8]==0,,drop=FALSE]
    totalkilled = nrow(nkilled)   #consider if you want to track this for Analyze.R
    print(paste("killed", totalkilled, "individuals"))
    
  }else{
    print(paste("no killing in AgeDeath"))
  }
  #combine pop and previously removed dead indv
  pop = rbind(pop, babes, dead)
  
  #if(nrow(alreadydead) >= 1){
  #pop = rbind(pop,alreadydead)
  #}
  
  remove(babes, dead, nkilled, totalkilled, age)
#   return(pop)
# }
  
  
##age death for source population 

  sdeadS = source[source[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  source = source[source[,8] == 1, , drop=FALSE]                  #isolate alive
  
  sbabes = source[source[,4] == 0, , drop=FALSE]      #remove babies from chance of dying
  source = source[source[,4] != 0, , drop=FALSE]      #isolate those that are not newly generated babies
  
  
  if(nrow(source) > 1){
    for(see in 1:nrow(source)){
      sage = source[see,4] #+0.000001 #this controls for if age = 0 in source migrants in that generation
      source[see,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(sage/maxage,(1-(sage/maxage))))
      
      if(source[see,8]==0){
        source[see,10] = y    #this is to put year died if I create that column
      }
    }
    snkilled = NULL
    snkilled = source[source[,8]==0,,drop=FALSE]
    stotalkilled = nrow(snkilled)   #consider if you want to track this for Analyze.R
    print(paste("skilled", stotalkilled, "sindividuals"))
    
  }else{
    print(paste("s no killing in AgeDeath"))
  }
  #combine pop and previously removed dead indv
  source = rbind(source, sbabes, sdeadS)
  
  #if(nrow(alreadydead) >= 1){
  #pop = rbind(pop,alreadydead)
  #}
  
  remove(sbabes, sdeadS, snkilled, stotalkilled, sage)
  
  return(list(pop, source))
}
##DOUBLE CHECK THAT NOT LOSING INDVS
#RARELY, but still a thing, a few individuals die that shouldnt (i.e. they are not at max age and are killed with oldies but the number to kill is smaller than oldies, so that shouldnt happen). keep and eye on this and figure out why this occurs




#OLD CODE _ As of 2/20/23
#kill some more individuals
#nkill = round((nrow(pop) * ratemort), 0) - nrow(oldies)
#if((nkill>0)){
#kill  = sample(1:length(pop[,1]), nkill, replace=FALSE) #this is for RANDOM death

#USE_OLD##kill  = sample(1:length(pop[,1]), nkill, replace=FALSE, prob=(1-pop[,11])) #this is for fitness-influenced death
#pop[kill,8]  = 0
#pop[kill,10] = y   #this is if I have a generation died column

#mort.rate = (maxage - pop[,4])*.25  #this makes chance of death at age 0, 0==== ##pop[,4]/maxage                        #calculate mortality rate (age/maxage)

#find old individuals and mark as dead
#oldies = NULL
#oldies = (pop[pop[,4]>=maxage, , drop=FALSE])

#if(length(oldies) == 0){
#  oldies_killed = 0
#}else{
#  pop[pop[,1] %in% oldies,8]  = 0     #oldies become dead
#  pop[pop[,1] %in% oldies,10] = y    #this is to put year died if I create that column
#  oldies = pop[(pop[,8] == 0), , drop = FALSE]
#  pop = pop[(pop[,8] == 1), , drop = FALSE]
#  oldies_killed = nrow(oldies)
#}
#print(paste("there are", oldies_killed, "oldies killed"))

#notes 9/16/22 
#do oldies > fitness at maturity age > age-based death (age/lifespan) === DO NOT PUT A LIMIT ON THE NUMBER TO KILL
#
=======
>>>>>>> Stashed changes
