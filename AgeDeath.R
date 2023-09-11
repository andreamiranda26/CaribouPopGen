#Age-Dependent death
#taking the place of DeathByAge and RandomDeath and Death
#used for caribou genetics chapter 1 

#this will impose an increased in probability of death with increasing age AND kill indv over the maxage
AgeDeath = function(pop, maxage, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]                  #isolate alive
  
  babes = pop[pop[,4] == 0, , drop=FALSE]      #remove babies from chance of dying
  pop = pop[pop[,4] != 0, , drop=FALSE]      #isolate those that are not newly generated babies
  
  
  if(nrow(pop) > 1){
    for(ee in 1:nrow(pop)){
      age = pop[ee,4] #+0.000001 #this controls for if age = 0 in source migrants in that generation
      pop[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(age/maxage,(1-(age/maxage))))
      
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
  
  return(pop)
}


##DOUBLE CHECK THAT NOT LOSING INDVS
#RARELY, but still a thing, a few individuals die that shouldnt (i.e. they are not at max age and are killed with oldies but the number to kill is smaller than oldies, so that shouldnt happen). keep and eye on this and figure out why this occurs

