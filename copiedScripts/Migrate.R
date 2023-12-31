#Migrate
#used for caribou genetics chapter from Ginas code 

#do this for one migrant per year
#also try to find the max migrants (i.e, 5, or 10)
#also do a probability of getting 0 or 1 migrants per year
#the worry is that if the migranion is too high, the pops might not even be considered seperate

Migrate = function(pop, source, y, miggy, styr){ #edyr, dur these were in Ginas code
  #function of the source pop, the pop, idk what y, and migration miggy on the Cover.R and styr which is the starting year
  #select number of migrants, from 1-5
  #  N = sum(pop[,8])  #gets census size
  #  Nmig = round(N*.05)  #5% of N will be new migrants == vary this later == perhaps as a parameter???
  #  Vmig = round(N*.01)  #adds 1% variance +/- Nmig 
  #  mig = sample(c((Nmig-Vmig):(Nmig+Vmig)), 1, replace=T) #randomly sample within the variance of Nmig the number of migrants that year
  #NOTE-- TO IMPLEMENT THIS, will need to change the number of indv in the source pop, probs around 10k. this will take time tho so I will do this later
  #remember that H DROPS significantly when there is only 1-5 migrants per year when K stays around 1000. this is an argument AGAINST the 1 mig per generation rule
  
  if(miggy == "a"){
    mig = 16  #but this would be 16 individuals from 100 in the source?
    
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
  
    }else if(miggy == "b"){
    # if(y == 175){
      mig = 3 #this was from the genetic exchange of 2/67 animals in the breeding switching
      #this is seen in E to W, W to C, and C to E
      
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        source[migrant,9] <- y   #change gen born to the generation the migrant entered the pop
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    }else{mig=0}
  }else if(miggy == "c"){
    #if(y == 175|y == 201|y==225){
      mig = 6   #4/67 am I doing the math correct?
      
      #Ginas Notes: 25 #1  #sample(c(1:15), 1, replace=T) #put in the number of migrants for this set of runs #OLD
      #note that the 1:mig might affect the number. pay attention to this.
      
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        source[migrant,9] <- y   #change gen born to the generation the migrant entered the pop
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      } 
    }else{mig=0}
  }else if(miggy == "d"){
    #if(y <= styr | y >= edyr + dur){
      mig = 1
      
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        source[migrant,9] <- y   #change gen born to the generation the migrant entered the pop
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
  #}
  else{mig=0}
  }else if(miggy == "e"){
    #if(y == 125){
      mig = 100
      
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        source[migrant,9] <- y   #change gen born to the generation the migrant entered the pop
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    #}
    else{mig=0}
  }else{
    mig = 0
  # }
  # 
  # return(list(pop, mig, source))




####Top is to simulate migrant source into population and bottom is population to source 
############################################################################################################

    if(smiggy == "a"){
      smig = 16  #but this would be 16 individuals from 100 in the source?
      
      print(paste("there are", smig, "smigrants this year"))
      
      for(sm in 1:smig){
        #select smigrant without replacement
        smigrant = sample(1:nrow(source), 1, replace = F)
        
        source[smigrant,9] <- y   #change gen born to the generation the smigrant entered the source
        #Gina wrote this as 9 as generation born
        
        #take smigrant from source and put into source
        source = rbind(source, source[smigrant,])
        #remove smigrant from source
        source = source[-smigrant,]
      }
    }else if(smiggy == "b"){
      # if(y == 175){
      smig = 3 #this was from the genetic exchange of 2/67 animals in the breeding switching
      #this is seen in E to W, W to C, and C to E
      
      print(paste("there are", smig, "smigrants this year"))
      
      for(m in 1:smig){
        #select smigrant without replacement
        smigrant = sample(1:nrow(source), 1, replace = F)
        
        source[smigrant,9] <- y   #change gen born to the generation the smigrant entered the source
        
        #take smigrant from source and put into source
        source = rbind(source, source[smigrant,])
        #remove smigrant from source
        source = source[-smigrant,]
      }
    }else{smig=0}
  }else if(ssmiggy == "c"){
    #if(y == 175|y == 201|y==225){
    ssmig = 6   #4/67 am I doing the math correct?
    
    #Ginas Notes: 25 #1  #sample(c(1:15), 1, replace=T) #put in the number of smigrants for this set of runs #OLD
    #note that the 1:smig smight affect the number. pay attention to this.
    
    print(paste("there are", smig, "smigrants this year"))
    
    for(m in 1:smig){
      #select smigrant without replacement
      smigrant = sample(1:nrow(source), 1, replace = F)
      
      source[smigrant,9] <- y   #change gen born to the generation the smigrant entered the source
      
      #take smigrant from source and put into source
      source = rbind(source, source[smigrant,])
      #remove smigrant from source
      source = source[-smigrant,]
    } 
  }else{smig=0}
}else if(sssmiggy == "d"){
  #if(y <= styr | y >= edyr + dur){
  smig = 1
  
  print(paste("there are", smig, "smigrants this year"))
  
  for(m in 1:smig){
    #select smigrant without replacement
    smigrant = sample(1:nrow(source), 1, replace = F)
    
    source[smigrant,9] <- y   #change gen born to the generation the smigrant entered the source
    
    #take smigrant from source and put into source
    source = rbind(source, source[smigrant,])
    #remove smigrant from source
    source = source[-smigrant,]
  }
  #}
  else{smig=0}
else if(smiggy == "e"){
  #if(y == 125){
  mig = 100
  
  print(paste("there are", mig, "migrants this year"))
  
  for(sm in 1:smig){
    #select migrant without replacement
    smigrant = sample(1:nrow(source), 1, replace = F)
    
    source[smigrant,9] <- y   #change gen born to the generation the migrant entered the source
    
    #take migrant from source and put into source
    source = rbind(source, source[smigrant,])
    #remove migrant from source
    pop = pop[-smigrant,]
  }
  #}
  else{mig=0}
else{
  mig = 0
  

return(list(pop, mig, source, smig))
}


















#things to remember from Allendorf's book, pg 209: the larger the demes, the slower they are diverging through drift; 
#therefore proportionally fewer migrants needed to counteract drift


#see https://doi.org/10.1023/A:1025563107092 for info about how they decided how many migrants to add - constant, attraction, and avoidance


#in function
#return(list(n, pop)) #where n = number of indv added

#in runmodel.R
#temp = function(n, pop)

#n = unlist(temp)[1] #unlist the first object
#pop = unlist(temp)[[2]]


#maybe