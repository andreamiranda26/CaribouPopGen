#Mate
#used for Caribou Genetics Andrea, code from Lamka Willoughby 2023
#tweaked 

MateChoice = function(pop, source, sex, maturity) { #allee, matemigs
  dead = pop[pop[,9] == 0, , drop=FALSE]          #remove dead indvs
  pop = pop[which(pop[,1]%NOTin%dead), , drop=FALSE]
  
  immature  = pop[pop[,4] < 2, ,drop=FALSE]          #remove immature indvs
  pop =       pop[pop[,4] >= 2, ,drop=FALSE]         #pop without immature
  #since not returning pop, don't need to re-add pop and immature at end of function

  #find which sex has more, male or female.
  ck = mean(pop[,'sex']) #<0.5 female, >.5 male
  print(paste("the sex ratio is", ck))
  if(ck == 1){
    print(paste("Only males left"))
    return()   ##ERROR HERE BECAUSE BREAK/NEXT ARENT IN A LOOP (if is not a loop)
  }else if(ck == 0){
    print(paste("Only females left"))
    return() #break #next
  }else
    {
    #REMOVED###turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
    #REMOVED###sex <- c(0,1)[which.min(tabulate(match(pop[,'sex'], c(0,1))))]
    
    #fem = 0 #this matches males to females by putting the first column always as females, this would make them all females 
    
    #match those of opposite sex with replacement*
   
    #first, grab males so that there are the same number of mates as there are females 
    #mates <- sample(pop[pop[,'sex'] != fem, 'id'], tabulate(match(pop[,'sex'], fem)), replace=TRUE) #this means indv can mate more than once
    #for now, replace = true since sometimes more males than females
    #note, != means NOT 
    
    # # Define the proportion of males you want relative to females
    # male_ratio <- 0.3  # This value based off of literature 30% bulls 
    # 
    # # Select females from the population
    # females <- pop[pop$sex == 'female', ]
    # 
    # # Select males from the population
    # males <- pop[pop$sex == 'male', ]
    # 
    # # Determine the number of males to select based on the proportion
    # num_males_select <- round(male_ratio * nrow(females))
    # 
    # # Sample males
    # selected_males <- sample(males$id, num_males_select, replace = TRUE)
    # 
    # # Combine selected males and females as mates
    # pairs <- c(selected_males, females$id)
    # 
    # # Shuffle the mates
    # pairs <- sample(mates)
    
    #pair individuals - females with males so n_pairs = n_females
    #pairs <- cbind(pop[pop[,'sex'] == fem, 'id'], mates)
    
    print(paste("there are", nrow(pairs), "pairs"))
    
    
    
    #if allee effect switch is turned on (1 = on, 0 = off)
    if(allee == 1){
      al = nrow(pairs)
      matedpairs <- cbind(pairs, rep(1, al))
      for(lee in 1:nrow(matedpairs)){
        #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
        if(matemigs == 1){
          #grab pairs with migrant parents
          mom = matedpairs[lee,1]
          dad = matedpairs[lee,2]
          if(mom >= 1 & dad >= 1){
            #find random chance of mates finding each other so that as Nc ^, chance of interacting ^
            matedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al)))) 
          }else{
            #give migrant parents unique identitifier
            matedpairs[lee,3] = 2
          }
          remove(mom, dad)
        }else{
          #find random chance of mates finding each other so that as Nc ^, chance of interacting ^ -- happens for all if switch is turned off
          matedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al))))
        }
      }
      keptpairs = matedpairs[matedpairs[,3]>=1,,drop=FALSE]
      pairs = keptpairs #keptpairs[,c(1:2)]  <- use this if don't want migrant identifier
      
      remove(lee, matedpairs, keptpairs)
    }else{
      al = nrow(pairs)
      pairs <- cbind(pairs, rep(1, al))
    }
    
    if(nrow(pairs) >= 3){
      #randomize the pairs
      rand = sample(1:nrow(pairs),nrow(pairs))
      pairs <- pairs[rand, ]
      
      #pairs <- rand
    }
    colnames(pairs) <- c('mom','dad','migident')
    
    remove(dead, immature, mates, rand)
    }
  

    
    
    
 #######Code below is for source population 
    
    sdeadS = source[source[,9] == 0, , drop=FALSE]          #remove sdeadS indvs
    source = source[which(source[,1]%NOTin%sdeadS), , drop=FALSE]
    
    simmature  = source[source[,2 < 13, ,drop=FALSE]          #remove simmature indvs
        source = source[source[,2 >= 13, ,drop=FALSE]         #source without simmature
                               #since not returning source, don't need to re-add source and simmature at end of function
                                    
                                   #find which sex has more, male or female.
                                    sck = mean(source[,'sex']) #<0.5 female, >.5 male
                                    print(paste("the sex ratio is", sck))
                                    if(sck == 1){
                                      print(paste("Only males left"))
                                      return()   ##ERROR HERE BECAUSE BREAK/NEXT ARENT IN A LOOP (if is not a loop)
                                    }else if(sck == 0){
                                      print(paste("Only females left"))
                                      return() #break #next
                                    }else{
                                      #REMOVED###turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
                                      #REMOVED###sex <- c(0,1)[which.min(tabulate(match(source[,'sex'], c(0,1))))]
                                      
                                      #fem = 0 #this matches males to females by putting the first column always as females, this would make them all females 
                                      
                                      #match those of opposite sex with replacement*
                                      
                                      #first, grab males so that there are the same number of mates as there are females 
                                      #mates <- sample(source[source[,'sex'] != fem, 'id'], tabulate(match(source[,'sex'], fem)), replace=TRUE) #this means indv can mate more than once
                                      #for now, replace = true since sometimes more males than females
                                      #note, != means NOT 
                                      
                                      # Define the proportion of males you want relative to females
                                      smale_ratio <- 0.3  # This value based off of literature 30% bulls 
                                      
                                      # Select females from the source
                                      sfemales <- source[source$sex == 'female', ]
                                      
                                      # Select males from the source
                                      smales <- source[source$sex == 'male', ]
                                      
                                      # Determine the number of males to select based on the proportion
                                      snum_males_select <- round(smale_ratio * nrow(sfemales))
                                      
                                      # Sample males
                                      sselected_males <- sample(smales$id, snum_males_select, replace = TRUE)
                                      
                                      # Combine selected males and females as mates
                                      pairs <- c(sselected_males, sfemales$id)
                                      
                                      # Shuffle the mates
                                      pairs <- sample(smates)
                                      
                                      #pair individuals - females with males so n_pairs = n_females
                                      #pairs <- cbind(source[source[,'sex'] == fem, 'id'], mates)
                                      
                                      print(paste("there are", nrow(spairs), "spairs"))
                                      
                                      
                                      
                                      #if allee effect switch is turned on (1 = on, 0 = off)
                                      if(allee == 1){
                                        al = nrow(spairs)
                                        smatedpairs <- cbind(spairs, rep(1, al))
                                        for(lee in 1:nrow(smatedpairs)){
                                          #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
                                          if(smatemigs == 1){
                                            #grab pairs with migrant parents
                                            smom = smatedpairs[lee,1]
                                            sdad = smatedpairs[lee,2]
                                            if(smom >= 1 & sdad >= 1){
                                              #find random chance of mates finding each other so that as Nc ^, chance of interacting ^
                                              smatedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al)))) 
                                            }else{
                                              #give migrant parents unique identitifier
                                              smatedpairs[lee,3] = 2
                                            }
                                            remove(smom, sdad)
                                          }else{
                                            #find random chance of mates finding each other so that as Nc ^, chance of interacting ^ -- happens for all if switch is turned off
                                            smatedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al))))
                                          }
                                        }
                                        skeptpairs = smatedpairs[smatedpairs[,3]>=1,,drop=FALSE]
                                        spairs = skeptpairs #keptpairs[,c(1:2)]  <- use this if don't want migrant identifier
                                        
                                        remove(slee, smatedpairs, skeptpairs)
                                      }else{
                                        sal = nrow(spairs)
                                        spairs <- cbind(spairs, rep(1, sal))
                                      }
                                      
                                      if(nrow(spairs) >= 3){
                                        #randomize the pairs
                                        srand = sample(1:nrow(spairs),nrow(spairs))
                                        spairs <- spairs[srand, ]
                                        
                                        #pairs <- rand
                                      }
  colnames(spairs) <- c('smom','sdad','smigident')
                                      
 remove(sdeadS, ssimmature, smates, srand)
     
    return(list(pair, spairs))
                                    }
                                    
}



#Reminder: migrants are not preferentially chosen, so migrants =/= effective migrants




  #############################################################
