#used for caribou genetics Andrea from Lamka Willoughby 2023
#tweaked 
#this will examine the population size and trend for the future generation. 
#this will also help in giving the number of offspring to generate from mating pairs

#as of 9/23/22, this will be combined with Stochastic.R
#this means that there could potentially be a lag, especially when k is large



new parameters:
styr          = 100 #year to start pop decline
# edyr          = 150 #year to end pop decline, first year at low pop size
# nwk           = 250 #pop size after decline -- probs makes sense to keep these even in vary decline years and decline rate. should end @ same pt for all pop sizes
# dur           = 50  #duration of small pop size before pop growth 

# PopSizeNext = function(pop, k, r0, maturity, y, styr, edyr, nwk, dur, parameters, r, K)

  PopSizeNext = function(pop, k, r0, maturity, y, styr, parameters, r, K){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
  alive = pop[which(pop[,1]%NOTin%dead), , drop = FALSE]
  #since only returning numboff, dont need to rbind dead and alive into pop
  #pop = pop[pop[,4] >= maturity, , drop=FALSE]          #isolate adults -- if only want effective pop size to count here
  #if only using adults, remove immature
  
  #calculate the current population size following aging and death
  ###REMOVED# Nt = nrow(pop) #use this if removing dead from pop
  Nt = nrow(alive)
  
  if(y < styr){               #maintain k for the burn in period
    K = k
    R0 = r0
  }else if(y > edyr + dur){   
    deltaK = abs(round((k - nwk)/(styr - edyr)))
    K = K + deltaK
    R0 = r0
    
    if(K > parameters$k[r]){  #check to make sure K is not greater than the original k in Cover.R
      K <- parameters$k[r]
    }
  }else if(styr <= y & y < edyr){ #change K to push pop into bottleneck
    deltaK = round(abs(k - nwk)/(styr - edyr)) #this is the change in K per year for the decline period
    #note, deltaK will be negative when pop is decreasing
    
    K = K + deltaK
    
  }else{                       #if during the duration period of bottleneck, maintain small pop size
    K = nwk
    R0 = r0/2    #introduce allee effect, but only at lowest pop size
  }
  
  #calculate the new pop size with the logistic growth equation
  Ntt = Nt*(1+R0*(1-(Nt/K))) #logistic
  #r0 is the per capita growth rate, set as a parameter in Cover.R
  
  #add Density Independent variance in growth
  Nt1 = round(rnorm(1, Ntt, 1), 0)
  Nt1 = as.integer(Nt1)
  print(paste("The next generation's population size will be", Nt1, "K is", K, "k is", k))
  
  #determine the number of additional offspring to produce
  numboff = Nt1 - Nt
  
  #for some reason numboff is a double and causing problems
  #turn numboff into an integer
  numboff = as.integer(numboff)
  print(paste("the number of offspring needed is", numboff))
  
  remove(alive, dead)
  
  return(list(numboff,K))
}