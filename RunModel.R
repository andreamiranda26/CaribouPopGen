#RunModel.R for Caribou Genetics Andrea from code -> Lamka and Willoughby 2023

RunModel = function(parameters, r, directory, replicates, prj, grp){ #prj and grp are in Cover.R
  FINAL = NULL
  REP   = NULL
  POP   = NULL 
  #rr=1  #use this when debugging, remove this when not skipping through the below line
  for(rr in 1:replicates){
    #call parameters for this replicate run
    k             = parameters$k[r]
    nSNP          = parameters$nSNP[r]
    miggy         = parameters$miggy[r] #found in Migrate.R
    LBhet         = parameters$LBhet[r] #c(0.45, 0.07) #lowerbound limit for SOURCE POP
    LBp           = parameters$LBp[r]   #c(0.45, 0.07) #lowerbound limit for FOCAL POP
    nMicro        = parameters$nMicro[r]
    maxage        = parameters$maxage[r]
    broodsize     = parameters$broodsize[r]
    maturity      = parameters$maturity[r]
    years         = parameters$years[r]
    r0            = parameters$r0[r]
    # nSNP.mig      = parameters$nSNP.mig[r] 
    # nSNP.cons     = parameters$nSNP.cons[r]
    #if add more parameters in Cover.R, add them here as well
    
    
################################################################################    
    
    
    
  ##from code to help tweak 
    
    
    
    # simulation parameters
    
    popsize  = c(50,100)                    # population sizes to simulate 
    simyears = 100                          # total years to run the isolation portion of the simulation (does not include delay)
    survival = 0.90                         # survival rate from literature I saw 85-90%
    maxage   = 13                           # set maximum age based on caribou literature 
    #agecap   = seq(2, 102, 20)             # maximum age (2 to 102 using 20 year intervals in example)
    maturity = 2                            # age of maturity to reproduce set at 2 years
    reps     = 100                          # replicates
    structK  = 3                            # number of K for structure analyses
    levels   = seq(0, 500, 25)              # years to run structure
    delay    = 100                           # number of years between initiation of large pop and isolation second pop
    
    
    
    RunSims(alleles, allelefreqs, popsize, simyears, survival, agecap, reps, structK, levels, delay)
    
    
##################################################################################
  

    
    #initialize population                   #matrix is easier to manipulate than a dataframe -- "ncol = X + (nloci)*2
    pop = matrix(nrow=k, ncol=9)            #each individual gets its own row 
   # colnames(pop) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness", "prop migrant SNPs") #just to give a better understanding of what these variables are, set names
    colnames(pop) <- c("id", "age", "sex", "subgroup","relative fitness", "prop migrant SNPs", "sub born", "sub died", "alive", "n adult offspring")
    pop[,1] = seq(1,k,1)                     #each individual has unique ID name; sequence starting at 1, through k, with each 1 iteration
   # pop[,2:3] = 0                            #parent ID; at this point, we are putting all equal to zero because this is the initial generation and we don't know parents
    pop[,2] = rpois(k,maturity)-1            #set age with a poisson distribution around the age of maturity and subtract 1 because we age as the first step in the simulation   #FOR UNIFORM DIST: dunif(k, min =0, max = maturity, log = FALSE)-1  #FOR RANDOM DIST: sample(seq(0,maxage,1),k,replace=T)-1
   # pop[,3] = sample(c(0,1),k,replace=T)     #assign indvs as male (1) or female (0) 
    pop[, 3] <- sample(c(0, 1), size = k, replace = TRUE, prob = c(1 - male_ratio, male_ratio))  # Generate males and females with the desired ratio, each individual assigned male (1) or female (0)
    pop[,4] = sample(c("E","W"), k, replace=T) #assigns indvs as East of West subgroups? this should be something I can change 
   # pop[,6] = NA                             #this will be for number of times as a parent - calculated in RepSucc.R
    #pop[,7] = NA                             #this will be for number of offspring survive to maturity - calculated in RepSucc.R
    pop[,9] = 1                              #alive or dead? alive = 1, dead = 0
    pop[,7] = 0                              #thinking subgroup born? Gina added generation born
    pop[,8] = 0                             #thinking subgroup died? gina added generation died
    pop[,5] = NA                            #relative fitness, aka heterozygosity *of nSNP only* - calculated below 
    pop[,6] = 0                             #proportion of migrant SNPs - initial pop will all be 0
    pop[,10] = NA                             #for number of offspring that reach maturity
    sz = k                                   #to keep track of the number of indv for ID'ing later
    sz_col = ncol(pop)
    
   
  
    
    
    #generate SNPs for the starting pop 
    popgen = matrix(nrow=k, ncol=nSNP*2)
    columns = seq(1,(nSNP*2),2)  #create 2 columns per SNP with 0-1 for each allele
    for(l in 1:nSNP){
      p = sample(seq(from=LBp, to=(LBp+0.1), by=0.01),1)  #introduce variation by selecting p, range defined in Cover.R
      #create pool of genotypes in HWE to select from
      pool = c(rep(0, round(k*p*p, 0)),                                      #homozygous p*p
               rep(1, round(k*(1-p)*(1-p), 0)),                              #homozygous (1-p)*(1-p)  
               rep(2, k-(round(k*p*p, 0)+(round(k*(1-p)*(1-p), 0))))         #heterozygous
      )
      #connect pool to indvs and assign the genotypes
      gtype = sample(pool, k, replace = FALSE)
      for(kk in 1:k){
        if(gtype[kk]==0){                 #homo (0,0)
          popgen[kk,columns[l]]   = 0
          popgen[kk,columns[l]+1] = 0
          next
        }else if(gtype[kk]==2){           #hetero (0,1)
          popgen[kk,columns[l]]   = 0
          popgen[kk,columns[l]+1] = 1
        }else{                            #homo (1,1)
          popgen[kk,columns[l]]   = 1
          popgen[kk,columns[l]+1] = 1
        }
      }
      pool = NULL  #nullify the variable for use in generating source genotypes below
    }
    
    #calculate heterozygosity values for generated genotypes - NOTE this is across nSNPs only
    het <- matrix(nrow=nrow(popgen), ncol=1)
    for(g in 1:nrow(popgen)){
      w <- sum(popgen[g ,seq(1,ncol(popgen),2)]!=popgen[g,seq(2,ncol(popgen),2)])/(ncol(popgen)/2)   #add up number of hetero sites per number of SNPs
      het[g,1] <- w
    } 
    pop[,5] <- het  #fill in calculated heterozygosities in the pop matrix
    
    # #create migrant and nonmigrant unique SNPs - will be used to follow migrant ancestry
    # popSNPs = matrix(nrow=k, ncol=nSNP.mig*2)
    # columnsb = seq(1,(nSNP.mig*2),2)
    # for(b in 1:nrow(popSNPs)){    #set up similar to above in case change the sequence or format later
    #   popSNPs[b,] = 0             #all focal pop indv have nSNP.mig = 0
    }
    
    #REMOVE###create conserved SNPs - will be used to follow mutation    
    #REMOVE##conSNPs = matrix(nrow=k, ncol=nSNP.cons*2)
    #REMOVE##columnsc = seq(1,(nSNP.cons*2),2)
    #REMOVE##for(c in 1:nrow(conSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE##  conSNPs[c,] = 0    #all indv of the species have nSNP.cons = 0
    #REMOVE##}
    
    #REMOVE##focalpop <- cbind(pop, popgen, popSNPs, conSNPs)   ##use this when generating all 3 types of SNPs
    focalpop <- cbind(pop, popgen, popSNPs)  
    pop <- focalpop
    
    #write starting pop to table
    ####REMOVED### write.table(pop, paste(directory, "/Output/focal_population", r, ".csv", sep=""), sep=",", col.names=T, row.names=F)
    
    #clean up
    remove(popgen, popSNPs, het, b, g, w, columns, columnsb, gtype, kk, l, pool) #focalpop, conSNPs
    ########################################################
    
    
    ################# SOURCE POP, this would be the sub populations 
    
    
    #initialize source population 
    source = matrix(nrow=s, ncol=12)            #each individual gets its own row.
    colnames(source) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness", "prop migrant SNPs") #just to give a better understanding of what these variables are, set names
    source[,1] = seq(-(s),-1,1)                 #each individual has unique ID name; sequence starting at -1, through -k, with each 1 iteration, negative flag for source pop
    source[,2:3] = -1                           #at this point, we are putting all equal to negative 1 to flag from source pop, and we dont know parents because parents arent in focal pop
    source[,4] = sample(seq(0,maxage,1),s,replace=T)   #set age between 0 and maxage (source isnt aged, so dont subtract 1)
    source[,5] = sample(c(0,1),s,replace=T)     #each individual assigned male (1) or female (0) 
    source[,6] = NA                             #this will be for number of times as a parent
    source[,7] = NA                             #for number of offspring that reach maturity
    source[,8] = 1                              #alive or dead? alive = 1, dead = 0
    source[,9] = -1                             #generation born - will be changed in Migrate.R to the generation entered focal pop
    source[,10] = 0                             #generation died
    source[,11] = NA                            #relative fitness, aka heterozygosity *of nSNP only* - calculated below 
    source[,12] = 1                             #proportion of migrant SNPs - initial source pop will all be 1
    
    #generate source gentoypes
    sourcegen = matrix(nrow=s, ncol=nSNP*2)
    columns = seq(1,(nSNP*2),2)  #create 2 columns per SNP with 0-1 for each allele
    for(l in 1:nSNP){
      p = sample(seq(from=LBhet, to=(LBhet+0.1), by=0.01), 1)  #introduce variation by selecting p, range defined in Cover.R
      #create pool of genotypes in HWE
      pool = c(rep(0, round(s*p*p, 0)),                                      #homozygous p*p
               rep(1, round(s*(1-p)*(1-p), 0)),                              #homozygous (1-p)*(1-p)  
               rep(2, s-(round(s*p*p, 0)+(round(s*(1-p)*(1-p), 0))))         #heterozygous
      )
      #connect pool to indvs and assign the genotypes
      gtype = sample(pool, s, replace = FALSE)
      for(ss in 1:s){
        if(gtype[ss]==0){                 #homo (0,0)
          sourcegen[ss,columns[l]]   = 0
          sourcegen[ss,columns[l]+1] = 0
          next
        }else if(gtype[ss]==2){           #hetero (0,1)
          sourcegen[ss,columns[l]]   = 0
          sourcegen[ss,columns[l]+1] = 1
        }else{                            #homo (1,1)
          sourcegen[ss,columns[l]]   = 1
          sourcegen[ss,columns[l]+1] = 1
        }
      }
    }
    
    #calculate heterozygosity values for generated genotypes - NOTE this is across nSNPs only
    sourcehet <- matrix(nrow=nrow(sourcegen), ncol=1)
    for(j in 1:nrow(sourcegen)){
      z <- sum(sourcegen[j ,seq(1,ncol(sourcegen),2)]!=sourcegen[j,seq(2,ncol(sourcegen),2)])/(ncol(sourcegen)/2)  #add up number of hetero sites per number of SNPs
      sourcehet[j,1] <- z
    } 
    source[,11] <- sourcehet  #fill in calculated heterozygosities in the source matrix
    
    #create migrant and nonmigrant unique SNPs - used to track migrant ancestry
    migSNPs = matrix(nrow=s, ncol=nSNP.mig*2)
    columnsd= seq(1,(nSNP.mig*2),2)
    for(d in 1:nrow(migSNPs)){    #set up similar to above in case change the sequence or format later
      migSNPs[d,] = 1              #all source pop indv have nSNP.mig = 1
    }
    
    #REMOVE###create conserved SNPs - used to track mutation    
    #REMOVE##conSNPs = matrix(nrow=s, ncol=nSNP.cons*2)
    #REMOVE##columnse = seq(1,(nSNP.cons*2),2)
    #REMOVE##for(e in 1:nrow(conSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE##  conSNPs[e,] = 0             #all indv of the species have nSNP.cons = 0
    #REMOVE##}
    
    #REMOVE##source1 <- cbind(source, sourcegen, migSNPs, conSNPs)   ##use this when generating all 3 types of SNPs
    source1 <- cbind(source, sourcegen, migSNPs)
    source <- source1
    
    #write starting source to table
    ###REMOVED### write.table(source, paste(directory, "/Output/source", r, ".csv", sep=""), sep=",", col.names=T, row.names=F)
    
    
    
    
    
    
    
    #prepare focal and source pop for Fst analysis in hierfstat (enacted in Analyze.R)
    SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)                                #find number of SNPs
    pos1 = seq(1, SNPS, 2)                                                        #allele 1 positions, aka odd values
    pos2 = pos1+1                                                                 #allele 2 positions, aka even values
    
    fstinit <- focalpop[, -c(ncol(focalpop)-(SNPS):ncol(focalpop))]               #grab SNPs
    fstinit[fstinit[,]==0] <-2                                                    #change 0s to 2s
    fstinit[,pos1] <- as.numeric(paste(fstinit[,pos1], fstinit[,pos2], sep=""))   #merge SNPs
    fstinit <- fstinit[,-c(pos2)]                                                 #remove single pos2 SNPs
    initident <- matrix(nrow=nrow(fstinit), ncol=1)                               #add pop identifier
    initident[,1] = 0                                                             #define initialized focal pop
    fstinit <- cbind(initident,fstinit)                                           #merge identifier and genotypes
    
    fstsource <- source1[, -c(ncol(source1)-(SNPS):ncol(source1))]                    #grab SNPs
    fstsource[fstsource[,]==0] <-2                                                    #change 0s to 2s
    fstsource[,pos1] <- as.numeric(paste(fstsource[,pos1], fstsource[,pos2], sep="")) #merge SNPs
    fstsource <- fstsource[,-c(pos2)]                                                 #remove single pos2 SNPs
    sourceident <- matrix(nrow=nrow(fstsource), ncol=1)                               #add pop identifier
    sourceident[,1] = -1                                                              #define initialized source pop
    fstsource <- cbind(sourceident,fstsource)                                         #merge identifier and genotypes
    
    #clean up
    remove(source1, focalpop, initident, sourceident, sourcegen, pool, migSNPs, l, d, ss, sourcehet, gtype, columns, columnsd, z, j) #currently holding p if needed
    #REMOVE##remove(conSNPs, columnsc,  columnse, c)  #use this if nSNP.cons != 0 
    
    #create for loop for each time step
    for(y in 0:years){
      if(y != 0){
        pop = AgeUp(pop)                        #age pop + 1 year
        #pop = FitnessDeath(pop, maturity, y)    #kill indv
        #REMOVE##pop = DeathByAge(pop, maxage)          #age-dependent mortality
        # if(sum(pop[,8]) <= 10){                 #if there are <=10 indv, crash pop
        #   print(paste("Crash @ FitnessDeath - Population low, less than 10 indv"))
          out = Analyze(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp)
          FINAL = rbind(FINAL, out[1,])
          break
        }
        #REMOVE##tttt = Stochastic(pop, stoch, k, numboff, styr, edyr, nwk, dur, y, years, r0, parameters, r)  #function for stochastic decline in pop
        #REMOVE##pop = tttt[[1]]
        #REMOVE##k = tttt[[2]]
        #REMOVE##pop = RandomDeath(pop)                  #random mortality
        tt = Migrate(pop, source, y, miggy, styr, edyr, dur)        #subpop migration from source to focal
        pop = tt[[1]]  #output 1 is the pop object
        mig = tt[[2]]  #output 2 is the number of migrants
        sz = sz + mig  #used for tracking number of indv and their ID numbers
        source = tt[[3]] #output 3 is the source object
       # if(sum(pop[,8]) <= 4){                #if there are <=4 indv, crash pop
          #print(paste("Population crash @ MateChoice, less than 4 indv"))
          out = Analyze(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp)
          FINAL = rbind(FINAL, out[1,])
          break
        }
        pairs = MateChoice(pop, sex, maturity)  #choose mates, Gina had also added allee, matemigs in there , now its same as MateChoice.R
        if(is.null(pairs)==TRUE){    #if there are no mates, pop crashes
          print(paste("skipping pop size next, breed due to no parents"))
          out = Analyze(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp)
          FINAL = rbind(FINAL, out[1,])
          break  #break out of this loop
        }
        pp = PopSizeNext(pop, k, r0, maturity, y, styr, edyr, nwk, dur, parameters, r, K) #calculate the next generation's pop size according to logistic growth eqn
        numboff = pp[[1]]  #output 1 is the number of offspring to produce
        K = pp[[2]]        #output 2 is the new K (carrying capacity)
        if(numboff >= 1){
          ttt = Breed(pop, pairs, numboff, k, sz, nSNP, nSNP.mig, broodsize, y, mu, mutate, nSNP.cons, pos1, pos2, rr, r, prj, grp, matemigs) #create new babies!
          pop = ttt[[1]]   #output 1 is the new pop object
          bb = ttt[[2]]    #output 2 is the number of babies added
          sz = sz + bb     #for tracking number of indv for IDing
        }else if(numboff <= 0){
          print(paste("No new babies, skip breed")) 
          #still fill out Ne count table
          {
            NE = matrix(nrow=1, ncol=12)   #create matrix for Ne Count table
            colnames(NE) <- c("year", "eff_mom", "eff_dad", "nbabies", "naliveadults", "possible_mom", "possible_dad", "eff_mig", "parameterset", "replicate", "project", "group") #just to give a better understanding of what these variables are, set names
            
            NE[1,1] = y                             #grab year
            NE[1,2] = 0                             #grab n unique effective moms
            NE[1,3] = 0                             #grab n unique effective dads
            NE[1,4] = 0                             #grab n babies
            
            alive = pop[pop[,8]==1,,drop=FALSE]
            adult = alive[alive[,4]!=0,,drop=FALSE]
            adult_f = adult[adult[,5]==0,,drop=FALSE]
            adult_m = adult[adult[,5]==1,,drop=FALSE]
            NE[1,5] = nrow(adult)                   #grab n alive adults
            NE[1,6] = nrow(adult_f)                 #grab n possible moms
            NE[1,7] = nrow(adult_m)                 #grab n possible dads
            
            NE[1,8] = 0                             #grab number of migrant parents
            
            NE[1,9] = r                             #parameter number
            NE[1,10] = rr                           #replicate number
            NE[1,11] = prj                          #defined project
            NE[1,12] = grp                          #defined group
            
            remove(alive, adult, adult_f, adult_m)  #clean up
            
            if(isTRUE(y == 1 && r == 1 && rr == 1)){
              write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE) #create new table for this sim
            }else{
              write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=FALSE, append=TRUE, quote=FALSE, row.names=FALSE) #add to previous table
            }
          }
        }
        pop = AgeDeath(pop, maxage, y)          #kill indv based on age
        if(sum(pop[,8]) <= 10){                 #if there are <=10 indv, crash pop
          print(paste("CRASH @ AgeDeath - Population low, less than 10 indv"))
          out = Analyze(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp) #remember to feed same thing to all Analyze functions!
          FINAL = rbind(FINAL, out[1,])
          break
        }
        
        print(paste("DONE!", y, "param", r, "rep", rr))   #for tracking which sim run you're on
        
        # #clean up by removing dead indv every 25 years, this will speed up computation time
        # if(is.wholenumber(y/25)==TRUE){
        #   print(paste("Cleaning up dead!"))
        #   dead <- pop[pop[,8]==0,,drop=FALSE]
        #   deadindv <- dead[, c(1:sz_col)]  #remove indv genotypes, will speed up computation time
        #   if(y==25){
        #     write.table(deadindv, paste(directory, "/Output/dead.csv", sep=""), sep=",", col.names=FALSE, append=FALSE, quote=FALSE, row.names=FALSE) #create new dead for this parameter set
        #   }else{
        #     write.table(deadindv, paste(directory, "/Output/dead.csv", sep=""), sep=",", col.names=FALSE, append=TRUE, quote=FALSE, row.names=FALSE)  #add to previously made table
        #   }
          pop <- pop[pop[,8]==1,,drop=FALSE] #make new pop object with only alive indv
          remove(dead, deadindv) #clean up
        }
        #REMOVE##pop <- pop[pop[,8]==1,, drop=FALSE] #remove dead indv -- use this if don't need to hold dead indv above and not using RepSucc.R -- this will speed it up!!
      }
      # if(y == 0){
      #   K = k
      }
      #analyze each replicate
      out = Analyze(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp)
      FINAL = rbind(FINAL, out[1,])
    }
    # #read in dead indv for RepSucc.R
    # died = read.table(paste(directory, "/Output/dead.csv", sep=""), header=F, sep=",")
    # indv = pop[, c(1:sz_col)]  #remove indv genotypes - just a check, should have happened when saving dead indv above
    # colnames(died) = colnames(indv)
    # pop_indv = rbind(indv,died) #add dead to pop for RepSucc calculations
    # remove(pop, indv, died)   #clean up
    # 
    # #calc Reproductive Success using pop data
    # aa = RepSucc(pop_indv, maturity, years, rr, r, prj, grp)
    # pop_indv = aa[[1]]  #output 1 is the final pop with all indv and all indv data
    # rep = aa[[2]]       #output 2 are calculations of repro success
    # REP = rbind(REP, rep)  #combine with other runs
    # #REMOVE##POP = rbind(POP, pop)  #use this if tracking pop, slows computation considerably
    
    print(paste("REPLICATE", rr, "OF PARAM", r, "DONE!"))  #track where you are in the sim
  } 
  return(list(FINAL, REP)) #POP,        #return final datatables!
}

#note on simulation checks:
#next will go to the next loop AKA the next year
#break will completely stop th