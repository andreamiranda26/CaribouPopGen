#Analyze.R
#used for complex model for ABM class fall 2021

#taken from Janna's captive breeding IBM, function WriteOut

Analyze = function(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp){  #should this be parameters or replicates?
  #get variables for run -- I think this can be copied from RunModel.R
  k             = parameters$k[r]
  #REMOVED###allele        = parameters$allele[r]
  nSNP          = parameters$nSNP[r]
  miggy         = parameters$miggy[r]
  LBhet         = parameters$LBhet[r]
  LBp           = parameters$LBp[r]
  #REMOVED###nMicro        = parameters$nMicro[r]
  #REMOVED###sex           = parameters$sex[r]
  maxage        = parameters$maxage[r]
  broodsize     = parameters$broodsize[r]
  #REMOVED###sexratio      = parameters$sexratio[r]
  maturity      = parameters$maturity[r]
  years         = parameters$years[r]
  r0            = parameters$r0[r]
  #REMOVED###ratemort      = parameters$ratemort[r]
  nSNP.mig      = parameters$nSNP.mig[r] 
  nSNP.cons     = parameters$nSNP.cons[r]                  #number of conserved alleles
  
  #writeout final POP == compare this to the final pop in Cover.R, should be the same
  #write.table(pop, paste(directory, "/Output/WriteOutPop.csv", sep=""), sep=",", col.names=T, row.names=F) #since in RunModel, might not need to feed it pop
  
  #check for indv ID numbers
  if(!length(pop[,1])==length(unique(pop[,1]))){ #notice that ! means NOT
    print(r)
    print("NON UNIQUE ID NUMBERS")
    return()
  }
  #if no indv, exit
  if(length(pop[,1])==0){return()}
  
  #SHOULD THESE BE ALL INDV OR ONLY ALIVE??
  #separate out alive indv
  alive = pop[pop[,8]==1, , drop=FALSE]
  ###could also use: pop = pop[pop[,8]!=0, , drop=FALSE]
  
  #calculate summary stats for final pop
  FIN = matrix(nrow=years+1, ncol=20)
  colnames(FIN) = c("year", "popsize", "propmig", "He", "Ho", "Fis", "nadults", "sxratio", "nmig", "Fst", "replicate", "parameterset", "numboff", "FstVSource", "FisVSource", "deltaK", "propMigSNPs", "Ho_allSNPs", "project", "group")
  #note that because this is for all years of the simulation, the initialized pop is not included in this (e.g., year 0)
  
  #add year to summary matrix
  #FIN[,1] = c(0:nrow(FIN))
  
  f = 1
  #for(f in 1:nrow(FIN)){
  #year = FIN[f,1] #-1 #doing this cuz also taking year 0 -- note that on 5/2/22 there were still errors with numbering on column 1
  FIN[f,1] <- y
  
  FIN[f,16] <- K
  
  #separate out alive in current year -- Janna did these from year born and year died column
  data = alive[alive[,8]>0, , drop = FALSE]
  
  x=NULL
  x = try(length(data[,1]), silent=TRUE)
  if(is.null(x)){break}
  if(x<1){break}
  if(!is.numeric(x)){break}
  
  #number of indv
  FIN[f,2] = nrow(data)
  
  #proportion migrants in population
  FIN[f,3] =  sum(data[,2]==-1)/length(data[,1])   #1 - sum(data[,2]==-1)/length(data[,1])
  
  #proportion of migrant genotypes in population
  FIN[f,17] <- mean(data[,12])
  
  #He and Ho - neutral (?)
  SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)
  genotype = data[, -c(ncol(data)-SNPS:ncol(data))] #THERE IS AN ERROR HERE THAT IS CHANGING THE ORDER OF COLUMNS?? ALSO NOTE THAT THE NUMBER OF SNPS IS WRONG--PROBS CUZ OF NOT RUNNING THE DIFFERENT TYPES IN RUNMODEL. FIX THIS~!
  #snps = rep(c(1,2),ncol(genotype)/2)
  
  HE = NULL
  HO = NULL
  
  loc.pos = seq(1, SNPS, 2)
  for(lp in loc.pos){
    #per locus heterozygostiy
    locus <- genotype[, c(lp, lp+1), drop=FALSE]
    #NOTE this includes MigSNPS while the other het value does not
    geno  <- length(locus[,1])
    het   <- length(which(locus[,1] != locus[,2]))
    het.observed <- het/geno
    HO = c(HO, het.observed)
    
    freqs <- table(locus)
    homozygous = NULL
    for(v in 1:length(freqs)){
      homozygous = c(homozygous, (freqs[v]/sum(freqs)*freqs[v]/sum(freqs)))
    }
    het.expected <- 1 - sum(homozygous)
    HE = c(HE, het.expected)
  }
  FIN[f,4] <- mean(HE)
  FIN[f,5] <- mean(data[,11]) 
  FIN[f,18] <- mean(HO)
  
  #find number of adults per year
  adults = data[data[,4]>= maturity, , drop = FALSE]
  FIN[f,7] = nrow(adults)
  
  ########################################NEW####################################3
  #find the sex ratio
  FIN[f,8] = mean(data[,'sex']) #<0.5 female, >.5 male
  #note this includes babies
  
  #note number of migrants
  if(y != 0){
    FIN[f,9] = mig  #generated in Migrate.R, passed to RunModel.R, and passed here in Analyze.R
  }else{
    FIN[f,9] = 0  #this is for year 0, there are no migrants in this pop
  }
  
  
  #calc the proportion of migrant alleles in the population
  #do I want heterozygosity of mig alleles? or freq of mig alleles? or something different?
  #?
  #?
  #?
  
  #table function https://www.datasciencemadesimple.com/table-function-in-r/#:~:text=Table%20function%20in%20R%20-table%20%28%29%2C%20performs%20categorical,creating%20Frequency%20tables%20with%20condition%20and%20cross%20tabulations.
  
  
  #LINKS FOR CALC FST USING HIERFSTAT
  #https://rdrr.io/cran/hierfstat/
  #https://rdrr.io/cran/hierfstat/#vignettes
  #https://rdrr.io/cran/hierfstat/man/biall2dos.html
  #https://rdrr.io/cran/hierfstat/man/fs.dosage.html
  #https://rdrr.io/cran/hierfstat/man/fstat2dos.html
  #https://rdrr.io/cran/hierfstat/man/nb.alleles.html
  #https://rdrr.io/cran/hierfstat/man/pairwise.neifst.html
  #https://rdrr.io/cran/hierfstat/man/pairwise.WCfst.html
  #https://rdrr.io/cran/hierfstat/man/ppfst.html
  
  
  
  #prepare the genotypes for hierfstat
  SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)                    #find number of SNPs
  fstdata <- data[, -c(ncol(data)-(SNPS):ncol(data))]               #grab SNPs
  
  #change 0s to 2s for hierfstat to read
  fstdata[fstdata[,]==0] <-2
  
  #merge pos1 and pos2 into pos1, then remove pos2
  fstdata[,pos1] <- as.numeric(paste(fstdata[,pos1], fstdata[,pos2], sep=""))
  fstdata <- fstdata[,-c(pos2)]
  
  #add pop identifier for calculations
  popident <- matrix(nrow=nrow(fstdata), ncol=1)
  popident[,1] = y
  fstdata <- cbind(popident,fstdata)
  
  if(y != 0){
    #do the same to the initialized focal pop -- for comparison
    #MOVEDTORUNMODEL##fstinit <- focalpop[, -c(ncol(focalpop)-(SNPS):ncol(focalpop))]               #grab SNPs
    #MOVEDTORUNMODEL##fstinit[fstinit[,]==0] <-2                                                    #change 0s to 2s
    #MOVEDTORUNMODEL##fstinit[,pos1] <- as.numeric(paste(fstinit[,pos1], fstinit[,pos2], sep=""))                     #merge SNPs
    #MOVEDTORUNMODEL##fstinit <- fstinit[,-c(pos2)]                                                 #remove single pos2 SNPs
    #MOVEDTORUNMODEL##initident <- matrix(nrow=nrow(fstinit), ncol=1)                               #add pop identifier
    #MOVEDTORUNMODEL##initident[,1] = 0
    #MOVEDTORUNMODEL##fstinit <- cbind(initident,fstinit)                                           #merge identifier and genotypes
    
    fstnow <- rbind(fstdata, fstinit)                                            #merge current year and initialized year to one matrix for calculations
    
    fstnow <- as.data.frame(fstnow)                                               #turn into a dataframe
    calc <-wc(fstnow, diploid=TRUE, pol=0)                                         #calc FST and FIS
    #calc <- pairwise.WCfst(fstdata,diploid=TRUE)                                   #calculate FST
    
    FIN[f,10] <- calc$FST
    FIN[f,6] <- calc$FIS
    
    #do the same to the initialized source pop -- for comparison
    #MOVEDTORUNMODEL##fstsource <- source1[, -c(ncol(source1)-(SNPS):ncol(source1))]                    #grab SNPs
    #MOVEDTORUNMODEL##fstsource[fstsource[,]==0] <-2                                                    #change 0s to 2s
    #MOVEDTORUNMODEL##fstsource[,pos1] <- as.numeric(paste(fstsource[,pos1], fstsource[,pos2], sep=""))                     #merge SNPs
    #MOVEDTORUNMODEL##fstsource <- fstsource[,-c(pos2)]                                                 #remove single pos2 SNPs
    #MOVEDTORUNMODEL##sourceident <- matrix(nrow=nrow(fstsource), ncol=1)                               #add pop identifier
    #MOVEDTORUNMODEL##sourceident[,1] = -1
    #MOVEDTORUNMODEL##fstsource <- cbind(sourceident,fstsource)                                         #merge identifier and genotypes
    
    fstdatavsource <- rbind(fstdata, fstsource)                                              #merge current year and initialized year to one matrix for calculations
    
    fstdatavsource <- as.data.frame(fstdatavsource)
    calcvsource <-wc(fstdatavsource, diploid=TRUE, pol=0) 
    FIN[f,14] <- calcvsource$FST
    #FIN[f,15] <- calcvsource$FIS
  }
  if(y == 0){
    #do the same to the initialized source pop -- for comparison
    #MOVEDTORUNMODEL##fstsource <- source1[, -c(ncol(source1)-(SNPS):ncol(source1))]                    #grab SNPs
    #MOVEDTORUNMODEL##fstsource[fstsource[,]==0] <-2                                                    #change 0s to 2s
    #MOVEDTORUNMODEL##fstsource[,pos1] <- as.numeric(paste(fstsource[,pos1], fstsource[,pos2], sep=""))                     #merge SNPs
    #MOVEDTORUNMODEL##fstsource <- fstsource[,-c(pos2)]                                                 #remove single pos2 SNPs
    #MOVEDTORUNMODEL##sourceident <- matrix(nrow=nrow(fstsource), ncol=1)                               #add pop identifier
    #MOVEDTORUNMODEL##sourceident[,1] = -1
    #MOVEDTORUNMODEL##fstsource <- cbind(sourceident,fstsource)                                         #merge identifier and genotypes
    
    fstyo <- rbind(fstinit, fstsource) #merge current year and initialized year to one matrix for calculations
    
    fstyo <- as.data.frame(fstyo)                                               #turn into a dataframe
    calcyo <-wc(fstyo, diploid=TRUE, pol=0)                                         #calc FST and FIS
    
    FIN[f,10] <- 0 #no divergence at y=0
    FIN[f,6] <- 0  #no inbreeding at y=0
    FIN[f,14] <- calcyo$FST
    FIN[f,15] <- calcyo$FIS
    
    remove(fstyo)
  }
  
  
  FIN[f,11] = rr   #add replicate number
  FIN[f,12] = r    #add parameter set number
  
  if(y == 0){
    numboff = 0
  }
  FIN[f,13] = numboff
  
  FIN[f,19] = prj
  
  FIN[f,20] = grp
  
  
  
  #Fis for this pop
  
  #NOTES 4/18/22 -- consider another pairwise measure tha tmay work rather than FST. potentially the number of unique alleles??
  #this could be because FST is not sensitive to minor changes because the scale is in evolutionary time
  #may also want to add the number of variable SNPS (across indvs) since heterozy is within an indv
  
  #ALSO add from meeting 4/18/22
  #year 0 should be in this table >> look at the photo I took to see how to do this. in short, will want to ski[ to analyze in year 0 and then go through all timesteps]
  #that way, year 0 will be in the same tbale and iwll make it easier to analyze for following years
  #ALSO will want to add SNPs that are the SAME between pop and source so that we can seperate mutation, migration, and drift forces. think about this more.
  
  
  #}
  
  params = parameters[rep(r, nrow(FIN)),]
  out = cbind(FIN,params)
  colnames(out) = c("year", "popsize", "propmig", "He", "Ho", "Fis", "nadults", "sxratio", "nmig", "Fst", "replicate", "parameterset", "numboff", "FstVSource", "FisVSource", "deltaK", "propMigSNPs", "Ho_allSNPs", "project", "group",
                    "k", "nSNP", "miggy", "LBhet", "LBp", "maxage", "broodsize", "maturity", "years", "r0", "nSNP.mig", "nSNP.cons")
  
  remove(alive, adults, data, FIN, fstdata, genotype, locus, params, popident,
         freqs, geno, HE, het, het.expected, het.observed, HO, homozygous, loc.pos)
  
  return(out)
}

#additional things that I should make figs of that will need to be added to Analyze.R
#prop migrant alleles in pop
#number of males and females that bred in that year (may be able to replace Ne)
#number of mates (use the function "table"; data$habitat.mate (or could try "apply")) >> TABLE THIS FOR NOW, PROB WONT NEED OR WANT THIS
#sex ratio
#number of new migrants that generation (?)
#Fst


#notes 2/28/22
#add in Fst. do this for every year, and then later will cherry pick the results for every generation
#compare the Fst of the starting pop to the Fst of successive years to see how fst changes. 
#use the Weir and Cockerheim's Fst-- this should be a function/package in R to calculate this to make it easier
#consider any biases in independence, but that may not be an issue
#POTENTIAL APPLICATION is trying to create a calculation for estimating migrants from Fst values. --could be linked to our data?

#consider adding 
#number/proportion of migrants vs number/proportion of effective migrants
#genome-wide Fst (compare across loci or in source vs pop)
#a way to track inbreeding?? we know the pedigree so maybe if have a great grand parent in common? -- maybe can relate with real krat inbreeding data??
#relationship between fitness and heterozygosity (hetero advantage)? is this a thing since most things are random, not selected for?
#histogram for age of death

#to add: (noted 12/29/2021)
#LRS, RRS >> must use info from ReproSuc.R, so maybe better to calculate there and then feed to Plot.R
#number of alleles over time
#number of migrant alleles in population over time
#additional possible values
#number of unique breeders unique(pop[,2])+unique(pop[,3])
#mating success = total number of mates with whome an indv produced offspring
#-- also check the vortex abm paper to add any things they have that might be useful and informative here


#attempted and removed 5/3/2022
#library(GeneClusterNet)

#gtypes1 <- focalpop[,11:ncol(focalpop)] #focalpop is initialized pop
#gtypes2 <- source1[,11:ncol(source1)]   #source1 is the initialized source
#gtypes3 <- data[,11:ncol(data)]         #data are all current alive indv

#if(!(ncol(gtypes1) == ncol(gtypes2) && ncol(gtypes1) == ncol(gtypes3))){
#  print("GENOTYPE ERROR")
#  return()
#}
#u <- ncol(gtypes1) + 1
#gtypes1 <- cbind(c(1),gtypes1)
#gtypes2 <- cbind(c(2),gtypes2)
#gtypes3 <- cbind(c(3),gtypes3)

#all.gtypes <- rbind(gtypes1, gtypes2, gtypes3)
#fst <- fst(all.gtypes, all.gtypes[,1], 3)