setwd("~/Documents/Projects, papers, programs/Documents by person/Renae Sattler/MooseSims/output")

#sim parameters
replicates = 100
adultmort  = 1/15
gen        = 52
plotit     = 1

#repeat simulation for k number of times, iterate through cutoff values
for(k in 1:replicates){
  ####initialize####
  #set up replicate output file
  final.output = matrix(nrow=(gen+1), ncol=6)
  final.cols   = c("generation", "alleles", "Ho", "He", "cutoff", "numb.indv")
  colnames(final.output) = final.cols
  
  #set up plot
  if(plotit==1){
    Na         = NULL
    plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, gen), ylim=c(0, 3000), type="b", main=k) 
  }
  
  #initialize moose population
  final.output[1,5] = "not calculated"
  final.output[1,6] = 564
  
  pop.size = c(564, 572, 579, 596, 620, 634, 661, 766, 848, 1041, 1045, 1183, 1243, 1215,
               1203, 1139, 1070, 949, 845, 857, 788, 767, 780, 830, 927, 976, 1014, 1046, 
               1116, 1260, 1315, 1496, 1697, 1784, 2017, 2117, 2398, 900, 925, 997, 1031, 
               1120, 1100, 900, 750, 540, 450, 385, 650, 530, 510, 515)
  
  #read in allele frequency data
  locus1 = c(108, 110, 112, 114)
  freq1  = c(0.0227, 0.4432, 0.0114, 0.5227)
  locus2 = c(131, 133, 137, 143)
  freq2  = c(0.0122, 0.2927, 0.4268, 0.2683)
  locus3 = c(180, 184, 186, 188)
  freq3  = c(0.1477, 0.7273, 0.0455, 0.0795)
  locus4 = c(350, 354, 356, 358)
  freq4  = c(0.0750, 0.4250, 0.0250, 0.4750)
  locus5 = c(121, 133, 135)
  freq5  = c(0.0106, 0.3617, 0.6277)
  locus6 = c(191, 193, 195, 197, 199)
  freq6  = c(0.0610, 0.0488, 0.3902, 0.1220, 0.3780)
  locus7 = c(148, 150)
  freq7  = c(0.0227, 0.9773)
  locus8 = c(124, 126, 128, 130)
  freq8  = c(0.0870, 0.2500, 0.0761, 0.5870)
  locus9 = c(190, 192, 200)
  freq9  = c(0.0357, 0.7976, 0.1667)
  
  #start individual id#
  ind = 1
  ped = matrix(nrow=pop.size[1], ncol=24)
  remain.size = pop.size[1]
  
  #assign genotypes to create populatino of 564 individuals
  for(si in 1:pop.size[1]){
    ped[si,1] = 1 #pednum
    ped[si,2] = si #idnumber
    ped[si,3] = 0 #fid
    ped[si,4] = 0 #mid
    ped[si,5] = sample(x=c(1,2), size=1, prob=c(0.5,0.5), replace=TRUE)
    ped[si,6] = sample(x=locus1, size=1, prob=freq1, replace=TRUE)
    ped[si,7] = sample(x=locus1, size=1, prob=freq1, replace=TRUE)
    ped[si,8] = sample(x=locus2, size=1, prob=freq2, replace=TRUE)
    ped[si,9] = sample(x=locus2, size=1, prob=freq2, replace=TRUE)
    ped[si,10] = sample(x=locus3, size=1, prob=freq3, replace=TRUE)
    ped[si,11] = sample(x=locus3, size=1, prob=freq3, replace=TRUE)
    ped[si,12] = sample(x=locus4, size=1, prob=freq4, replace=TRUE)
    ped[si,13] = sample(x=locus4, size=1, prob=freq4, replace=TRUE)
    ped[si,14] = sample(x=locus5, size=1, prob=freq5, replace=TRUE)
    ped[si,15] = sample(x=locus5, size=1, prob=freq5, replace=TRUE)
    ped[si,16] = sample(x=locus6, size=1, prob=freq6, replace=TRUE)
    ped[si,17] = sample(x=locus6, size=1, prob=freq6, replace=TRUE)
    ped[si,18] = sample(x=locus7, size=1, prob=freq7, replace=TRUE)
    ped[si,19] = sample(x=locus7, size=1, prob=freq7, replace=TRUE)
    ped[si,20] = sample(x=locus8, size=1, prob=freq8, replace=TRUE)
    ped[si,21] = sample(x=locus8, size=1, prob=freq8, replace=TRUE)
    ped[si,22] = sample(x=locus9, size=1, prob=freq9, replace=TRUE)
    ped[si,23] = sample(x=locus9, size=1, prob=freq9, replace=TRUE)
  }
  
  ped.data = as.data.frame(ped)
  cols3 =  c("pednum", "id", "fid", "mid", "sex",
             "l1.1","l1.2","l2.1","l2.2","l3.1","l3.2",
             "l4.1","l4.2","l5.1","l5.2","l6.1","l6.2",
             "l7.1","l7.2","l8.1","l8.2","l9.1","l9.2", "age")
  colnames(ped) = cols3
  colnames(ped.data) = cols3
  
  #assign ages to individual from distribution of possible (1-15 years)
  ped[,24] = sample(seq(1,15,1), nrow(ped), replace=TRUE)
  
  #set start point for offspring ID numbers
  off.id = pop.size[1] + 1
  
  ####simulate population ####
  for(n in 1:gen){
    #plot pop size
    if(plotit==1){
      Na      = c(Na, nrow(ped))
      lines(c(1:n), Na , xlab="generation", ylab="population size", cex = 2, lty = 1, col="black", lwd=5)
    }
    
    #analysis of population parameters
    final.output[n,1] = n
    
    #He and Ho - neutral
    genos = ped[,6:23, drop=FALSE] 
    
    HEs   = NULL
    HOs   = NULL
    Als   = NULL
    
    loc.pos = seq(1, (9*2), 2)
    for(l in loc.pos){
      # per locus heterozygosity
      locus = genos[, c(l, l+1), drop=FALSE]
      geno  = length(locus[, 1])
      het   = length(which(locus[, 1] != locus[, 2]))
      het.observed  = het/geno
      HOs = c(HOs, het.observed)
      
      freqs = table(locus)
      homozygous = NULL
      for(l in 1:length(freqs)){
        homozygous = c(homozygous, (freqs[l]/sum(freqs) * freqs[l]/sum(freqs)))
      }
      het.expected = 1 - sum(homozygous)
      HEs = c(HEs, het.expected)
      
      catlocus = c(locus[,1], locus[,2])
      Als = c(Als, length(unique(catlocus)))
    }
    final.output[n,4 ] = mean(HEs)
    final.output[n,3 ] = mean(HOs)
    
    #number of alleles
    final.output[n,2 ] = mean(Als)
    
    #pop size
    final.output[n,6]  = nrow(ped)
    
    #break for end of simulation 
    if(n==52){break}
    
    #apply adult mortality
    ped = ped[ped[,24]<15,,drop=FALSE]
    nkill = round((nrow(ped) * adultmort), 0)
    if(nkill>0){
      kill  = sample(1:length(ped[,24]), nkill, replace=FALSE)
      ped   = ped[-kill,]
    }
    ped.data = as.data.frame(ped)
    
    #determine change in pop size
    current   = nrow(ped)
    next.year = pop.size[n+1]
    diff      = next.year - current
    n.pairs   = diff
    
    #if population growing, find pairs and simulate offspring
    if(n.pairs>0){
      #generate list of possible males and females for reproduction
      females.new = ped[ped[,5]==1, ]
      females.new = females.new[females.new[,24] > 1, ,drop=FALSE]
      
      males.new = ped[ped[,5]==2, ]
      males.new = males.new[males.new[,24] > 4, ,drop=FALSE]
      males.new = males.new[males.new[,24] < 13, ,drop=FALSE]
      
      maxpairs = min(nrow(females.new), nrow(males.new))
      if(maxpairs > n.pairs){
        females.approved = sample(females.new[,2], n.pairs, replace=FALSE)
        males.approved   = sample(males.new[,2], n.pairs, replace=TRUE)
      }else{
        females.approved = sample(females.new[,2], maxpairs, replace=FALSE)
        males.approved   = sample(males.new[,2], maxpairs, replace=TRUE)
      }
      
      #save pairs into matrix
      pairs = matrix(nrow=length(females.approved), ncol=2)
      pairs[, 2] = males.approved
      pairs[, 1] = females.approved
      
      #add offspring simulation parameters
      number.pairs = nrow(pairs)
      number.offspring = 1
      
      #pull information for parent pairs
      for(p in 1:number.pairs){
        #dummy offspring data frame
        offspring     = matrix(nrow = 1, ncol = 24)
        offspring[1,] = c(1:24)
        colnames(offspring) = cols3 
        
        mom = ped.data[ped.data$id==pairs[p,1],,drop=FALSE] 
        dad = ped.data[ped.data$id==pairs[p,2],,drop=FALSE]
        
        #simulate offspring for each parent pair drawn and saved above
        offspring[1,1] = 1
        offspring[1,2] = off.id   #id number (sequential)
        offspring[1,3] = dad[1,2] #dad id number
        offspring[1,4] = mom[1,2] #mom id number
        offspring[1,5] = sample(c(1,2), 1, replace=TRUE, prob=c(0.5, 0.5)) #sex
        
        # genotypes
        # prep parent genotypes
        fg = dad[1,6:23,drop=FALSE]
        mg = mom[1,6:23,drop=FALSE]
        
        # allele 1 positions
        positions = seq(1, (9*2), 2)
        
        # randomly sample either position 1 or 2 (add 0 or 1) to starting position
        fallele  = positions + sample(0:1, 9 * 1, replace = TRUE)
        fallele2 = fg[fallele] + (rbinom(9, 1, prob=0.0001) * sample(x=c(-2, 2), size=1, replace=TRUE, prob=c(0.5, 0.5)))
        fallele3 = matrix(fallele2, nrow = 1, ncol = 9, byrow = TRUE)
        
        mallele  = positions + sample(0:1, 9 * 1, replace = TRUE)
        mallele2 = mg[mallele] + (rbinom(9, 1, prob=0.0001) * sample(x=c(-2, 2), size=1, replace=TRUE, prob=c(0.5, 0.5)))
        mallele3 = matrix(mallele2, nrow = 1, ncol = 9, byrow = TRUE)
        
        for(l in 1:length(positions)){
          offspring[1,5+positions[l]]   = as.numeric(fallele3[1,l])
          offspring[1,5+positions[l]+1] = as.numeric(mallele3[1,l])
        }
        
        #add age (0)
        offspring[1,24] = 0
        
        #convert to data frame, add col names, and save into generation data frame 
        offspring.data = as.data.frame(offspring)
        colnames(offspring.data) = cols3
        ped.data = rbind(ped.data, offspring.data)
        
        #increase id number
        off.id = off.id + 1
      } 
      ped = as.matrix(ped.data)
      females.approved = NULL
      males.approved = NULL
    }
    
    #if population is shrinking, randomly select survivors
    if(n.pairs<0){
      ped.data = ped.data[sample(nrow(ped.data), next.year), ]
      ped = as.matrix(ped.data)
    }
    
    #increment age of entire population
    ped[,24] = ped[,24] + 1
  }
  write.table(final.output, paste("output_", k, ".csv", sep=""), col.names=FALSE, row.names=FALSE, sep=",", append=FALSE)
}

#plot(c(1:nrow(final.output)), final.output[,3], ylim=c(0,0.6), type="l")
#plot(c(1:nrow(final.output)), final.output[,4], ylim=c(0,0.6), type="l")
#plot(c(1:nrow(final.output)), final.output[,2], ylim=c(0,6), type="l")