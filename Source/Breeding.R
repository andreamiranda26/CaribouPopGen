

# set age
age <- pop[,2]  # min and max age of the caribou in the population, set in RunModel.R 

# # Calculate breeding percentages based on age
# age_data$breed_percentage <- sapply(age_data$age, calculate_breeding_percentage)
# 
# # Print the resulting data frame
# print(age_data)


# Calculate breeding percentage based on age
breed_percentage <- function(age) {
  # Define the age-based breeding percentage
  if (age < 2) {
    return(0)  # Breeding percentage for individuals under 2 years old
  } else (age > 2:4) {
    return(0.3)  # Breeding percentage for individuals between 20 and 39 years old
  } else (age > 4:7) {
    return(0.7)  # Breeding percentage for individuals between 20 and 39 years old
  } else (age > 7) {
    return(0.9)  # Breeding percentage for individuals ages 7 and older 
  }
}

# simulate breeding
Breed = function(pop, pairs, numboff, k, sz, nSNP, nSNP.mig, broodsize, y, mu, mutate, nSNP.cons, pos1, pos2, rr, r, prj, grp, matemigs){
  
  
  # Separate males and females, #Note: IDK if I need this since I already have male and female separate in RunModel.R
  males <- subset(population, sex == 'male')
  females <- subset(population, sex == 'female')
  
  # Loop through each age category
  for (age in unique(pop$age)) {
    # # Calculate breeding percentage for each age group
    # breed_percentage <- cal_breed_percentage(age)
    
    # Sample males and females separately for breeding
    s_males <- sample(males$ID, size = round(nrow(males) * breed_percentage), replace = TRUE)
    s_females <- sample(females$ID, size = round(nrow(females) * breed_percentage), replace = TRUE)
    
    #selects a random subset of male IDs from the males data frame based on the breeding percentage calculated for the given scenario, 
    #and the number of IDs to sample is determined by multiplying the number of males by the breeding percentage.
    
    # Here, you can implement the breeding logic for selected pairs
    
    # Example: Add offspring to the population
    # offspring <- data.frame(ID = 1:100, age = 0, sex = sample(c('male', 'female'), 100, replace = TRUE))
    # population <- rbind(population, offspring)
  }
  
  # Return the updated population
  return(population)
}


####From Ginas code 


#randomly narrow down pairings from pairs if n_pairs > numboff
if(is.null(nrow(pairs))==TRUE){
  print(paste("no pairs of parents available"))
  break
}else if(nrow(pairs)>= numboff){
  #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
  #note this is not prop migrant ancestry, but that migrants are mating so that the number of migrants~=number effective migrants
  if(matemigs == 1){
    migparents = pairs[pairs[,3]==2,,drop=FALSE]           #grab parents with migrant ancestry
    nonmigrents = pairs[pairs[,3]!=2,,drop=FALSE]          #grab parents without migrants
    if(nrow(migparents) < numboff){                        #if there are more offspring needed, grab some nonmig parents
      pairings = sample(1:nrow(nonmigrents), numboff-nrow(migparents), replace = F, prob = NULL) #grab the additional parent pairs needed
      parents <- nonmigrents[pairings,,drop=FALSE]
      parents <- rbind(migparents,parents)                 #make parent matrix
    }else{
      parents <- migparents
      pairings=NULL
    }
    remove(migparents, nonmigrents)
  }else{
    #if not preferentially mating migrants, randonmly select pairs
    pairings = sample(1:nrow(pairs), numboff, replace = F, prob = NULL)
    parents <- pairs[pairings,,drop=FALSE]
  }
}else{
  pairings = NULL
  parents <- pairs
}
#consider if migrants should be preferentially chosen to be parents - should we follow introduced alleles if this is the case?

#generate fecundity for each set of parents
fecundity = sample(seq(1,broodsize,1),nrow(parents),replace=T, prob = NULL) #broodsize is in Cover.R
parents[,3] = fecundity       #cbind(parents, fecundity)
#assigns those values to the parents data frame, 
#specifically in the third column, to represent the number of offspring each parent produces

nbabes = sum(parents[,3])
TEMP = NULL
for(n in 1:nrow(parents)){
  t = parents[n, ,drop=FALSE] #need drop = false or else will lose 
  f = t[1,3]       #use this to store the number of offspring per parent set
  t[1,3] = 1       #use this as a check for later on to make sure this loop works
  while(f > 0){
    TEMP = rbind(TEMP, t)
    f = f - 1
    if(f==0){       #another check so it doesnt get stuck in this loop
      break
    }
  }
}
parents = TEMP
remove(TEMP)


#generate parturitiona and calving percentages in females 


#generate unique IDS
##REMOVED### newid = seq(from = (max(pop[,1])*10) +1, to = (max(pop[,1])*10) + nrow(parents), by = 1)
SZ = seq(from = sz+1, to = sz + nrow(parents), by =1)

babies = matrix(nrow=nrow(parents), ncol=12) #make new matrix for offspring     
colnames(babies) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness", "prop migrant SNPs")
babies[,1] = SZ                   #each individual has unique ID name; sequence starting at 1, through k, with each 1 iteration
babies[,2] = parents[,1]
babies[,3] = parents[,2]
babies[,4] = 0    #first of the year - consider if these should be 0 or -1
babies[,5] = sample(c(0,1),nrow(babies),replace=T)    #each individual assigned male (1) or female (0) #sample from zero nrow times, with replacements. aka set sex
babies[,6] = NA #REMOVED##0                #####sample(c(0,1),nrow(babies),replace=T)    #set allele 1 as either A=1 or a=0
babies[,7] = NA                 #####sample(c(0,1),nrow(babies),replace=T)    #set allele 2 as either A=1 or a=0
babies[,8] = 1      #make every baby alive
babies[,9] = y  #MUST feed y to function   #generation born
babies[,10] = 0      #generation died
babies[,11] = NA      #relative fitness
babies[,12] = NA                            #proportion of migrant SNPs - initial pop will all be 1

#create a check to make sure the correct number of babies are being added to pop
if(nrow(babies) <= numboff){
  bb = nrow(babies)
  #print("no reproduction")
  #return(list(pop,0))
}else if(nrow(babies) > numboff){
  #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
  if(matemigs == 1){
    migbbys = babies[babies[,2]<=0|babies[,3]<=0,,drop=FALSE]
    #if only migrant offspring, select from them the numboff needed
    if(nrow(migbbys) > numboff){
      kept = sample(migbbys[,1], numboff, replace = FALSE, prob = NULL) #remove babies so that you generate only the number needed
      babies = babies[which(babies[,1]%in%kept), , drop=FALSE] 
      remove(kept, migbbys)
      rm=NULL
    }else{
      keep = migbbys[,1]
      if(length(keep)>=1){
        BABIES = babies[-which(babies[,1]%in%keep),,drop=FALSE]
      }else{
        BABIES = babies
      }
      rm = sample(BABIES[,1], nrow(babies)-numboff, replace = FALSE, prob = NULL) #remove babies so that you generate only the number needed
      BABIES = BABIES[-which(BABIES[,1]%in%rm), , drop=FALSE] 
      babies <- rbind(migbbys, BABIES)
      remove(keep, migbbys, BABIES)
    }
  }else{
    #if not preferentially choosing migrant offspring, randomly select to have numboff needed
    rm = sample(babies[,1], nrow(babies)-numboff, replace = FALSE, prob = NULL) 
    babies = babies[-which(babies[,1]%in%rm), , drop=FALSE] 
  }
  bb = nrow(babies)
  remove(rm)
}else if(is.null(nrow(babies))==TRUE){
  bb = 1
  print(paste("only one new baby"))
}

##NOTES 3/14/23 -- preferentially choose the migrant babies 
#babies wiht migrant parents taken out, then randomly select additional babies to be added. 
#will need to do this in allee effect too --migrant parents cannot be removed due to allee effect. 

#rename babies so count doesnt get messed up
babies =  as.matrix(babies)
babies[,1] = seq(from = (sz+1), to = (sz+bb), by = 1)

#genotypes
#prep parent genotypes
f = babies[,2]
m = babies[,3]

fem = pop[-which(pop[,1]%NOTin%f), , drop = FALSE]
mal = pop[-which(pop[,1]%NOTin%m), , drop = FALSE]

if(nrow(mal) == 0){
  print(paste("can't generate father genotypes"))
  break
}
if(nrow(fem)==0){
  print(paste("can't generate mother genotypes"))
  break
}

SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)

babygeno = matrix(nrow=bb, ncol=SNPS)
#loop over each row in babies
for(i in 1:nrow(babies)){
  mom = babies[i,2]
  dad = babies[i,3]
  
  mm = pop[pop[,1] == mom, , drop=FALSE]
  dd = pop[pop[,1] == dad, , drop=FALSE]
  
  momgeno = mm[, -c(ncol(mm)-(SNPS):ncol(mm))] 
  dadgeno = dd[, -c(ncol(dd)-(SNPS):ncol(dd))]
  
  #from each snp (2 columns), grab 1 of mom's alleles
  momgeno.s = pos1 + (sample(0:1, length(pos1), replace=T)) #list of values to pull, exactly 1 allele (here it is index number) from each set of two columns
  momgeno.s = momgeno[momgeno.s] #these are now the actual alleles
  
  #from each snp (2 columns), grab 1 of dad's alleles
  dadgeno.s = pos1 + (sample(0:1, length(pos1), replace=T)) #list of values to pull, exactly 1 allele (here it is index number) from each set of two columns
  dadgeno.s = dadgeno[dadgeno.s] #these are now the actual alleles
  
  #now need to interweve mom and dad's genos so that the loci are jumbled
  babygeno[i,pos1] = momgeno.s
  babygeno[i,pos2] = dadgeno.s
}

if(mutate == 1){  #if mutate is turned "on"
  drift <- babygeno[,1:(nSNP*2),drop=FALSE]
  miggeno <- babygeno[,(nSNP*2+1):SNPS,drop=FALSE]
  for(x in 1:nrow(drift)){    #iterate over indv
    mut <- sample(c("Y","N"), nSNP*2, replace = TRUE, prob = c(mu,1-mu))   #SNPS if for all SNPs, nSNP for only drift SNPs
    init <- drift[x,] ## keep track of the 'ancestral' state within this individual
    drift[x, which(mut=='Y' & drift[x,]==1)] <- 0
    ## if a SNP is supposed to mutate, but its ancestral state was '1' (i.e., it's already been mutated in the previous line),
    ## then set its index in mut to 'N', indicating that no further mutations should happen in this round.
    mut[which(mut=='Y' & init==1)] <- 'N'
    drift[x, which(mut=='Y' & drift[x,]==0)] <- 1
  }
  babygeno <- cbind(drift, miggeno)
}else{
  print(paste("no mutation"))
}
#NOTE -- this allows mutation in conserved and mig snps!!!!

#calculate relative fitness (heterozygosity)
het <- matrix(nrow=nrow(babygeno), ncol=1)
for(g in 1:nrow(babygeno)){
  w <- sum(babygeno[g ,seq(1,ncol(babygeno),2)]!=babygeno[g,seq(2,ncol(babygeno),2)])/(ncol(babygeno)/2)
  het[g,1] <- w
} 
babies[,11] <- het
#note that all SNPs are being considered here -- might want to separate out mig/drift SNPs

#calculate proportion of migrant SNPs
migrantgen <- babygeno[, -c(ncol(babygeno)-(nSNP.mig*2):ncol(babygeno))]
migrantgen <- matrix(unlist(migrantgen), nrow = bb, ncol = nSNP.mig*2)
mSNP <- matrix(nrow = bb, ncol = 1)
for(q in 1:nrow(migrantgen)){
  ww <- sum(migrantgen[q,])/ncol(migrantgen)
  mSNP[q,1] <- ww
}
babies[,12] <- mSNP
#note this might break when bb=1; need to figure that out

NE = matrix(nrow=1, ncol=12)
colnames(NE) <- c("year", "eff_mom", "eff_dad", "nbabies", "naliveadults", "possible_mom", "possible_dad", "eff_mig", "parameterset", "replicate", "project", "group") #just to give a better understanding of what these variables are, set names

NE[1,1] = y                             #grab year
NE[1,2] = length(unique(babies[,2]))    #grab n unique effective moms
NE[1,3] = length(unique(babies[,3]))    #grab n unique effective dads
# NE[1,4] = nrow(babies)                  #grab n babies
# 
# alive = pop[pop[,8]==1,,drop=FALSE]
# adult = alive[alive[,4]!=0,,drop=FALSE]
# adult_f = adult[adult[,5]==0,,drop=FALSE]
# adult_m = adult[adult[,5]==1,,drop=FALSE]
# NE[1,5] = nrow(adult)                   #grab n alive adults
# NE[1,6] = nrow(adult_f)                 #grab n possible moms
# NE[1,7] = nrow(adult_m)                 #grab n possible dads
# 
# mig_f = babies[babies[,2]<=-2,,drop=FALSE]
# mig_m = babies[babies[,3]<=-2,,drop=FALSE]
# NE[1,8] = length(unique(mig_f[,2])) + length(unique(mig_m[,3]))    #grab number of migrant parents
# 
# NE[1,9] = r
# NE[1,10] = rr
# NE[1,11] = prj
# NE[1,12] = grp
# 
# if(isTRUE(y == 1 && r == 1 && rr == 1)){
#   write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
# }else{
#   write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=FALSE, append=TRUE, quote=FALSE, row.names=FALSE)
# }
print(paste("there are", nrow(babies), "babies added to the pop"))

babies = cbind(babies, babygeno)
pop = rbind(pop, babies)

remove(babies, babygeno, dd, fem, het, mal, migrantgen, mm, mSNP, pairs, parents, pairings, 
       t, dadgeno, dadgeno.s, f, fecundity, momgeno, momgeno.s, mut, nbabes, SZ, 
       NE, alive, adult, adult_f, adult_m, mig_f, mig_m)

return(list(pop,bb))
}


#CHANGES TO MUTATION THAT NEED TO BE MADE - 1.13.22
#set a mutation rate so that each SNP has XX chance of mutating
#this should be within Breed.R, not through the lifetime
#make it a reasonable rate for each SNP



#x[mut[mut=="Y"]&x[x==1]] = 0                          #if it should be mutated, if it is a 1, go to zero
#x[mut[mut=="Y"]&x[x==0]] = 1                          #same as above but vise versa





#REMOVED#x = sample(1:nrow(babies), 1, replace = TRUE,)   #find row to mutate
#REMOVED#u = sample(1:nSNP*2, 1, replace = TRUE,)         #find column to mutate 

#REMOVED#if(babies[x, (ncol(babies)-u)] == 1){
#REMOVED#babies[x, (ncol(babies)-u)] = 0
#REMOVED#print(paste("mutated 1 -> 0"))
#REMOVED#}else if(babies[x, (ncol(babies)-u)] == 0){
#REMOVED#babies[x, (ncol(babies)-u)] = 1
#REMOVED#print(paste("mutated 0 -> 1"))
#}



#notes on mutation 2/14: 
#set mutation rate as mu, feed in nSNPs (?)
#loop over each indv in breed:  
#mut = sample(c("mutate":"no mutate"), 2*nSNPs, prob = (1-mu, mu))   #sample to either mutate or not for each indv allele with a probability of mu
#indv[mut[mut=="mutate"]&indv[indv==1]] = 0                          #if it should be mutated, if it is a 1, go to zero
#indv[mut[mut=="mutate"]&indv[indv==0]] = 1                          #same as above but vise versa

#IS THERE A REASON TO TRACK MUTATIONS? i.e. a column with the number SNP mutated ? would that be beneficial to track or just confusing?
#should mutation happen later also (i.e. when parents are breeding?)

#check out lines 129 from Jannas Repro.R in CaptiveBreedingIBM for generating genotypes
#the suggestion is to cbind the genotypes to the babies matrix to better keep track of column numbers

#think about adding in a "generation born" and "generation died" columns in pop
#consider adding a column for calculating parents' lifetime reproductive success -- ORRR print this as a separate table!


#notes 12/29/2021
#ADDING MUTATION
#within the Breed.R function, add in a probability of mutation. (randomly assigned or related to something..?)
#mutate 0->1 or 1->0
#meaning the genotype should be generated, then a random switch across SNPs... right?
#at what frequency should the mutation occur?
#MUTATION NOTES 1/13 -- if happens throughout lifetime, that it more like epi effects** table this idea for another day!
#now, set a MUTATION RATE so that each SNP has a chance of mutating
#think about later when will need to do a sensitivity to make sure that the mutation rate doesnt affect extinction risk. if it does, figure out why and see if it matters! would expect it will be uniform


#BREEDING NOTES FROM JANNA 10/19/2021  
#next steps: make sure to randomize the order of the pairs table before breeding
#then generate fecundity for each pair down th list for a certain number of times to get past the number of offspring needed to reach K
#make sure to buffer by overshooting the number
#then before rturning offspring, drop some until the total number needed
#total offspring/2*fecundity
#check at end of function if below or above K
#make sure to break with an error message if too small
#return offspring and then rbind them to pop

#notes 10/28
#consider adding gamma distribution to fecundity
#take out fecundity=0 - cuz doesnt matter
#think about variable to calculate the ID names using the nrow for total indv created--
# # what the above means is that for every time I add indv (init, migrate, breed), keep a running total of all the indv created so no duplicate values
# # this will be easier than *10 that I have now once I start running over several years


#match mom by ID in pop
#ncol(pop)-(nSNP*2):ncol(pop) #use this to select the genotypes


###notes from 11/4/2021
#make sure to put in a check for small popsizes

#OLD CODE
#singlebabe = NULL
#singlebabe = matrix(1:8,nrow = 1)
#bby = rbind(babies,singlebabe)

#NEED TO FIGURE OUT SOLUTION FOR WHEN THERE IS ONLY ONE BABY 
##ERROR WILL CONTINUE ON LINE 79 UNTIL RESOLVED
