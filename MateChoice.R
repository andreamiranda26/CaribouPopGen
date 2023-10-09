#Mate
#used for Caribou Genetics Andrea, code from Lamka Willoughby 2023
#tweaked 

MateChoice = function(pop, sex, maturity){ #allee, matemigs
  dead = pop[pop[,9] == 0, , drop=FALSE]          #remove dead indvs
  pop = pop[which(pop[,1]%NOTin%dead), , drop=FALSE]
  
  immature  = pop[pop[,2 < maturity, ,drop=FALSE]          #remove immature indvs
  pop =       pop[pop[,2 >= maturity, ,drop=FALSE]         #pop without immature
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
  }else{
    #REMOVED###turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
    #REMOVED###sex <- c(0,1)[which.min(tabulate(match(pop[,'sex'], c(0,1))))]
    
    #fem = 0 #this matches males to females by putting the first column always as females, this would make them all females 
    
    #match those of opposite sex with replacement*
    
  #############################################################
  
  ##from HELP 
  
  # Set your desired ratio of males to females
  male_ratio <- 0.3  # this would be a 30% males 70% females ratio which is what is found in the literature 
  
  # set parameters
  #Calculate the number of males and females based on the desired ratio
  num_males <- round(k * male_ratio) # It ensures that the result is a whole number.
  num_females <- (k - num_males)
  num_females <- 100  # Number of females
 # num_generations
  # # Set parameters
  # num_males <- 100  # Number of males
  #  <- 10 # Number of generations to simulate
  
  # Create a dataset representing individuals with age information
  # Step 1: Create the initial data frame
  set.seed(123)  # Set a seed for reproducibility
  num_individuals = popsize
  mate <- data.frame(
    ID = 1:num_individuals,
    sex = sample(c(0,1), size= num_individuals, replace = TRUE),
    age = sample(maturity:maxage, num_individuals, replace = TRUE),  # Assuming age ranges from 2 to 13, that the maturity and  max age respectively. 
    breed_perc = rep(0, num_individuals)  # Initialize breeding percentage to 0
  )
  
  # Step 2: Define rules for breeding percentage based on age
  breed_rules <- function(pop, maxage, maturity, sex) {
    if (sex == "male") {
      return(0)  # Males do not contribute to breeding
    } else {
      # Define rules for females, e.g., increase breeding percentage with age
      # You can customize this function based on your specific requirements
      return(age * 5)  # Example: Breeding percentage increases by age * 5
    }
  }
  
  # Step 3: Loop through ages and update breeding percentages
  for(age in unique(pop$age)) {
    for (sex in c("male", "female")) {
      subset_df <- pop[pop$age == age & pop$sex == sex, ]
      if (nrow(subset_df) > 0) {
        breeding_pct <- breeding_rules(age, sex)
        pop[pop$age == age & pop$sex == sex, "breeding_percentage"] <- breeding_pct
      }
    }
  }
  
  # Step 4: Simulate breeding for each age group
  for (age in unique(pop$age)) {
    males <- pop$ID[pop$age == age & pop$sex == "male"]
    females <- pop$ID[pop$age == age & pop$sex == "female"]
    
    # Perform breeding simulation (e.g., using sample())
    # You can customize this step based on your breeding algorithm
    # For example: randomly select pairs of males and females to breed
    # Update the breeding percentage in the data frame accordingly
  }
  
  
  # # Define mating probabilities based on age
  # # For example, males aged 5 have a higher probability of mating
  # mating_prob <- c(0, 0, 0.05, 0.1, 0.15, 0.15, 0.15, 0.1, 0.05, 0.05, 0.05, 0.05,0.05) #bulls are fully mature by age 5 
  # #probabilities are 0 in ages 1-3, 10-30% breeding in ages 4-7, >50% 8-13. 
  
  # Initialize vectors to track mating pairs
  mating_pairs <- data.frame(MaleID = integer(0), FemaleID = integer(0)) #the mating_pairs data frame is initially empty, with two columns, MaleID and FemaleID, both of which are expected to contain integer values.
  
  # Simulate the mating process for multiple generations
  for (gen in 1:num_generations) {
    # Select males to mate based on age and probabilities
    selected_males <- sample(males$ID, size = length(females$ID), prob = mating_prob, replace = TRUE)
    
    # Create mating pairs
    mating_pairs_gen <- data.frame(MaleID = selected_males, FemaleID = females$ID)
    
    # Append mating pairs to the overall list
    mating_pairs <- rbind(mating_pairs, mating_pairs_gen)
    
    
    # Simulate mating and producing offspring
    offspring_sex <- sample(0:1, size = 1)  # Randomly assign offspring's sex
    offspring[[mating_pairs]] <- c(offspring[[mating_pairs]], offspring_sex)
    
    return(offspring)
    
  }
  
  # Results: mating_pairs dataframe contains male-female mating pairs
  
 
  