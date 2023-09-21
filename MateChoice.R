#Mate
#used for Caribou Genetics Andrea, code from Lamka Willoughby 2023
#tweaked 

MateChoice = function(pop, sex, maturity, allee, matemigs){
  dead = pop[pop[,9] == 0, , drop=FALSE]          #remove dead indvs
  pop = pop[which(pop[,1]%NOTin%dead), , drop=FALSE]
  
  immature  = pop[pop[,2 < maturity, ,drop=FALSE]          #remove immature indvs
  pop =       pop[pop[,2 >= maturity, ,drop=FALSE]         #pop without immature
  #since not returning pop, don't need to re-add pop and immature at end of function


  
  # Function to simulate random mating
  simulate_mating <- function(pop) {
   
    
    # Iterate through females and males, allowing them to mate
    for (sex in c("females", "males")) {
      for (individual in population[[sex]]) {
        # Choose a random mate from the opposite sex
        mate_sex <- ifelse(sex == "females", "males", "females")
        mate <- sample(pop[[mate_sex]], size = 1)
        
        # Simulate mating and producing offspring
        offspring_sex <- sample(0:1, size = 1)  # Randomly assign offspring's sex
        offspring[[mate_sex]] <- c(offspring[[mate_sex]], offspring_sex)
      }
    }
    
    return(offspring)
  }
  
  # Run the simulation for a specified number of generations
  for (gen in 1:num_generations) {
    offspring <- simulate_mating(pop)
    
    # Update the population with the new generation
    pop$females <- offspring$females
    pop$males <- offspring$males
    
    # You can analyze or record data about each generation here
  }
  
  # Final population after the simulation
  final_population <- pop
  
  
  ##from HELP 
  
  # Set parameters
  num_males <- 100  # Number of males
  num_females <- 100  # Number of females
  num_generations <- 10  # Number of generations to simulate
  
  # Create a dataset representing individuals with age information
  set.seed(123)  # For reproducibility
  males <- data.frame(
    ID = 1:num_males,
    Age = sample(1:10, num_males, replace = TRUE)  # Random age between 1 and 10
  )
  
  females <- data.frame(
    ID = 1:num_females,
    Age = sample(1:10, num_females, replace = TRUE)
  )
  
  # Define mating probabilities based on age
  # For example, males aged 5 have a higher probability of mating
  mating_probabilities <- c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.05, 0.05, 0.05, 0.05)
  
  # Initialize vectors to track mating pairs
  mating_pairs <- data.frame(MaleID = integer(0), FemaleID = integer(0))
  
  # Simulate the mating process for multiple generations
  for (gen in 1:num_generations) {
    # Select males to mate based on age and probabilities
    selected_males <- sample(males$ID, size = length(females$ID), prob = mating_probabilities, replace = TRUE)
    
    # Create mating pairs
    mating_pairs_gen <- data.frame(MaleID = selected_males, FemaleID = females$ID)
    
    # Append mating pairs to the overall list
    mating_pairs <- rbind(mating_pairs, mating_pairs_gen)
  }
  
  # Results: mating_pairs dataframe contains male-female mating pairs
  
 
  