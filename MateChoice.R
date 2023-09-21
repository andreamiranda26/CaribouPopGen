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
  
 
  