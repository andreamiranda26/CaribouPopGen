

#incorporating breeding and breeding ground switching 

switching_prob <- 0.05  # Percentage switching breeding grounds in each time step
maturity 

# Parameters
# total_agents <- 200  # Total number of agents
# breeding_ground1_capacity <- 50
# breeding_ground2_capacity <- 60
switching_prob <- 0.05  # Percentage switching breeding grounds in each time step
maturity 

# Initialize agents
agents <- data.frame(
  id = 1:total_agents,
  age = sample(1:10, total_agents, replace = TRUE),  # Age distribution
  subgroup = sample(1:2, total_agents, replace = TRUE)  # Initial subgroup (1 or 2)
)

# Define a function to determine breeding ground switching
breeding_ground_switching <- function(agent) {
  if (agent$age >= maturity_age && runif(1) < switching_prob) {
    # Switch breeding grounds
    agent$subgroup <- 3 - agent$subgroup  # Toggle between 1 and 2
  }
  return(agent)
}

# Simulate agent interactions and switching
time_steps <- 100

for (t in 1:time_steps) {
  # Implement agent interactions, mate choice, and other relevant behaviors
  
  # Apply breeding ground switching logic
  agents <- lapply(agents, breeding_ground_switching)
}

# Calculate the number of agents in each breeding ground at the end
num_agents_in_ground1 <- sum(agents$subgroup == 1)
num_agents_in_ground2 <- sum(agents$subgroup == 2)

# Print results
cat("Agents in Breeding Ground 1:", num_agents_in_ground1, "\n")
cat("Agents in Breeding Ground 2:", num_agents_in_ground2, "\n")
