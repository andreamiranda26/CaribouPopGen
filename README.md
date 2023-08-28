# CaribouPopGen

Modified code from Willoughby's Isolation-Drift (description of model below is similar to that IsoDrift Code)
R code will simulates breeding ground movement and how that impacts genetic structure over time in Mulchatna caribou herd. It also calls Structure to evaluate the impact. Analysis also includes measurement of heterozygosity, Fst, and DAPC (discriminant analysis of principle components) to futher quantify the effects of isolation in the two populations.

Use PopGenModel.R to control:

input of genotypes (alleles and allele frequencies)
simulation parameters
running of program
PopGenModel.R will call FunctionSourcer.R (stored in source directory), sources calls RunSims.R and Advance.R (also in source directory). Output written to a directory called 'output'.

Program will output 4 files for each of the population sizes simulated. Files written will include:

dpc.csv: results of dicriminant analysis of principle components. Columns contents (in order): population size, replicate number (set in IsoDriftModel.R as reps), year, maximum age allowed in the simulation, proportion of individuals in source population that assigned to the source population, proporiton of individuals in sink (isolated) population that assigned to the larger source population.
eva.csv: results of structure analysis. Columns contents (in order): population size, replicate number, year, maximum age allowed in the simulation, penalized log likelihood for K = 1, penalized log likelihood for K = 2, penalized log likelihood for K = 3.
fst.csv: estimates of Fst between source and sink populations. Columns contents (in order): population size, replicate number, year, maximum age allowed in the simulation, and Fst between the two simulated populaitons.
het.csv: heterozygosity estimated each year in the sink population. Columns contents (in order): population size, replicate number, year, maximum age allowed in the simulation, and heterozygosity estimate.
