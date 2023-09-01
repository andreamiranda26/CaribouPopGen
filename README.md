# CaribouPopGen


Chapter 1: Genetic Analysis of the Mulchatna Caribou Herd from Southwest Alaska

Code assessed here is used for the first chapter of dissertation at Auburn University.

Model description last updated: 08/31/23

Keywords:
- population structure
- genetics
- caribou
- Alaska
- wildlife conservation

Model Objectives

1. Gather information on the population structure of the Mulchatna caribou herd.
2. Use genetic values and collected collar data to examine how long we will expect to see a genetic separation between subgroups observed through the amount of breeding ground switching.
3. Determine how this can be used for future management and conservation of this declining resource.

Our aim in conducting these analyses is to gain insights into the genetic structure of the herd, the interconnections, and variances between these separated groups in terms of their spatial distribution.

# Model Overview

We will design an agent-based forward time model to simulate the length of time it takes to observe a genetic separation between subgroups observed in the Mulchatna caribou herd. We will simulate sexually reproducing diploid individuals (e.g. caribou) with discrete time intervals and generation times. We will simulate the Mulchatna caribou herd that randomly mates, produces calves, and is subject to mortality. We will output estimates of Fst, heterozygosity, results of structure analysis, and a PCA or PCoA? We will allow our model to persist for 100 years to understand if the current genetic composition of the herd in addition to the current spatial movement (breeding ground switching) observed with collar data will lead to genetic separation of the herd into subgroups. 

# Initializing Model Design

Populations will start with a population size (popsize), with overlapping generations that will allow it to be simulated as a natural caribou population. Alleles (0 and 1) will be randomly assigned to each individual with equal probability for all loci. Loci will be simulated for SNP (n = 10) molecular markers, as the results provide conclusions from coding genetic data. The maximum age of individuals (13 years), number of offspring (1), popsize (100), and the age at maturity (2 years), percent breeding ground switching (16%), and sex ratio (1 male: 2 females) will all be parameters fed to the simulation.

# Model Details

Individual caribou agents will be simulated throughout their lifetime to determine the population-level effects on the genetic structure of breeding ground switching between determined subgroups within the herd. To initialize the population, three subgroups will be simulated and individuals will be aged so that the age of every individual in the population increases by one each year. Following aging, individual caribou can be removed such that each caribou individual has a percentage of survival (ranging from .85-0.90% survival rate). Breeding ground switching will occur by simulating a proportion of individuals (starting with 16% of individuals from popsize, switching at least once) percentage of breeding ground switching will be a parameter that can be modified depending on popsize. Individuals in the population choose mates randomly so that one male and one female will mate every year, with new reassignments to new mates every year. Each female will be given a probabiliy of becoming parturient (0.89) and successfully calving of 0.69. 

# Model Calibration with Empirical Data 

For this model, empirical data was used, however, further analysis and calibration of this simulation may exploit empirical data. 

# Expected Results

We hypothesize that the fidelity of female caribou to their calving sites will be evident both in the data collected from radio and satellite tracking, as well as in the genetic divergence. This loyalty of females to their calving grounds is well-documented. Also in real world populations, due to the high mobility of caribou as a migratory species, we expect these genetic differences to be minimal (Boulet et al. 2007). Considering the observed rates of breeding ground switching, it is anticipated that the division of the Mulchatna caribou herd into distinct subgroups would require a significant amount of time before it is regarded as more than a single herd. Our aim in conducting these analyses is to gain insights into the genetic structure of the herd over time as female caribou switch breeding grounds at a certain percentage or rate.

We expect that as breeding ground switching values increase, the Fst values will be small and heterozygosity values will be low as greater breeding ground switching will most likely lead to mixing between populations and indistict subgroups. 

# Potential Challenges




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
