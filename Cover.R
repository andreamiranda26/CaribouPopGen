#Cover.R
#This model is written by Gina F. Lamka and used for Caribou Genetics chapter for Andrea 
#adapted from Janna R. Willoughby's Captive Breeding IBM (https://github.com/jwillou/captivebreeding-IBM | doi:10.1111/cobi.13217)

#This Cover.R is used to run the simulation. All scripts are connected via this R script. See the README for more information.

#Set working directory and out directory
setwd("~/GitHub/CaribouPopGen") #working directory from my documents> github folder> caribou pop gen folder with all the code
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")   #  outdir = paste("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM", "/Output_local/", sep = "")
#Source function scripts
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#define location of project and groups for high performance computing cluster
prj = "_proj_"
grp = "_group_"

#parameters
k.V           = 100        #carrying capacity
nSNP.V        = 10         #number of SNPs simulated, used to track drift
miggy.V       = c(0,       #migration parameter type -- set in Migrate.R; 0 = no migration
                  "a",     #"a"=one mig per gen  
                  "b",     #"b"=1xof50@175  
                  "c",     #"c"=3xpf25@175|201|225
                  "d",     #"d"=
                  "e")     #"e"=
LBhet.V       = c(0.45, 0.07) #lowerbound limit for SOURCE POP -- called in RunModel.R
LBp.V         = c(0.45, 0.07) #lowerbound limit for FOCAL POP -- called in RunModel.R
maxage.V      = 13            #maximum age individuals can be
broodsize.V   = 1             #max number of caribou offspring, aka max fecundity  
maturity.V    = 2             #age indv becomes reproductively mature
years.V       = 100           #total run time
r0.V          = 1             #per capita growth rate #0/1 is stable, <0/1 is decreasing, >0/1 is increasing - currently checking cuz r0+1 in log growth eq
#nSNP.mig.V    = 10            #number of migrant specific alleles -- these are ADDITIONAL alleles to nSNP above, migrants = 1, orig pop = 0 -- called in RunModel.R and Breed.R
#nSNP.cons.V   = 0             #number of conserved alleles within species -- used to track mutation

### when adding additional variables, don't forget to add 3 times in Cover.R below, in RunModel.R, and other functions that need the variable fed in

#generate list of parameter combinations
parameters = expand.grid(k.V, nSNP.V, miggy.V, LBhet.V, LBp.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V)
colnames(parameters) = c("k", "nSNP", "miggy", "LBhet", "LBp", "maxage", "broodsize", "maturity", "years", "r0", "nSNP.mig", "nSNP.cons")
write.table(parameters, paste(directory, "/Output/parameters__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

#clean up, remember that these are still available in parameters
remove(k.V, nSNP.V, miggy.V, LBhet.V, LBp.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V) 

#on/off switches for functions
replicates    = 20 
allee         = 1    #1=yes, 0=no
matemigs      = 1    #1=yes, 0=no
plotit        = 0    #1=yes, 0=no
plotit2       = 0    #1=yes, 0=no
mutate        = 1    #1=yes, 0=no   #average mammalian genome mutation rate is 2.2 x 10^-9 per base pair per year, https://doi.org/10.1073/pnas.022629899
#bannertailed krats = 0.0081 mutants/generation/locus, in Busch, Waser, and DeWoody 2007 doi: 10.1111/j.1365-294X.2007.03283.x.
mu            = 0.001  #mutation rate

#bottleneck parameters
# styr          = 100 #year to start pop decline
# nwk           = 300 #pop size after decline 
# drp           = 10  #number of years to drop from k to nwk
# dur           = 40  #duration of small pop size before pop growth 
# edyr          = styr+drp  #year to end pop decline, first year at low pop size (nwk)
# s             = 5000 #size of source pop

#run model iterating over parameters 
theEND = NULL
repEND = NULL
finalPOP = NULL
#r=1  #use this when debugging, remove this when not skipping through the below line
for(r in 1:nrow(parameters)){
  ALL = RunModel(parameters, r, directory, replicates, prj, grp)
  FINAL = ALL[[1]]
  REP = ALL[[2]]
  #POP = ALL[[3]]  #keep if want to hold indv and genotype data, slows computation time considerably
  
  theEND = rbind(theEND, FINAL)  #final data calculated in Analyze.R
  repEND = rbind(repEND, REP)    #final data calculated in RepSucc.R
  #finalPOP = rbind(finalPOP, POP)  #keep if want to hold indv and genotype data, slows computation time considerably
}
#write out data tables
write.table(theEND, paste(directory, "/Output/summary__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(repEND, paste(directory, "/Output/rep_summary__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#####REMOVED##write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this code for a .txt file, good for in a text editor. ; "/t" for macs
#summary table should have nrows = nparameters * nyears * nreplicates
#^^ may have some fewer than above because some simulations may break before all years are able to be run

Plot(theEND)
Plot2(repEND)

#add celebratory note at the end, because it worked!
print(paste("WHOOP"))


#Notes on things that will need changed when going from a local computer to a high performance computing cluster:
#makefile and run_model.sh needs to be changed each run
#Cover.R needed to change the working directory and FunctionSourcer.R path
#reload and call required R packages (and make sure it uses the correct R version)
#remove dead indv to make it faster to run (in RunModel.R)
#add write.table for the final datasets and parameters, typically done in makefile
#check all functions so that when checking/removing dead, that if nrow(dead)==0, pop is unchanged (fix this by using %NOTin% dead rather than -which%in%dead)
#add an empty Output folder to put tables and figs in

#major things edited Dec 2022 [in progress]
#hold more data in Analyze -- n killed, n oldies, n heterozy killed
#CRASH THE POP