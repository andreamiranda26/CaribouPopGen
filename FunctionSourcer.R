#Set working directory, import packages, source functions, 
#setwd(paste(directory,"/source/", sep = ''))    # set temp working directory 
setwd("~/GitHub/CaribouPopGen")

# Modified from FunctionSourcer.R for Lamka and Willoughby 2023

#set working directory, import packages, source functions,


#import packages - only need to do this once then can comment out
#install.packages("reshape2", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("ape", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("adegenet", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("hierfstat", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("plyr", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("gtools", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#.libPaths("/home/gfl0003") #need this when defining location of libraries in HPC

#call installed libraries
library(tidyverse)
library(reshape2)    #need this for plotting
library(adegenet)    #need this for plotting
library(scales)      #need this for plotting, this allows transparency in colors for overlapping lines
library(matrixStats) #need this for colCount and rowCount in ReproSuc.R
library(hierfstat)   #need this for FST analysis in Analyze.R
library(tidyr)       #need this for FST analysis in Analyze.R
library(gtools)      #need this for plotting
#library(MASS)       #used to write out dead indv - only necessary if writing out a matrix

'%NOTin%' <- Negate(`%in%`) #this defines the Not In function so that I can select identities that are not defined by something (see RandomDeaths.R for an example)
#note that the ' ' is not included in the %NOTin% operator -- see https://r-lang.com/not-in-r/#:~:text=The%20not%20in%20operator%20is%20a%20logical%20vector%2C,a%20vector.%20The%20%21%20indicates%20logical%20negation%20%28NOT%29.

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol   #used for removing dead indv in RunModel.R

#source functions
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/AgeDeath.R", sep = ''))
#source(paste(getwd(), "/FitnessDeath.R", sep = '')) #maybe for later 
source(paste(getwd(), "/Migrate.R", sep = ''))
source(paste(getwd(), "/MateChoice.R", sep = ''))
source(paste(getwd(), "/PopSizeNext.R", sep = ''))
#source(paste(getwd(), "/Breed.R", sep = '')) #seems a bit complicated for what I need 
source(paste(getwd(), "/Analyze.R", sep = ''))
source(paste(getwd(), "/Plot.R", sep = ''))       #has on/off switch
source(paste(getwd(), "/Plot2.R", sep = ''))      #has on/off switch
source(paste(getwd(), "/RepSucc.R", sep = ''))

#end of Ginas Packagesfor her code 


#import packages

#install.packages("diveRsity", lib="/scratch/snyder/j/jwillou/isodrift/Rlibs/", repos='http://cran.us.r-project.org')
#install.packages("psych", lib="/scratch/snyder/j/jwillou/isodrift/Rlibs/", repos='http://cran.us.r-project.org')
.libPaths("/scratch/snyder/j/jwillou/isodrift/Rlibs") #include this path to search libraries 
library(diveRsity)
library(strataG)
library(adegenet)

#source functions
source(paste(getwd(), "/RunSims.R", sep = ''))
source(paste(getwd(), "/Advance.R", sep = ''))