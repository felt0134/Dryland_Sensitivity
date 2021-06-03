# Script run which to call and run other key scripts (tasks) in this project. 
#From this, you can set up the directory, import data and set up key data frames,
# and run models

# start clean
rm(list=ls())

# Import packages
pkgs <- c("dplyr", "splitstackshape", "raster", "reshape2", "cowplot","ggplot2",
          "rgdal","rstudioapi","tidyr","cowplot")
#install
#lapply(pkgs, install.packages, character.only = TRUE)

#load
lapply(pkgs, library, character.only = TRUE)

# Set working directory 
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Call to and run key scripts

# Load functions needed for data formatting, analysis, and plotting
source("Functions.R") 

# Format data
source("Format_data.R")

# Stratified spatial block bootstrapping and regressions
source("core_loop.R") 

# Turn coefficients to data frames and output tables of summary statistics 
source("coefficients_dataframe_production.R") 

# Make four panel map of study region
source("Figure_1.R")

# Make maps of predicted and observed temporal sensitivity 
source("sensitivity_maps.R")

# Make figure looking at change sin sensitivity with mean annual precipitation
# and % herbaceous vegetation cover

source("coefficient_distributions.R")

# Make plot of correlations between predicted and observed temporal sensitivity 
source('variance_partitioning.R')

# Make some supporting figures
source("Supporting_Figures.R")


# to do:

#write up to the readme doc



