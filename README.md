# Dryland Temporal Sensitivity 

Description of key scripts and work flow for 'Biotic versus abiotic controls on 
temporal sensitivity of primary production to precipitation across 
North American drylands' published in New Phytologist 2021

This project was conducted in R studio Version 1.3.1093 using R version 4.0.3

These scripts were written by Andrew J. Felton

#Run everything 

Script: run_everything.R

Use the script 'run_everything.R' to install and load packages, set working directory,
load in-house functions, and to call the key scripts from source. Running this script
will allow you to run all code at once (~ 10 minutes), and thus produce and save
all figures and tables in the Tables_Figures.R folder

# Step 1: Integrate data

Script: Format_data.R

This script combines two data frames to produce one spatiotemporal data frame (30-year)
of net primary productivity, annual and mean (water-year) precipitation, and mean % ground cover
of herbaceous (grasses + forbs) vegetation across five western US dryland ecoregions

# Step 2: Run spatiotemporal analyses for five different models

Script: core_loop.R

This script runs a spatial block bootstrapping procedure to run five different models
and store their coefficients and model fits metrics (1000 iterations) into lists.
The spatial block bootstrapping procedure randomly selects a small subset of
pixels and stratifies this selection by 1) ecoregion type and mean annual precipitation
for that ecoregion, selecting either above or below the mean annual precipitation

# Step 3: Turn model coefficients and model fit metrics stored in lists into data frames

Script: coefficients_dataframe_production.R

This script creates data frames from the previously generated lists of coefficients 
and model fit metrics to aid subsequent analyses and figure production. This script 
also outputs and saves tables of summary statistics of the different models

# Step 4: Creates figures, tables, and supporting information

Figure_1.R: Creates a four-panel map of ecoregion boundaries, mean annual precipitation,
mean net primary productivity, and mean % ground cover of herbaceous vegetation

sensitivity_maps.R: Creates a four-panel map comparing observed spatial variation in temporal
sensitivity with predictions from three different models where the annual precipitation
deviation from mean precipitation interacts with either mean annual precipitation, 
mean % ground cover of herbaceous vegetation, or ecoregion type

coefficients_distributions.R: Creates a three-panel figures comparing ecoregion-specific
coefficient distributions of temporal sensitivity and the spatiotemporal interactions
of annual precipitation deviation with mean annual precipitation and mean % ground
cover of herbaceous vegetation

variance_partitoning.R Creates a two-panel figure comparing sources of spatiotemporal
variation in net primary productivity

Supporting_Figures.R: Creates supporting figures and tables summarizing and comparing
the vegetation structure of the five ecoregions, as well as distributions of AIC
estimates for the five different models.



