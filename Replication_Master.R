# Load renv environment
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::activate()

## Load libraries needed:
## MJF To do: deduplify and standardize into a
## renv() lock,

required_packages <- c('tidyverse','dplyr', "here",
                'randomForest','caret',
                'ggplot2', 'ggalluvial', 'readr', 
              'RColorBrewer', 'reshape2')

invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# Set directory paths 
rootPath   <- here::here() #sets directories relative to where this script is
dataPath   <- file.path(rootPath, "data")
codePath   <- file.path(rootPath, "code")
figPath    <- file.path(rootPath, "figures")
outPath    <- file.path(rootPath, "outputs")

## MJF To-Do: modify the header notes into the readme

##  Descriptive Statistics:
# Small script to produce AQAP growth plot
# from REVMOD data
source(file.path(codePath, "AQAP_Growth_Viz.R"), local = TRUE)
# In: rdynv2_AQAP_subset.csv (revmod subset)
# Out: aqapgrowthchart.pdf

## Use UCDP data to plot dyad patterns across time:
source(file.path(codePath,"AQAP-Dyad-Plot.R"), local = TRUE)
# In: gedevents-2021-AQAP2.csv (ucdp AQAP subset)
# Out: AQAPActivities.pdf

## Random Forest classification
source(file.path(codePath,"randomForest_2025.R"), local = TRUE)
# In: develop_df.csv, validate_df.csv
# Out: random forest results and diagnostics

##%%%%%%%%%%%%%%%%
## Topic Modeling: Prep and Details
##%%%%%%%%%%%%%%%% 

## Data preparation steps:
source(file.path(codePath, "dataPrep.R"), local =TRUE)

# Topic Model Thematic Groupings
# script implements custom topic grouping
# NOTE: this is vulnerable to breaking changes in underlying STM infrastructure
# Replication option 1: run using gen_topic_clusters_2025.R 
# Option 2 (default): use precomputed clusters for system and version robustness
source(file.path(codePath,"gen_topic_clusters_2025.R"), local = TRUE)  #optional, runs in a local environment
# Calls modified STMuUtils: STMfunctionsalt.R, plotcontinuousalt.R
#produce_cmatrixalt.R, simbetasalt.R
# In: selectModAQAP70UT.Rdata
# Out precomputed_topic_groups.RData

source(file.path(codePath,"PlotTopicClusters.R"), local=TRUE) 
# in: precomputed_topic_groups.RData
# Out: localtranscluster.pdf

