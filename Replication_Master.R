# ============================================================
#  Replication Master Script
#  Project: Multi-Source Machine Learning for Opaque Organizations
#  Author:  Margaret J. Foster
#  Affiliation: National Security and Data Policy Institute, University of Virginia
#  Contact: m.jenkins.foster@gmail.com
#  Date: October 2025
# ============================================================
#
#  DESCRIPTION:
#  This script runs all replication steps for the manuscript
#  "MultiSource Machine Learning for Opaque Organizations."
#
#  It assumes that the project-level environment has been activated via renv,
#  either automatically (through .Rprofile) or manually by running renv::activate().
#
#  STRUCTURE:
#   1. Load required libraries
#   2. Set directory paths
#   3. Execute analytic modules in sequence:
#        - Descriptive visualizations
#        - Random forest classification
#        - Topic modeling and cluster visualization
#   4. Log session information for reproducibility
#
#  NOTE:
#  Run this file from the project root (same directory as renv.lock).
#  All figures will write to the "figures/" subdirectory. 
#  Session informatoin will write to the projet root path 
# ============================================================


# Load packages
required_packages <- c(
  "here", "readr", "reshape2",
   "tidyverse", "dplyr", 
  "randomForest", "caret",
  "ggplot2", "ggalluvial",
  "RColorBrewer"
)

invisible(lapply(required_packages, 
                 require, character.only = TRUE))

# Set directory paths 
rootPath   <- here::here() #sets directories relative to where this script is
dataPath   <- file.path(rootPath, "data")
codePath   <- file.path(rootPath, "code")
figPath    <- file.path(rootPath, "figures")

# Seed for reproducability:
set.seed(6889)

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

# ------------------------------------------------------------
# Topic Model Thematic Groupings
# ------------------------------------------------------------
# Option 1: regenerate clusters from STM model
# (sensitive to breaking changes in STM underlying structure)
# Option 2 (default): load precomputed clusters for robustness
# ------------------------------------------------------------

use_precomputed <- TRUE  # change to FALSE to re-run topic generation

if (use_precomputed) {
  message("Using precomputed topic clusters
          (default for replication robustness).")
} else {
  message("Regenerating topic clusters from STM model")
  source(file.path(codePath, "gen_topic_clusters_2025.R"), 
         local = TRUE)
}

# Plot topic cluster trends (always runs)
source(file.path(codePath, "PlotTopicClusters.R"),
       local = TRUE)

print("Replication finished!")
## Log session info:
sink("sessionInfo.txt")
sessionInfo()
sink()
