# Replication Package: Multi-Source Machine Learning for Opqaue Organzations
Measuring Strategic Change in Data-Sparse Environments

Author: Margaret J. Foster
Affiliation: National Security and Data Policy Institute, University of Virginia
Contact: m.jenkins.foster@gmail.com
Date: October 2025

## Overview

This replication package accompanies the manuscript “Multi-Source Machine Learning for Opqaue Organzations.”
It provides the code, processed data, and environment files necessary to reproduce the analyses and figures reported in the article.

The project applies interpretable machine learning and topic modeling to analyze official communications and behavioral indicators of al-Qaeda in the Arabian Peninsula (AQAP), demonstrating how transparent modeling can illuminate the evolution of covert organizations.

This project uses NLP (via the Structural Topic Model) and machine learning (Stratified Random Forests) to quantitatively test theoretical expectations of changes in the geopolitical risk environment. It uses al-Qaeda in the Arabian Peninsula (AQAP) as a case study because the Yemeni civil war has provided longstanding threats to geopolitical, human, and maritime security.

## Structure

Replication/
│
├── code/                # All analysis scripts
│   ├── AQAP_Growth_Viz.R
│   ├── AQAP-Dyad-Plot.R
│   ├── randomForest_2025.R
│   ├── dataPrep.R
│   ├── gen_topic_clusters_2025.R
│   ├── PlotTopicClusters.R
│   └── master_run.R       # Main orchestration script
│
├── data/                # Input data (processed only)
│   ├── aqap_growth.csv
│   ├── ucdp_summary.csv
│   ├── tfidf_merged_576.csv
│   ├── stm_processed_corpus.RData
│   ├── selectModAQAP70UT.RData
│   └── precomputed_topic_groups.RData
│
├── figures/             # Generated figures (output)
│   ├── aqapgrowthchart.pdf
│   ├── AQAPActivities.pdf
│   └── localtranscluster.pdf
│
├── outputs/             # Model objects, diagnostics, etc.
│
├── renv/                # R environment management files
│
└── README.md            # This document


## Data and Sources
All data are derived from processed and tagged sources.
Raw texts cannot be distributed due to licensing and data-sharing restrictions, but all intermediate objects required to reproduce figures are included.

**Data files:**
1. `data/aqap_growth.csv` — Group size estimates (REVMOD)
2. `data/ucdp_summary.csv` — Conflict event summary (UCDP)
3. `data/tfidf_merged_576.csv` — News article TF-IDF matrix (ICEWS)
4. `data/stm_processed_corpus.RData` — AQAP propaganda corpus (SITE, processed)

**Precomputed binaries:**
1. `selectModAQAP70UT.RData` — Original STM model and metadata
2. `precomputed_topic_groups.RData` — Stable cluster-level topic summaries

## Software and Environment

All analyses were conducted in R 4.4.0 using the following major packages:

Category    Packages
Core data handling    tidyverse, dplyr, readr, here
Visualization    ggplot2, RColorBrewer, ggalluvial, reshape2
Machine learning    randomForest, caret
Topic modeling    stm (legacy functions for backward compatibility)
The project uses `renv` for reproducible environments.  

## Execution Instructions

To run the full analysis:

```r
install.packages("renv")
renv::restore()
source("code/master_run.R")
```

## Outputs
figures/aqapgrowthchart.pdf    AQAP organizational growth (REVMOD data)
figures/AQAPActivities.pdf    UCDP dyad activity patterns
outputs/randomForest_results.csv    Random forest performance metrics
figures/localtranscluster.pdf    Temporal trends in local vs transnational topic clusters

##Notes on Replicability

The STM portion relies on precomputed topic distributions to ensure stability across package versions.

All scripts assume the project is run using here::here() for relative paths.

Proprietary text data are represented only in derived and aggregated form.

##Citation

If you use these materials, please cite:

Foster, M. J. (2025).  Multi-Source Machine Learning for Opqaue Organzations. Manuscript under review at PLOS ONE.
