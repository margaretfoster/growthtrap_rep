# Replication Package: Multi-Source Machine Learning for Opaque Organizations
Measuring Strategic Change in Data-Sparse Environments

### Author: 
Margaret J. Foster

### Affiliation: 
National Security and Data Policy Institute, University of Virginia

### Contact: 
m.jenkins.foster@gmail.com
### Date: 
October 2025

## Overview

This replication package accompanies the manuscript “Multi-Source Machine Learning for Opaque Organizations.”
It provides the code, processed data, and environment files necessary to reproduce the analyses and figures reported in the article.

The project applies interpretable machine learning and topic modeling to analyze official communications and behavioral indicators of al-Qaeda in the Arabian Peninsula (AQAP), demonstrating how transparent modeling can illuminate the evolution of covert organizations.

This project uses NLP (via the Structural Topic Model) and machine learning (Stratified Random Forests) to quantitatively test theoretical expectations of changes in the geopolitical risk environment. It uses al-Qaeda in the Arabian Peninsula (AQAP) as a case study because the Yemeni civil war has provided longstanding threats to geopolitical, human, and maritime security.

## Structure

```markdown
## Directory Structure
├── code/ # Analysis and plotting scripts
│ ├── master_run.R # Main orchestration script
│ ├── AQAP_Growth_Viz.R
│ ├── AQAP-Dyad-Plot.R
│ ├── randomForest_2025.R
│ ├── dataPrep.R
│ ├── gen_topic_clusters_2025.R
│ └── PlotTopicClusters.R
│
├── data/ # Input and intermediate data
│ ├── aqap_growth.csv
│ ├── ucdp_summary.csv
│ ├── tfidf_merged_576.csv
│ ├── stm_processed_corpus.RData
│ ├── selectModAQAP70UT.RData
│ └── precomputed_topic_groups.RData
│
├── figures/ # Output figures
│ ├── aqapgrowthchart.pdf
│ ├── AQAPActivities.pdf
│ └── localtranscluster.pdf
│
├── outputs/ # Model outputs, diagnostics, etc.
│
├── renv/ # Local R environment files
│
└── README.md # This document
```

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
The project automatically activates renv when opened. 
If running from the command line, first run renv::activate() manually before sourcing the replication script.

## Outputs
figures/aqapgrowthchart.pdf    AQAP organizational growth (REVMOD data)
figures/AQAPActivities.pdf    UCDP dyad activity patterns
outputs/randomForest_results.csv    Random forest performance metrics
figures/localtranscluster.pdf    Temporal trends in local vs transnational topic clusters

## Notes on Replicability

- All analyses assume the project root is set using `here::here()`.
- The STM components rely on **precomputed topic distributions** to avoid instability from changes in the `stm` package over time.
- Proprietary text data are represented only in processed or aggregated form; these are sufficient to reproduce all figures and results in the paper.
- Figures and outputs will be generated into `figures/` and `outputs/` directories automatically.

## Keywords
interpretable machine learning, topic modeling, organizational change, terrorism, replication, structural topic model

## License
This replication package is distributed under the MIT License.  
Users may reuse and adapt the code with attribution.

## Citation

If you use these materials, please cite:

Foster, M. J. (2025).  Multi-Source Machine Learning for Opaque Organizations. Manuscript under review at PLOS ONE.
