
## Data:
## Starting from processed and tagged data
## For both communiques and stories
## Reason: The formatted articles are proprietary
## So my agreement was that I could distribute processed data objects rather than original data.

## AQAP Descriptive Statistics:

# Small script to produce AQAP growth plot
# from REVMOD data
source("AQAP_Growth_Viz.R")
# In: rdynv2_AQAP_subset.csv (revmod subset)
# Out: aqapgrowthchart.pdf

## Use UCDP data to plot dyad patterns across time:
source("AQAP-Dyad-Plot.R")
# In: gedevents-2021-AQAP2.csv (ucdp AQAP subset)
# Out: AQAPActivities.pdf

## Random Forest and TSNE analysis

# RF stratified to account for imbalance
# This version has the tsne & visualization code
source("randomForestAndPCA2022.R")
# In: develop_df.csv, validate_df.csv
# Out: training/test confusion matrices
# pca_proximityRF.pdf, randomForestRes.pdf
# Dev and validation tsne visualization plots

# Script to generate the K-fold misclassification
# Probability between Ansar al-Shariah and AQAP:
# Stratified Random Forest
source("stratifiedKfoldRandomForestJustSunni.R")
# In: develop_df.csv, validate_df.csv
# Out: KfoldClassErrorSunniOnly.pdf, randomForestComparison.pdf
 
# Topic Model Topic Clusters
source("PlotTopicClusters.R")
#In, modified STM Utils: STMfunctionsalt.R, plotcontinuousalt.R
#produce_cmatrixalt.R, simbetasalt.R
# In: selectModAQAP70UT.Rdata
# Out: localtranscluster.pdf

## Topic model visualss
source("UT70SelectModPlots_C.R")
#In: selectModAQAP70UT.Rdata
#Out:
# Out- Data: estimatedEffect_UT70_18Model.Rdata
# Out- plots: topicProportions_UT70EDModel.pdf,
# HouthiandLT_UT7_K18_top.pdf,
# HouthiandLT_UT7_K18_bottom.pdf
# AASAndMiscYemen_UT7_K18_top.pdf,
# AASAndMiscYemen_UT7_K18_bottom.pdf
# ClashFactions_UT7_K18_top.pdf,
# ClashFactions_UT7_K18_bottom.pdf
# TransJihadiAlt_UT7_K18_top.pdf,
# TransJihadiAlt_UT7_K18_bottom.pdf

