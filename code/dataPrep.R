# ==============================================================================
# STM Preprocessing Pipeline
# ==============================================================================
# NOTE: Raw translated AQAP statements are from proprietary
# from the SITE Intelligence Group and cannot be shared.
# This script documents the preprocessing steps applied.
# 
# The processed corpus (procsDocsUpper_0.7.Rdata) was created using the stm
# package's standard preprocessing pipeline with the following parameters:

# Preprocessing specifications:
# - Language: English 
# - Lowercase: TRUE
# - Remove stopwords: TRUE
# - Remove numbers: TRUE
# - Remove punctuation: TRUE
# - Stemming: TRUE
# - Lower threshold: documents containing rare words were filtered
# - Upper threshold: 0.70 

# This filtering balances vocabulary size with discriminating power,
# removing both very rare words (likely noise/OCR errors) and very common
# words (limited discriminating value).
# The above parameters reflect a standard stm::textProcessor() 
# and stm::prepDocuments() workflow.
# The processing was developed on STM Version 1.2
# as a result the workflow may no longer be compatible with current or future
# stm releases.
# To ensure replicability, I include an .Rdata file with 
# processed data and a pre-computed stm models.
# The processed corpus provided enables full replication of all
# downstream analyses.

# Load the processed corpus:
load("./data/selectModAQAP70UT.Rdata") 

## Corpus statistics, from procsDocsUpper_0.7 object:

cat("Documents:", length(procsDocsUpper_0.7$documents), "\n") #809
cat("Vocabulary size:", length(procsDocsUpper_0.7$vocab), "\n") #7764
cat("Total tokens:", sum(sapply(procsDocsUpper_0.7$documents, 
                                function(x) sum(x[2,])))) #464395
    