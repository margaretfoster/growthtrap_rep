set.seed(6889)
#------------------------------------------------------------
# Load and prepare data
# Because of the class imbalance and relatively small size of the data
# merging both the "dev" and "val" sets
# and validated via cross-validation

dev <- readr::read_csv(file.path(dataPath,"develop_df.csv"))
val <- readr::read_csv(file.path(dataPath, "validate_df.csv"))

# Remove metadata
meta_vars <- c("event.date", "storyid", "test", "X")
dev.df <- dev %>% select(-any_of(meta_vars))
val.df <- val %>% select(-any_of(meta_vars))
## the val columns are out of order:
val_clean <- val[, colnames(dev)]

## merging the data back together
## to cross-validate:
merged <- rbind(dev, val_clean)

dim(merged) #576 2218

table(merged$label)
#36 Ansar al-Shariah; 327 AQAP/Al-Qaeda; 213 Houthi/Ansarallah 

## double confirm that non-numeric and labels are out:
meta_vars <- c("X", "label", "event.date", "storyid")
merged_x <- merged %>% select(-any_of(meta_vars))
merged_y <- as.factor(merged$label)

## ID any non-numeric columns:
non_numeric_columns <- sapply(merged_x, function(x) !is.numeric(x))
names(merged_x)[non_numeric_columns] #0


## Remove data that we no longer need:
rm(merged)
rm(val)
rm(dev)
rm(dev.df)
rm(val.df)

## Prepare cross-validation
## random forest with stratified sampling:
## class distribution of data:
print(table(merged_y))

#------------------------------------------------------------
# K-FOLD CROSS-VALIDATION
#------------------------------------------------------------

set.seed(6889)
k_folds <- 5

## createFolds() automatically stratifies 
## based on the input character vector 
folds <- createFolds(merged_y, #input label vector
                     k = k_folds, 
                     list = TRUE, 
                     returnTrain = FALSE) #return test indicies, not train indicies

cat("Using", k_folds, "-fold stratified cross-validation\n")
cat("Fold sizes:\n")

## Summarize the class distribution in each fold:
##  as close as possible to 7-66-43 across all
for (i in 1:k_folds) {
  fold_labels <- merged_y[folds[[i]]]
  cat(sprintf("  Fold %d: %d total (%s)\n", 
              i, length(folds[[i]]), 
              paste(table(fold_labels), collapse=", ")))
}

#------------------------------------------------------------
# HYPERPARAMETER TUNING
#------------------------------------------------------------

# Define tuning grid
## create grid of combinations of parameters:
param_grid <- expand.grid(
  mtry = c(3, 5, 10, 15, 20),
  nodesize = c(1, 5, 10),
  stringsAsFactors = FALSE
)

cat("Tuning grid:", nrow(param_grid), "parameter combinations\n")
cat("  mtry values:", paste(unique(param_grid$mtry), collapse=", "), "\n")
cat("  nodesize values:", paste(unique(param_grid$nodesize), collapse=", "), "\n\n")

cat("Stratified sampling: 20 observations per class per tree\n\n")

# Storage for CV results
cv_results <- data.frame()

cat("Running grid search with", k_folds, "-fold CV...\n")

## Nested loops:
## outer tries every hyperparameter combination, via param grid 
for (i in 1:nrow(param_grid)) { ## outer loop
  params <- param_grid[i, ]
  
  # Storage for this parameter combination across folds
  fold_metrics <- list()
  
  for (fold in 1:k_folds) {## tests hyperparam combo on each of the 5 folds
    # Split data
    #Create train-test for each fold:
    test_idx <- folds[[fold]] #row index for fold's test set
    train_idx <- setdiff(1:nrow(merged_x), test_idx) # fold-specific training set = the rest
    
    train_x <- merged_x[train_idx, ]
    train_y <- merged_y[train_idx]
    test_x <- merged_x[test_idx, ]
    test_y <- merged_y[test_idx]
    
    # Reset rownames
    rownames(train_x) <- NULL
    rownames(test_x) <- NULL
    
    # Fit model
    
    set.seed(6889 + fold)
    rf_fold <- randomForest(
      x = as.matrix(train_x),
      y = train_y,
      xtest = as.matrix(test_x),# test to compute fold x param level
      ytest = test_y,# predictions
      ntree = 500,
      mtry = params$mtry, # number of features for each split
      nodesize = params$nodesize, # min size of terminal nodes
      strata = train_y, #stratified sampling
      sampsize = rep(20, nlevels(train_y)), # for class imbalance, takes 20 obs per class per tree
      importance = TRUE, #track important features
      keep.forest = FALSE
    )
    
    # Test set predictions
    test_pred <- rf_fold$test$predicted #fold-level predictions
    test_cm <- table(Predicted = test_pred, Actual = test_y) # fold-level confusion matrix
    
    # Calculate metrics
    test_acc <- mean(test_pred == test_y) #fold-level accuracy
    test_bal_acc <- mean(diag(test_cm) / colSums(test_cm)) ## balanced accuracy 
    ## average of per-class accuracy (better for class imbalances)
    
    # class sensitivity
    ## First: calculate recall (sensitivity) for Ansar al-Shariah, the minority class
    #3 if clause to handle cases with no AAH predicted
    # Calculate sensitivity for all three classes
    aas_sens <- if ("Ansar al-Shariah" %in% rownames(test_cm) && 
                    "Ansar al-Shariah" %in% colnames(test_cm)) {
      test_cm["Ansar al-Shariah", "Ansar al-Shariah"] / 
        sum(test_cm[, "Ansar al-Shariah"])
    } else {
      0
    }
    
    aqap_sens <- if ("AQAP" %in% rownames(test_cm) && 
                     "AQAP" %in% colnames(test_cm)) {
      test_cm["AQAP", "AQAP"] / 
        sum(test_cm[, "AQAP"])
    } else {
      NA
    }
    
    houthi_sens <- if ("Houthi/Ansarallah" %in% rownames(test_cm) && 
                       "Houthi/Ansarallah" %in% colnames(test_cm)) {
      test_cm["Houthi/Ansarallah", "Houthi/Ansarallah"] / 
        sum(test_cm[, "Houthi/Ansarallah"])
    } else {
      NA
    }
    
    fold_metrics[[fold]] <- data.frame(
      fold = fold,
      accuracy = test_acc,
      balanced_acc = test_bal_acc,
      aas_sensitivity = aas_sens,
      aqap_sensitivity = aqap_sens,  
      houthi_sensitivity = houthi_sens,
      oob_error = rf_fold$err.rate[500, "OOB"]
    )
  } # close fold in k_folds loop
  

  # Aggregate across folds, for each param set
  fold_df <- do.call(rbind, fold_metrics) #combines all folds into one df
  
  ## calculates mean and SD across all metrics for the 5 folds
  cv_results <- rbind(cv_results, data.frame(
    mtry = params$mtry,
    nodesize = params$nodesize,
    mean_accuracy = mean(fold_df$accuracy),
    sd_accuracy = sd(fold_df$accuracy),
    mean_balanced_acc = mean(fold_df$balanced_acc),
    sd_balanced_acc = sd(fold_df$balanced_acc),
    mean_aas_sensitivity = mean(fold_df$aas_sensitivity),
    sd_aas_sensitivity = sd(fold_df$aas_sensitivity),
    mean_aqap_sensitivity = mean(fold_df$aqap_sensitivity),
    sd_aqap_sensitivity = sd(fold_df$aqap_sensitivity),
    mean_houthi_sensitivity = mean(fold_df$houthi_sensitivity, na.rm = TRUE),
    sd_houthi_sensitivity = sd(fold_df$houthi_sensitivity, na.rm = TRUE),
    mean_oob_error = mean(fold_df$oob_error),
    sd_oob_error = sd(fold_df$oob_error)
  ))
  
  if (i %% 3 == 0 || i == nrow(param_grid)) {
    cat(sprintf("  Completed %d/%d parameter combinations\n", i, nrow(param_grid)))
  }
} ## close param_grid loop


#%%%%%%%%%%%%
# Hyperparameter Selection
#%%%%%%%%%%%%

## summarize per-fold mean OOB error:

cat("Top 5 models by mean OOB Error:\n")
top_models <- cv_results %>% 
  arrange(mean_oob_error) %>% 
  head(5)
print(top_models %>% 
        mutate(across(where(is.numeric) & !c(mtry, nodesize), ~round(., 4))))

## sort all hyperparameter combinations by mean out of bag error (descending)
## take best one (the first)

best_params <- cv_results %>% 
  arrange(mean_oob_error) %>% 
  slice(1)

cat("\n", rep("-", 70), "\n", sep="")
cat("SELECTED HYPERPARAMETERS (based on mean OOB Error):\n")
cat("  mtry =", best_params$mtry, "\n")
cat("  nodesize =", best_params$nodesize, "\n")
cat("  Mean OOB Error =", round(best_params$mean_oob_error, 4), 
    "(SD =", round(best_params$sd_oob_error, 4), ")\n")
cat("  Mean Balanced Accuracy =", round(best_params$mean_balanced_acc, 4), 
    "(SD =", round(best_params$sd_balanced_acc, 4), ")\n")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FIT FINAL MODEL ON FULL DATASET
## fit final model on all 576 observations
## using the optimal parameter for the sweep
## This is the model for interpretation and results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rownames(merged_x) <- NULL

set.seed(6889)
final_model <- randomForest(
  x = as.matrix(merged_x),
  y = merged_y,
  ntree = 500,
  mtry = best_params$mtry,
  nodesize = best_params$nodesize,
  strata = merged_y,
  sampsize = rep(20, nlevels(merged_y)),
  importance = TRUE,
  proximity = TRUE,
  keep.forest = TRUE
)

cat("Final model trained on all 576 observations\n")
cat("  Trees:", 500, "\n")
cat("  mtry:", best_params$mtry, "\n")
cat("  nodesize:", best_params$nodesize, "\n\n")

#------------------------------------------------------------
# FULL DATASET PERFORMANCE (In-bag predictions)
#------------------------------------------------------------

full_pred <- final_model$predicted
full_cm <- table(Predicted = full_pred, Actual = merged_y)

cat("Confusion Matrix (In-bag predictions):\n")
print(full_cm)

cat("\nPredicted class distribution:\n")
print(table(full_pred))

# Overall metrics
full_acc <- mean(full_pred == merged_y)
full_bal_acc <- mean(diag(full_cm) / colSums(full_cm))

cat("\nOverall Metrics:\n")
cat("  Accuracy:", round(full_acc, 4), "\n")
cat("  Balanced Accuracy:", round(full_bal_acc, 4), "\n")
cat("  OOB Error Rate:", round(final_model$err.rate[500, "OOB"], 4), "\n")

# Per-class metrics
cat("\nPer-Class Metrics:\n")
full_perf <- data.frame()

for (class in levels(merged_y)) {
  tp <- full_cm[class, class]
  fn <- sum(full_cm[, class]) - tp
  fp <- sum(full_cm[class, ]) - tp
  
  n_actual <- tp + fn
  sensitivity <- tp / (tp + fn)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  
  cat(sprintf("  %s (N=%d): Sensitivity=%.3f, Precision=%.3f, Predicted=%d\n", 
              class, n_actual, sensitivity, precision, tp + fp))
  
  full_perf <- rbind(full_perf, data.frame(
    Class = class,
    N = n_actual,
    Sensitivity = sensitivity,
    Precision = precision
  ))
}

cat("\nKey: Model identifies", sum(full_pred == "Ansar al-Shariah"), 
    "Ansar al-Shariah cases out of", sum(merged_y == "Ansar al-Shariah"), "total.\n")

#------------------------------------------------------------
# CROSS-VALIDATED PERFORMANCE SUMMARY
#------------------------------------------------------------
cat("\n", rep("-", 70), "\n", sep="")
cat("CROSS-VALIDATED PERFORMANCE (Out-of-Sample Estimates)\n")
cat("Based on", k_folds, "-fold stratified CV with optimal hyperparameters\n")
cat(rep("-", 70), "\n\n")

## Pull up the cross-validated model
## using param combination that performed best

cat("Best hyperparameters:\n")
cat("  mtry:", best_params$mtry, "\n")
cat("  nodesize:", best_params$nodesize, "\n\n")

cat("Cross-Validated Metrics (Mean ± SD across folds):\n")
cat(sprintf("  Accuracy: %.3f ± %.3f\n", 
            best_params$mean_accuracy, best_params$sd_accuracy))
cat(sprintf("  Balanced Accuracy: %.3f ± %.3f\n", 
            best_params$mean_balanced_acc, best_params$sd_balanced_acc))
cat(sprintf("  OOB Error: %.3f ± %.3f\n", 
            best_params$mean_oob_error, best_params$sd_oob_error))

cat("\nPer-Class Cross-Validated Performance:\n")
cat(sprintf("  AAS Sensitivity: %.3f ± %.3f\n", 
            best_params$mean_aas_sensitivity, best_params$sd_aas_sensitivity))
cat(sprintf("  Houthi Sensitivity: %.3f ± %.3f\n", 
            best_params$mean_houthi_sensitivity, best_params$sd_houthi_sensitivity))
cat(sprintf("  Houthi Sensitivity: %.3f ± %.3f\n", 
            best_params$mean_aqap_sensitivity, best_params$sd_aqap_sensitivity))

#------------------------------------------------------------
# VARIABLE IMPORTANCE
#------------------------------------------------------------
imp <- importance(final_model)
imp_sorted <- sort(imp[, "MeanDecreaseGini"], decreasing = TRUE)[1:20]

cat("Mean Decrease in Gini:\n")
print(round(imp_sorted, 2))

#------------------------------------------------------------
# SUMMARY TABLES FOR MANUSCRIPT
#------------------------------------------------------------

cat("Table for Supplementary Materials: Hyperparameter Tuning Results (Top 5 Models)\n")
cat("Cross-validated performance across", k_folds, "folds\n\n")
print(top_models %>% 
        select(mtry, nodesize, mean_balanced_acc, sd_balanced_acc, 
               mean_aas_sensitivity, sd_aas_sensitivity,
               mean_houthi_sensitivity, sd_houthi_sensitivity) %>%
        mutate(across(where(is.numeric) & !c(mtry, nodesize), ~round(., 3))))

cat("RF Table 1: Final Model Performance Summary\n")
perf_summary <- data.frame(
  Metric = c("In-bag Accuracy", "In-bag Balanced Accuracy", "OOB Error",
             "CV Mean Accuracy", "CV Mean Balanced Accuracy"),
  Value = c(full_acc, full_bal_acc, final_model$err.rate[500, "OOB"],
            best_params$mean_accuracy, best_params$mean_balanced_acc),
  SD = c(NA, NA, NA, 
         best_params$sd_accuracy, best_params$sd_balanced_acc),
  N = c(576, 576, 576, 576, 576)
)
print(perf_summary %>% mutate(across(c(Value, SD), ~round(., 3))))

cat("\nTable 3: Per-Class Performance (Full Dataset)\n")
print(full_perf %>% mutate(across(where(is.numeric) & !N, ~round(., 3))))

cat("\nTable 4: Cross-Validated Per-Class Sensitivity\n")
cv_class_perf <- data.frame(
  Class = c("Ansar al-Shariah", "AQAP", "Houthi/Ansarallah"),
  Mean_Sensitivity = c(best_params$mean_aas_sensitivity, 
                       best_params$mean_aqap_sensitivity,
                       best_params$mean_houthi_sensitivity),
  SD_Sensitivity = c(best_params$sd_aas_sensitivity,
                     best_params$sd_aqap_sensitivity,
                     best_params$sd_houthi_sensitivity)
)
print(cv_class_perf %>% mutate(across(where(is.numeric), ~round(., 3))))


# Save results
results_list <- list(
  cv_results = cv_results,
  best_params = best_params,
  final_model = final_model,
  full_confusion = full_cm,
  full_performance = full_perf,
  variable_importance = imp_sorted,
  k_folds = k_folds
)

cat("\nResults saved to 'results_list' object.\n")
cat("Key components:\n")
cat("  - cv_results: All hyperparameter combinations with CV metrics\n")
cat("  - best_params: Optimal hyperparameters selected\n")
cat("  - final_model: Random forest trained on full dataset\n")
cat("  - full_confusion: Confusion matrix (in-bag predictions)\n")
cat("  - full_performance: Per-class metrics\n")
cat("  - variable_importance: Top 20 important features\n")

## Plots:
## Figure: Full Dataset Confusion Matrix

cm_melted <- melt(full_cm)
colnames(cm_melted) <- c("Predicted", "Actual", "Count")

## Rename to shorten labels:
cm_melted$Actual <- recode(cm_melted$Actual,
                           "Ansar al-Shariah" = "AAS",
                           "AQAP/Al-Qaeda" = "AQAP",
                           "Houthi/Ansarallah" = "Houthi")

cm_melted$Predicted <- recode(cm_melted$Predicted,
                              "Ansar al-Shariah" = "AAS",
                              "AQAP/Al-Qaeda" = "AQAP",
                              "Houthi/Ansarallah" = "Houthi")

colors <- brewer.pal(n = length(unique(cm_melted$Actual)), name = "Set3")

cmp = ggplot(cm_melted, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 6, color = "black") + # Adjusted text size and color
  scale_fill_gradient(low = "white", high = "steelblue", name = "Count") + # Clear color legend
  theme_bw(base_size = 14) + # Consistent font size with the other plot
  labs(
    title = "Confusion Matrix",
    subtitle = "Full Dataset (N = 576)",
    x = "Actual Class", 
    y = "Predicted Class"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Axis labels slightly larger
    axis.text.y = element_text(size = 12), # Axis labels slightly larger
    axis.title = element_text(size = 14), # Title font size adjusted
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Bold title, centered
    plot.subtitle = element_text(size = 14), # Subtitle font size adjusted
    legend.position = "none", # No legend, consistent with the other plot
    panel.grid = element_blank(), # Removed grid lines for cleaner look
  )

cmp
ggsave(cmp, file=file.path(figPath, "confusion_matrix.pdf"))

## Cross-validated sensitivity by class:
## shows that the hard-to-differentiate AAS isn't an artifact of one model

#First, extract per-class sensitivity from each fold
#cv_sensitivity_long <- data.frame(
#  Class = rep(c("AAS", "AQAP", "Houthi"), each = k_folds),
#  Fold = rep(1:k_folds, 3),
##  Sensitivity = c(aas_sens_by_fold, aqap_sens_by_fold, houthi_sens_by_fold)
#)

#ggplot(cv_sensitivity_long, aes(x = Class, y = Sensitivity, fill = Class)) +
#  geom_boxplot(alpha = 0.7) +
#  geom_jitter(width = 0.2, alpha = 0.5) +
#  scale_y_continuous(limits = c(0, 1)) +
#  theme_minimal() +
#  labs(title = "Cross-Validated Sensitivity by Group",
#       subtitle = "5-fold stratified CV",
#       y = "Sensitivity (Recall)") +
#  theme(legend.position = "none")

## feature importance plot:

# Top 20 features
imp_df <- data.frame(
  Feature = names(imp_sorted),
  Importance = imp_sorted
)

ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Most Important Features",
       subtitle = "Mean Decrease in Gini",
       x = NULL, y = "Importance")

## Hyperparameter tuning:
## how performance varies across hyperparameters
ggplot(cv_results, aes(x = mtry, y = mean_balanced_acc, color = as.factor(nodesize))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_balanced_acc - sd_balanced_acc,
                    ymax = mean_balanced_acc + sd_balanced_acc),
                width = 0.5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Hyperparameter Tuning Results",
       subtitle = "Cross-validated balanced accuracy",
       x = "mtry (features per split)",
       y = "Balanced Accuracy (mean ± SD)",
       color = "Node Size") +
  scale_color_brewer(palette = "Set1")

## MDS/PCA proximity matrix 

## MDS/PCA Proximity Matrix 
# Where to AQAP and AAS cluster in feature space 

# Extract proximity matrix from final model
prox <- final_model$proximity

# MDS
mds <- cmdscale(1 - prox, k = 2)
mds_df <- data.frame(
  MDS1 = mds[, 1],
  MDS2 = mds[, 2],
  Group = merged_y
)

# Plot MDS
mds = ggplot(mds_df, aes(x = MDS1, y = MDS2, color = Group)) + 
  geom_point(alpha = 0.6, size = 2) + 
  stat_ellipse(level = 0.68) +  # 1 SD ellipse
  scale_color_brewer(palette = "Set1") +  # Consistent color palette
  theme_bw(base_size = 14) +  # Base font size to match other plots
  labs(
    title = "Story Clustering in Feature Space",
    #subtitle = "",
    x = "Dimension 1",
    y = "Dimension 2"
  ) + 
  theme(
    axis.text.x = element_text(size = 12),  # Axis text size
    axis.text.y = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),   # Axis title font size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title style
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # Subtitle style
    legend.position = "none",  # Remove legend (consistent with other plots)
    panel.grid = element_blank()  # Remove grid lines for cleaner look
  )

mds

ggsave(mds, file = file.path(figPath, "mds.pdf"))

## OOB Error Rate
# Diagnostic for model stability
oob_df <- data.frame(
  Trees = 1:500,
  OOB_Error = final_model$err.rate[, "OOB"],
  AAS_Error = final_model$err.rate[, "Ansar al-Shariah"],
  AQAP_Error = final_model$err.rate[, "AQAP/Al-Qaeda"],
  Houthi_Error = final_model$err.rate[, "Houthi/Ansarallah"]
)

oob_long <- melt(oob_df, id.vars = "Trees")

# Plot OOB error rate
oob_error = ggplot(oob_long, aes(x = Trees, y = value, color = variable)) + 
  geom_line(alpha = 0.7) + 
  theme_minimal() + 
  labs(
    title = "OOB Error Rate by Number of Trees",
    y = "Error Rate",
    color = "Class"
  ) + 
  scale_color_brewer(palette = "Set1")

oob_error

ggsave(oob_error,
       file = file.path(figPath, "oob_error_trees.pdf"))
