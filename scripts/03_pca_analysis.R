# 03_pca_analysis.R
# Principal Component Analysis of food access and health data

library(here)
library(dplyr)
library(ggplot2)
library(factoextra)  # For PCA visualization
library(tidyr)

# Load processed data
urban_data <- readRDS(here("data", "processed", "urban_data.rds"))

# 1. SELECT VARIABLES FOR PCA

# Food access variables
access_vars <- urban_data %>%
  select(
    # Low income and low access flags
    LILATracts_1And10, LILATracts_halfAnd10, LILATracts_1And20,
    # Low access flags
    LATracts_half, LATracts1, LATracts10, LATracts20,
    # Access share variables (select a subset to avoid too many variables)
    lapophalfshare, lapop1share, lalowihalfshare, lalowi1share, 
    lakidshalfshare, lakids1share, laseniorshalfshare, laseniors1share
  )

# Check for missing values in PCA variables
missing_counts <- colSums(is.na(access_vars))
print("Missing value counts in access variables:")
print(missing_counts)

# Handle any missing values
if(any(missing_counts > 0)) {
  message("Imputing missing values with column means...")
  for(col in names(access_vars)) {
    if(missing_counts[col] > 0) {
      access_vars[[col]][is.na(access_vars[[col]])] <- mean(access_vars[[col]], na.rm = TRUE)
    }
  }
}

# Check for zero/constant variance columns
var_check <- apply(access_vars, 2, var, na.rm = TRUE)
print("Variance of each column:")
print(var_check)

# Identify columns with very low or zero variance
zero_var_cols <- names(which(var_check < 1e-8))
if(length(zero_var_cols) > 0) {
  message("Removing zero variance columns: ", paste(zero_var_cols, collapse = ", "))
  access_vars <- access_vars %>% select(-all_of(zero_var_cols))
}

# Also check for highly correlated variables (> 0.9) that could cause issues
cor_matrix <- cor(access_vars, use = "pairwise.complete.obs")
high_cor <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  message("Warning: Highly correlated variables detected. Consider removing some.")
  print(high_cor)
}

# 2. PERFORM PCA

# Double check we have variables left
if(ncol(access_vars) < 2) {
  stop("Not enough variables for PCA after removing zero variance columns")
}

# Scale the data (important for PCA)
pca_result <- prcomp(access_vars, scale = TRUE)

# Summarize the PCA results
pca_summary <- summary(pca_result)
print(pca_summary)

# Look at loadings (contributions of each variable to the principal components)
print("Principal component loadings (first 3 components):")
print(pca_result$rotation[, 1:3])

# 3. VISUALIZE PCA RESULTS

# Scree plot - to determine how many components to keep
scree_plot <- fviz_eig(pca_result, 
                       addlabels = TRUE, 
                       barfill = "#4285F4",
                       barcolor = "#4285F4",
                       linecolor = "#EA4335",
                       title = "Scree Plot: Variance Explained by Principal Components") +
  theme_minimal()

# Save the scree plot
ggsave(here("output", "figures", "pca_scree_plot.png"), 
       scree_plot, width = 10, height = 6, dpi = 300)

# Variables factor map (shows how variables relate to principal components)
var_plot <- fviz_pca_var(pca_result,
                         col.var = "contrib", # Color by contribution
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,        # Avoid text overlap
                         title = "Variables Factor Map") +
  theme_minimal()

# Save the variables factor map
ggsave(here("output", "figures", "pca_variables_map.png"), 
       var_plot, width = 10, height = 8, dpi = 300)

# 4. INTERPRET THE PRINCIPAL COMPONENTS

# Based on loadings, name the first few principal components
# Looking at the actual loadings in the output above, assign appropriate names
num_components <- min(5, ncol(pca_result$rotation))
component_names <- rep("Component", num_components)

# Default generic names that should be updated after examining loadings
component_names[1] <- "Overall Access Limitation"
if(num_components >= 2) component_names[2] <- "Urban-Rural Access Differential"
if(num_components >= 3) component_names[3] <- "Vulnerable Population Access"
if(num_components >= 4) component_names[4] <- "Low-Income Access Patterns"
if(num_components >= 5) component_names[5] <- "Child vs. Senior Access"

# Create a table of variable contributions to each component
contributions <- as.data.frame(pca_result$rotation[, 1:min(5, ncol(pca_result$rotation))])
contributions$Variable <- rownames(contributions)
contributions <- contributions %>%
  select(Variable, everything())

# Save contributions to CSV
write.csv(contributions, here("output", "tables", "pca_variable_contributions.csv"), row.names = FALSE)

# 5. ADD PCA SCORES TO ORIGINAL DATA

# Add the principal components to the data
num_pcs_to_keep <- min(5, ncol(pca_result$x))
pca_scores <- as.data.frame(pca_result$x[, 1:num_pcs_to_keep])
colnames(pca_scores) <- paste0("PC", 1:num_pcs_to_keep)

# Add descriptive names for the components
for(i in 1:num_pcs_to_keep) {
  pca_scores[[paste0("PC", i, "_Description")]] <- component_names[i]
}

# Combine with original data
urban_data_pca <- bind_cols(urban_data, pca_scores)

# 6. ANALYZE HEALTH OUTCOMES BY PCA COMPONENTS

# Correlation between principal components and health outcomes
pc_vars <- paste0("PC", 1:num_pcs_to_keep)
pc_health_corr <- cor(urban_data_pca[, c(pc_vars, "obesity", "diabetes")], 
                      use = "pairwise.complete.obs")

print("Correlation between PCs and health outcomes:")
print(pc_health_corr)

# Save correlation to CSV
write.csv(pc_health_corr, here("output", "tables", "pc_health_correlation.csv"), row.names = TRUE)

# 7. VISUALIZE RELATIONSHIPS BETWEEN PCS AND HEALTH

# PC1 vs. obesity with food desert overlay
pc1_obesity_plot <- ggplot(urban_data_pca, aes(x = PC1, y = obesity, color = food_desert)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828")) +
  labs(title = "Relationship Between PC1 and Obesity Rates",
       subtitle = paste0("PC1: ", component_names[1]),
       x = "PC1 Score",
       y = "Obesity Prevalence (%)") +
  theme_minimal()

# Save the PC1 vs. obesity plot
ggsave(here("output", "figures", "pc1_obesity_relationship.png"), 
       pc1_obesity_plot, width = 10, height = 6, dpi = 300)

# Check if we have PC2 before creating that plot
if(num_pcs_to_keep >= 2) {
  # PC2 vs. obesity with food desert overlay
  pc2_obesity_plot <- ggplot(urban_data_pca, aes(x = PC2, y = obesity, color = food_desert)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    scale_color_manual(values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828")) +
    labs(title = "Relationship Between PC2 and Obesity Rates",
         subtitle = paste0("PC2: ", component_names[2]),
         x = "PC2 Score",
         y = "Obesity Prevalence (%)") +
    theme_minimal()
  
  # Save the PC2 vs. obesity plot
  ggsave(here("output", "figures", "pc2_obesity_relationship.png"), 
         pc2_obesity_plot, width = 10, height = 6, dpi = 300)
}

# 8. PCA BIPLOT WITH HEALTH OUTCOME OVERLAY

# Check if we have at least 2 PCs for biplot
if(num_pcs_to_keep >= 2) {
  # Create a data frame with PC scores and additional information
  biplot_data <- urban_data_pca %>%
    select(PC1, PC2, obesity, diabetes, food_desert) %>%
    mutate(obesity_group = cut(obesity, 
                               breaks = quantile(obesity, probs = seq(0, 1, 0.25), na.rm = TRUE),
                               labels = c("Low", "Medium-Low", "Medium-High", "High")))
  
  # PC1 vs PC2 biplot with obesity color coding
  biplot_obesity <- ggplot(biplot_data, aes(x = PC1, y = PC2, color = obesity_group)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = c("Low" = "#4285F4", "Medium-Low" = "#34A853", 
                                  "Medium-High" = "#FBBC05", "High" = "#EA4335")) +
    labs(title = "PCA Biplot with Obesity Rates",
         subtitle = paste0("PC1: ", component_names[1], " | PC2: ", component_names[2]),
         x = "PC1",
         y = "PC2",
         color = "Obesity Level") +
    theme_minimal()
  
  # Save the biplot
  ggsave(here("output", "figures", "pca_biplot_obesity.png"), 
         biplot_obesity, width = 10, height = 8, dpi = 300)
  
  # PC1 vs PC2 biplot with food desert overlay
  biplot_fooddesert <- ggplot(biplot_data, aes(x = PC1, y = PC2, color = food_desert)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828")) +
    labs(title = "PCA Biplot with Food Desert Classification",
         subtitle = paste0("PC1: ", component_names[1], " | PC2: ", component_names[2]),
         x = "PC1",
         y = "PC2",
         color = "Food Desert Status") +
    theme_minimal()
  
  # Save the food desert biplot
  ggsave(here("output", "figures", "pca_biplot_fooddesert.png"), 
         biplot_fooddesert, width = 10, height = 8, dpi = 300)
}

# 9. REGRESSION WITH PRINCIPAL COMPONENTS

# Build the formula based on number of available PCs
pc_formula_obesity <- as.formula(paste("obesity ~", paste(paste0("PC", 1:num_pcs_to_keep), collapse = " + ")))
pc_formula_diabetes <- as.formula(paste("diabetes ~", paste(paste0("PC", 1:num_pcs_to_keep), collapse = " + ")))

# Regression of obesity on principal components
obesity_pc_model <- lm(pc_formula_obesity, data = urban_data_pca)
summary_obesity_pc <- summary(obesity_pc_model)
print("Regression of obesity on principal components:")
print(summary_obesity_pc)

# Regression of diabetes on principal components
diabetes_pc_model <- lm(pc_formula_diabetes, data = urban_data_pca)
summary_diabetes_pc <- summary(diabetes_pc_model)
print("Regression of diabetes on principal components:")
print(summary_diabetes_pc)

# Save regression results
sink(here("output", "tables", "pc_regression_results.txt"))
cat("Regression of obesity on principal components:\n")
print(summary_obesity_pc)
cat("\nRegression of diabetes on principal components:\n")
print(summary_diabetes_pc)
sink()

# 10. SAVE PCA-ENHANCED DATASET

# Save the dataset with PCA components for use in later analysis
saveRDS(urban_data_pca, here("data", "processed", "urban_data_pca.rds"))

message("PCA analysis complete.")