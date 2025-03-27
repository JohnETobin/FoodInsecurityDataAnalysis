# 04_regression_analysis.R
# Regression analysis of food access, PCA components, and health outcomes

library(here)
library(dplyr)
library(ggplot2)
library(broom)  # For tidying model results
library(car)    # For VIF calculations
library(cowplot) # For arranging multiple plots

# Load PCA-enhanced data
urban_data_pca <- readRDS(here("data", "processed", "urban_data_pca.rds"))

# 1. BASIC MODELS FOR OBESITY AND DIABETES

# Model 1: Obesity ~ Food Desert Status
model1_obesity <- lm(obesity ~ food_desert, data = urban_data_pca)
summary_model1_obesity <- summary(model1_obesity)
print("Model 1: Obesity ~ Food Desert Status")
print(summary_model1_obesity)

# Model 1: Diabetes ~ Food Desert Status
model1_diabetes <- lm(diabetes ~ food_desert, data = urban_data_pca)
summary_model1_diabetes <- summary(model1_diabetes)
print("Model 1: Diabetes ~ Food Desert Status")
print(summary_model1_diabetes)

# 2. MODELS WITH SOCIOECONOMIC CONTROLS

# Model 2: Obesity ~ Food Desert + Socioeconomic Factors
model2_obesity <- lm(obesity ~ food_desert + PovertyRate + MedianFamilyIncome, data = urban_data_pca)
summary_model2_obesity <- summary(model2_obesity)
print("Model 2: Obesity ~ Food Desert + Socioeconomic Factors")
print(summary_model2_obesity)

# Model 2: Diabetes ~ Food Desert + Socioeconomic Factors
model2_diabetes <- lm(diabetes ~ food_desert + PovertyRate + MedianFamilyIncome, data = urban_data_pca)
summary_model2_diabetes <- summary(model2_diabetes)
print("Model 2: Diabetes ~ Food Desert + Socioeconomic Factors")
print(summary_model2_diabetes)

# 3. MODELS WITH PCA COMPONENTS

# Model 3: Obesity ~ PCA Components
model3_obesity <- lm(obesity ~ PC1 + PC2 + PC3 + PC4 + PC5, data = urban_data_pca)
summary_model3_obesity <- summary(model3_obesity)
print("Model 3: Obesity ~ PCA Components")
print(summary_model3_obesity)

# Model 3: Diabetes ~ PCA Components
model3_diabetes <- lm(diabetes ~ PC1 + PC2 + PC3 + PC4 + PC5, data = urban_data_pca)
summary_model3_diabetes <- summary(model3_diabetes)
print("Model 3: Diabetes ~ PCA Components")
print(summary_model3_diabetes)

# 4. FULL MODELS WITH FOOD DESERT STATUS, SOCIOECONOMIC FACTORS AND PCA COMPONENTS

# Model 4: Obesity ~ Food Desert + Socioeconomic Factors + PCA Components
model4_obesity <- lm(obesity ~ food_desert + PovertyRate + MedianFamilyIncome + 
                       PC1 + PC2 + PC3 + PC4 + PC5, data = urban_data_pca)
summary_model4_obesity <- summary(model4_obesity)
print("Model 4: Obesity ~ Food Desert + Socioeconomic Factors + PCA Components")
print(summary_model4_obesity)

# Check for multicollinearity in the full model
vif_obesity <- car::vif(model4_obesity)
print("Variance Inflation Factors (VIF) for full obesity model:")
print(vif_obesity)

# Model 4: Diabetes ~ Food Desert + Socioeconomic Factors + PCA Components
model4_diabetes <- lm(diabetes ~ food_desert + PovertyRate + MedianFamilyIncome + 
                        PC1 + PC2 + PC3 + PC4 + PC5, data = urban_data_pca)
summary_model4_diabetes <- summary(model4_diabetes)
print("Model 4: Diabetes ~ Food Desert + Socioeconomic Factors + PCA Components")
print(summary_model4_diabetes)

# Check for multicollinearity in the full model
vif_diabetes <- car::vif(model4_diabetes)
print("Variance Inflation Factors (VIF) for full diabetes model:")
print(vif_diabetes)

# 5. MODEL COMPARISON

# Prepare a table comparing models for obesity
obesity_models <- list(model1_obesity, model2_obesity, model3_obesity, model4_obesity)
obesity_summaries <- lapply(obesity_models, summary)
obesity_comparison <- data.frame(
  Model = c("Model 1: Food Desert Only", 
            "Model 2: + Socioeconomic", 
            "Model 3: PCA Components",
            "Model 4: Full Model"),
  R_squared = sapply(obesity_summaries, function(x) round(x$r.squared, 3)),
  Adj_R_squared = sapply(obesity_summaries, function(x) round(x$adj.r.squared, 3)),
  F_statistic = sapply(obesity_summaries, function(x) round(x$fstatistic[1], 2)),
  p_value = sapply(obesity_summaries, function(x) ifelse(x$fstatistic[1] > 0, 
                                                         "< 2.2e-16", NA))
)

print("Obesity Model Comparison:")
print(obesity_comparison)

# Prepare a table comparing models for diabetes
diabetes_models <- list(model1_diabetes, model2_diabetes, model3_diabetes, model4_diabetes)
diabetes_summaries <- lapply(diabetes_models, summary)
diabetes_comparison <- data.frame(
  Model = c("Model 1: Food Desert Only", 
            "Model 2: + Socioeconomic", 
            "Model 3: PCA Components",
            "Model 4: Full Model"),
  R_squared = sapply(diabetes_summaries, function(x) round(x$r.squared, 3)),
  Adj_R_squared = sapply(diabetes_summaries, function(x) round(x$adj.r.squared, 3)),
  F_statistic = sapply(diabetes_summaries, function(x) round(x$fstatistic[1], 2)),
  p_value = sapply(diabetes_summaries, function(x) ifelse(x$fstatistic[1] > 0, 
                                                          "< 2.2e-16", NA))
)

print("Diabetes Model Comparison:")
print(diabetes_comparison)

# 6. VISUALIZE MODELS

# Function to plot coefficients from a model
plot_coefficients <- function(model, title, y_lab = "Coefficient") {
  model_df <- broom::tidy(model) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = gsub("food_desert", "Food Desert: ", term)) %>%
    mutate(
      significant = ifelse(p.value < 0.05, "Yes", "No"),
      term = reorder(term, estimate)
    )
  
  ggplot(model_df, aes(x = term, y = estimate, fill = significant)) +
    geom_col() +
    geom_errorbar(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error), 
                  width = 0.2) +
    scale_fill_manual(values = c("Yes" = "#4285F4", "No" = "#CCCCCC")) +
    coord_flip() +
    labs(
      title = title,
      x = "",
      y = y_lab,
      fill = "Significant (p<0.05)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold")
    )
}

# Plot coefficients for the full obesity model
obesity_coef_plot <- plot_coefficients(
  model4_obesity, 
  "Factors Influencing Obesity Rates",
  "Effect on Obesity Rate (%)"
)

# Plot coefficients for the full diabetes model
diabetes_coef_plot <- plot_coefficients(
  model4_diabetes, 
  "Factors Influencing Diabetes Rates",
  "Effect on Diabetes Rate (%)"
)

# Combined plot 
combined_coef_plot <- plot_grid(
  obesity_coef_plot, diabetes_coef_plot,
  labels = c("A", "B"),
  ncol = 2
)

# Save plots
ggsave(here("output", "figures", "obesity_coefficients.png"), 
       obesity_coef_plot, width = 8, height = 6, dpi = 300)
ggsave(here("output", "figures", "diabetes_coefficients.png"), 
       diabetes_coef_plot, width = 8, height = 6, dpi = 300)
ggsave(here("output", "figures", "combined_coefficients.png"), 
       combined_coef_plot, width = 14, height = 6, dpi = 300)

# 7. ADDITIONAL ANALYSIS: INTERACTION BETWEEN FOOD DESERT AND POVERTY

# Model with interaction
interaction_obesity <- lm(obesity ~ food_desert * PovertyRate, data = urban_data_pca)
summary_interaction_obesity <- summary(interaction_obesity)
print("Interaction Model: Obesity ~ Food Desert * Poverty Rate")
print(summary_interaction_obesity)

# Create interaction plot for obesity
urban_data_pca$poverty_group <- cut(urban_data_pca$PovertyRate, 
                                    breaks = c(0, 10, 20, 30, 100),
                                    labels = c("< 10%", "10-20%", "20-30%", "> 30%"))

interaction_plot <- ggplot(
  # Filter out NA values in relevant columns
  urban_data_pca %>% filter(!is.na(food_desert) & !is.na(poverty_group) & !is.na(obesity)),
  aes(x = poverty_group, y = obesity, fill = food_desert)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828"),
    # Explicitly define which values to include in the legend
    breaks = c("Not Food Desert", "Food Desert")) +
  labs(
    title = "Interaction Between Poverty Rate and Food Desert Status",
    x = "Poverty Rate",
    y = "Obesity Rate (%)",
    fill = "Food Desert Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save interaction plot
ggsave(here("output", "figures", "poverty_fooddesert_interaction.png"), 
       interaction_plot, width = 10, height = 6, dpi = 300)

# 8. SAVE REGRESSION RESULTS

# Save model comparison tables
write.csv(obesity_comparison, here("output", "tables", "obesity_model_comparison.csv"), row.names = FALSE)
write.csv(diabetes_comparison, here("output", "tables", "diabetes_model_comparison.csv"), row.names = FALSE)

# Save full model coefficients
write.csv(broom::tidy(model4_obesity), here("output", "tables", "obesity_full_model_coefficients.csv"), row.names = FALSE)
write.csv(broom::tidy(model4_diabetes), here("output", "tables", "diabetes_full_model_coefficients.csv"), row.names = FALSE)

# Save detailed regression results
sink(here("output", "tables", "regression_results_summary.txt"))
cat("--- OBESITY MODELS ---\n\n")
cat("Model 1: Obesity ~ Food Desert Status\n")
print(summary_model1_obesity)
cat("\nModel 2: Obesity ~ Food Desert + Socioeconomic Factors\n")
print(summary_model2_obesity)
cat("\nModel 3: Obesity ~ PCA Components\n")
print(summary_model3_obesity)
cat("\nModel 4: Obesity ~ Food Desert + Socioeconomic Factors + PCA Components\n")
print(summary_model4_obesity)
cat("\n\n--- DIABETES MODELS ---\n\n")
cat("Model 1: Diabetes ~ Food Desert Status\n")
print(summary_model1_diabetes)
cat("\nModel 2: Diabetes ~ Food Desert + Socioeconomic Factors\n")
print(summary_model2_diabetes)
cat("\nModel 3: Diabetes ~ PCA Components\n")
print(summary_model3_diabetes)
cat("\nModel 4: Diabetes ~ Food Desert + Socioeconomic Factors + PCA Components\n")
print(summary_model4_diabetes)
sink()

message("Regression analysis complete.")
