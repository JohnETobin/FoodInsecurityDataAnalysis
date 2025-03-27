# 02_exploratory_analysis.R
# Exploratory analysis of food access and health in North Carolina

library(here)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# Load processed data
urban_data <- readRDS(here("data", "processed", "urban_data.rds"))

# Verify data is loaded correctly
message("Data dimensions: ", nrow(urban_data), " rows x ", ncol(urban_data), " columns")
message("Food desert classification counts:")
print(table(urban_data$food_desert))
message("Health data summary:")
print(summary(urban_data[, c("obesity", "diabetes")]))

# 1. SUMMARY STATISTICS BY FOOD DESERT STATUS
desert_summary <- urban_data %>%
  group_by(food_desert) %>%
  summarize(
    n_tracts = n(),
    avg_obesity = mean(obesity, na.rm = TRUE),
    avg_diabetes = mean(diabetes, na.rm = TRUE),
    avg_poverty = mean(PovertyRate, na.rm = TRUE),
    avg_income = mean(MedianFamilyIncome, na.rm = TRUE)
  )

# Print summary table
print(desert_summary)

# 2. HEALTH OUTCOMES BY FOOD DESERT STATUS

# Obesity boxplot
obesity_boxplot <- ggplot(urban_data, aes(x = food_desert, y = obesity)) +
  geom_boxplot(fill = c("#9CCC65", "#EF5350"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Obesity Rates by Food Desert Status in Urban NC",
       subtitle = paste("Sample size:", nrow(urban_data), "census tracts"),
       x = "",
       y = "Obesity Prevalence (%)") +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Save the obesity boxplot
ggsave(here("output", "figures", "obesity_by_food_desert.png"), 
       obesity_boxplot, width = 8, height = 6, dpi = 300)

# Diabetes boxplot
diabetes_boxplot <- ggplot(urban_data, aes(x = food_desert, y = diabetes)) +
  geom_boxplot(fill = c("#9CCC65", "#EF5350"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Diabetes Rates by Food Desert Status in Urban NC",
       subtitle = paste("Sample size:", nrow(urban_data), "census tracts"),
       x = "",
       y = "Diabetes Prevalence (%)") +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Save the diabetes boxplot
ggsave(here("output", "figures", "diabetes_by_food_desert.png"), 
       diabetes_boxplot, width = 8, height = 6, dpi = 300)

# 3. CORRELATION ANALYSIS

# Select key variables for correlation analysis (excluding SwampRatio)
key_vars <- urban_data %>%
  select(
    obesity, diabetes,
    PovertyRate, MedianFamilyIncome,
    LILATracts_1And10, LATracts_half, LATracts1
  ) %>%
  drop_na()

# Calculate correlation matrix
correlation_matrix <- cor(key_vars)

# Create correlation plot
png(here("output", "figures", "correlation_matrix.png"), width = 800, height = 800, res = 100)
corrplot(correlation_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, tl.cex = 0.7,
         title = "Correlation Matrix of Key Variables",
         mar = c(0, 0, 1, 0))
dev.off()

# 4. RELATIONSHIP BETWEEN SOCIOECONOMIC FACTORS AND HEALTH

# Poverty vs. obesity by food desert status
poverty_obesity_plot <- ggplot(
  # Filter out NA values in relevant columns
  urban_data %>% filter(!is.na(food_desert) & !is.na(PovertyRate) & !is.na(obesity)),
  aes(x = PovertyRate, y = obesity, color = food_desert)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  geom_smooth(
    data = urban_data %>% filter(!is.na(PovertyRate) & !is.na(obesity)), 
    aes(x = PovertyRate, y = obesity), 
    method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828"),
    # Explicitly define which values to include in the legend
    breaks = c("Not Food Desert", "Food Desert")) +
  labs(title = "Relationship Between Poverty and Obesity",
       subtitle = "By Food Desert Status in Urban North Carolina",
       x = "Poverty Rate (%)",
       y = "Obesity Prevalence (%)",
       color = "Food Desert Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the poverty-obesity plot
ggsave(here("output", "figures", "poverty_obesity_relationship.png"), 
       poverty_obesity_plot, width = 10, height = 6, dpi = 300)

# Income vs. obesity by food desert status
income_obesity_plot <- ggplot(urban_data, aes(x = MedianFamilyIncome, y = obesity, color = food_desert)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  geom_smooth(data = urban_data, aes(x = MedianFamilyIncome, y = obesity), 
              method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828")) +
  labs(title = "Relationship Between Median Family Income and Obesity",
       subtitle = "By Food Desert Status in Urban North Carolina",
       x = "Median Family Income ($)",
       y = "Obesity Prevalence (%)",
       color = "Food Desert Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the income-obesity plot
ggsave(here("output", "figures", "income_obesity_relationship.png"), 
       income_obesity_plot, width = 10, height = 6, dpi = 300)

# 5. STATISTICAL TESTS

# T-test for obesity difference by food desert status
obesity_ttest <- t.test(obesity ~ food_desert, data = urban_data)
print("T-test for obesity by food desert status:")
print(obesity_ttest)

# T-test for diabetes difference by food desert status
diabetes_ttest <- t.test(diabetes ~ food_desert, data = urban_data)
print("T-test for diabetes by food desert status:")
print(diabetes_ttest)

# 6. SAVE OUTPUT TABLES

# Health metrics comparison by food desert status
health_comparison <- urban_data %>%
  group_by(food_desert) %>%
  summarize(
    mean_obesity = mean(obesity, na.rm = TRUE),
    sd_obesity = sd(obesity, na.rm = TRUE),
    mean_diabetes = mean(diabetes, na.rm = TRUE),
    sd_diabetes = sd(diabetes, na.rm = TRUE),
    count = n()
  )

# Save summary tables to output directory
write.csv(desert_summary, here("output", "tables", "food_desert_summary.csv"), row.names = FALSE)
write.csv(health_comparison, here("output", "tables", "health_comparison.csv"), row.names = FALSE)
write.csv(correlation_matrix, here("output", "tables", "correlation_matrix.csv"), row.names = TRUE)

# Save t-test results
sink(here("output", "tables", "ttest_results.txt"))
cat("T-test for obesity by food desert status:\n")
print(obesity_ttest)
cat("\nT-test for diabetes by food desert status:\n")
print(diabetes_ttest)
sink()

# 7. ADDITIONAL VISUALIZATIONS

# Create violin plots for health outcomes
health_violin <- urban_data %>%
  pivot_longer(cols = c(obesity, diabetes), 
               names_to = "health_outcome", 
               values_to = "prevalence") %>%
  mutate(health_outcome = factor(health_outcome, 
                                 levels = c("obesity", "diabetes"),
                                 labels = c("Obesity", "Diabetes")))

violin_plot <- ggplot(health_violin, aes(x = food_desert, y = prevalence, fill = food_desert)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  facet_wrap(~health_outcome, scales = "free_y") +
  scale_fill_manual(values = c("Not Food Desert" = "#2E7D32", "Food Desert" = "#C62828")) +
  labs(title = "Distribution of Health Outcomes by Food Desert Status",
       x = "",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the violin plot
ggsave(here("output", "figures", "health_outcomes_violins.png"), 
       violin_plot, width = 12, height = 6, dpi = 300)

message("Exploratory analysis complete.")