# 01_data_preparation.R
# Data loading and integration with tract ID fix

library(here)
library(dplyr)
library(tidyr)
library(sf)
library(readr)

# ========================= #
# LOAD AND PREPARE DATASETS #
# ========================= #

# 1. LOAD FOOD ACCESS DATA
message("Loading food access data...")
food_access <- read_csv(here("data", "raw", "Food Access Research Atlas.csv"),
                        guess_max = 10000)

# Filter for North Carolina
nc_food_access <- food_access %>% filter(State == "North Carolina")
message("NC food access rows: ", nrow(nc_food_access))

# Convert appropriate columns to numeric
# Food access data stores tract IDs as numbers - leave as is
numeric_columns <- c("PovertyRate", "MedianFamilyIncome")
for(col in numeric_columns) {
  if(col %in% names(nc_food_access) && is.character(nc_food_access[[col]])) {
    nc_food_access[[col]] <- as.numeric(gsub("[^0-9\\.]", "", nc_food_access[[col]]))
  }
}

# 2. LOAD CENSUS TRACT SHAPEFILE
message("Loading census tract shapefile...")
nc_tracts <- st_read(here("data", "raw", "shapefiles", "tracts", "tl_2019_37_tract.shp"))
nc_tracts <- st_transform(nc_tracts, crs = 4326)

# 3. LOAD HEALTH DATA
message("Loading health data...")
health_data <- read_csv(here("data", "raw", "500_Cities__Census_Tract-level_Data.csv"),
                        guess_max = 10000)

# Filter for North Carolina
nc_health <- health_data %>% filter(StateAbbr == "NC")
message("NC health data rows: ", nrow(nc_health))

# Find obesity and diabetes variables
health_cols <- colnames(nc_health)
obesity_var <- grep("OBESITY.*CrudePrev", health_cols, value = TRUE)[1]
diabetes_var <- grep("DIABETES.*CrudePrev", health_cols, value = TRUE)[1]
message("Using health variables: ", obesity_var, ", ", diabetes_var)

# 4. LOAD FOOD ENVIRONMENT DATA
message("Loading food environment data...")
food_environment <- read_csv(here("data", "raw", "StateAndCountyData.csv"),
                             guess_max = 10000)

# Filter for North Carolina
nc_food_environment <- food_environment %>% filter(State == "NC")

# 5. LOAD COUNTY SHAPEFILE
message("Loading county shapefile...")
nc_counties <- st_read(here("data", "raw", "shapefiles", "counties", "tl_2019_us_county.shp")) %>% 
  filter(STATEFP == 37)
nc_counties <- st_transform(nc_counties, crs = 4326)

# ========================= #
# FIX TRACT ID FORMATS      #
# ========================= #

# THE KEY FIX: We need to make the tract IDs in both datasets match
# Health data format: "37021000100" (37 = state, 021 = county, 000100 = tract)
# Food access format: 37001020100 (37 = state, 001 = county, 020100 = tract)

message("Preparing tract IDs for merging...")

# Examine a few examples from both datasets for reference
message("Health data tract examples:")
print(head(nc_health$TractFIPS))
message("Food access tract examples:")
print(head(nc_food_access$CensusTract))

# APPROACH: Convert both to string format with the full FIPS code for exact matching

# Convert health data tract IDs
nc_health <- nc_health %>%
  mutate(
    tract_fips_full = TractFIPS,
    # Create a backup numeric version just in case, removing the 37 prefix
    tract_numeric = as.numeric(gsub("^37", "", TractFIPS))
  )

# Convert food access tract IDs to string format for consistent matching
nc_food_access <- nc_food_access %>%
  mutate(
    # Ensure tract IDs are in proper string format with leading zeros preserved
    tract_fips_full = sprintf("%011s", as.character(CensusTract))
  )

# Check overlap using the new ID formats
health_tracts <- nc_health$tract_fips_full
food_tracts <- nc_food_access$tract_fips_full

message("Health tracts count: ", length(health_tracts))
message("Food tracts count: ", length(food_tracts))
message("Tracts in both datasets: ", sum(health_tracts %in% food_tracts))
message("Percentage overlap: ", round(sum(health_tracts %in% food_tracts) / length(health_tracts) * 100, 1), "%")

# Try matching using Census tract numbers (without state prefix) as backup
health_numeric <- nc_health$tract_numeric
food_numeric <- as.numeric(gsub("^37", "", nc_food_access$tract_fips_full))

message("Using numeric IDs - tracts in both datasets: ", sum(health_numeric %in% food_numeric))

# ========================= #
# CREATE PROCESSED DATASETS #
# ========================= #

# 1. PREPARE HEATH DATA SUBSET
health_subset <- nc_health %>%
  select(tract_fips_full, tract_numeric, !!sym(obesity_var), !!sym(diabetes_var)) %>%
  rename(
    obesity = !!sym(obesity_var),
    diabetes = !!sym(diabetes_var)
  )

# 2. PREPARE FOOD ENVIRONMENT DATA
wide_food_environment <- nc_food_environment %>%
  pivot_wider(
    id_cols = County,
    names_from = Variable_Code,
    values_from = Value
  )

# Calculate food swamp ratio
parsed_food_environment <- wide_food_environment %>%
  select(County, GROC16, CONVS16, FFR16, FMRKT18, SUPERC16) %>%
  mutate(across(c(GROC16, CONVS16, FFR16, FMRKT18, SUPERC16), ~as.numeric(as.character(.)))) %>%
  mutate(
    SwampRatio = (CONVS16 + FFR16) / pmax(0.001, (GROC16 + FMRKT18 + SUPERC16)),
    Swamp = ifelse(SwampRatio > 3.89, 1, 0)
  )

# 3. MERGE WITH SHAPEFILE
nc_map_data <- merge(nc_tracts, nc_food_access, 
                     by.x = "GEOID", by.y = "tract_fips_full", 
                     all.x = FALSE)  # Only keep matched tracts

# Define food desert classification
nc_map_data$food_desert <- factor(
  ifelse(nc_map_data$LILATracts_1And10 == 1, "Food Desert", "Not Food Desert"),
  levels = c("Not Food Desert", "Food Desert")
)

# 4. TRY BOTH JOINING APPROACHES TO GET HEALTH DATA
# First approach: Join using the full FIPS code string
message("Joining health data using full FIPS codes...")
nc_health_map_1 <- merge(nc_map_data, health_subset,
                         by.x = "GEOID", by.y = "tract_fips_full",
                         all.x = TRUE)  # Keep all census tracts

# Second approach: Join using the numeric tract ID 
message("Joining health data using numeric IDs...")
nc_health_map_2 <- merge(nc_map_data, health_subset,
                         by.x = "GEOID", by.y = "tract_numeric",
                         all.x = TRUE)  # Keep all census tracts

# Check which approach worked better
health_count_1 <- sum(!is.na(nc_health_map_1$obesity))
health_count_2 <- sum(!is.na(nc_health_map_2$obesity))

message("Health data rows with approach 1: ", health_count_1)
message("Health data rows with approach 2: ", health_count_2)

# Select the better approach
nc_health_map <- if(health_count_1 >= health_count_2) nc_health_map_1 else nc_health_map_2
message("Using approach ", if(health_count_1 >= health_count_2) "1" else "2", 
        " with ", sum(!is.na(nc_health_map$obesity)), " health data rows")

# 5. ADD COUNTY FOOD ENVIRONMENT DATA
county_data <- st_drop_geometry(nc_counties) %>%
  select(NAME) %>%
  left_join(parsed_food_environment, by = c("NAME" = "County"))

nc_health_map <- merge(nc_health_map, county_data,
                       by.x = "County", by.y = "NAME",
                       all.x = TRUE)

# 6. CREATE URBAN DATASET
urban_data <- nc_health_map %>%
  filter(!is.na(obesity)) %>%  # Only keep tracts with health data
  st_drop_geometry() %>%       # Remove geometry for analysis
  select(
    # Basic identifiers
    GEOID, County,
    # Food access indicators
    LILATracts_1And10, LILATracts_halfAnd10, LILATracts_1And20,
    LATracts_half, LATracts1, LATracts10, LATracts20,
    # Socioeconomic factors
    PovertyRate, MedianFamilyIncome, Urban,
    # Population access metrics
    contains("lapop"), contains("lalowi"),
    contains("lakids"), contains("laseniors"),
    contains("lahunv"),
    # Health outcomes
    obesity, diabetes,
    # Food environment
    SwampRatio, Swamp,
    # Classification
    food_desert
  ) %>%
  # Ensure all numeric variables are actually numeric
  mutate(across(where(is.character), ~as.numeric(as.character(.))))

# ========================= #
# VALIDATE FINAL DATASETS   #
# ========================= #

message("Final dataset summary:")
message("Total census tracts: ", nrow(nc_map_data))
message("Tracts with health data: ", nrow(urban_data))
message("Food desert counts:")
print(table(urban_data$food_desert))
message("Obesity summary:")
print(summary(urban_data$obesity))
message("Diabetes summary:")
print(summary(urban_data$diabetes))

# ========================= #
# SAVE PROCESSED DATASETS   #
# ========================= #

message("Saving processed datasets...")
saveRDS(nc_map_data, here("data", "processed", "nc_map_data.rds"))
saveRDS(nc_health_map, here("data", "processed", "nc_health_map.rds"))
saveRDS(urban_data, here("data", "processed", "urban_data.rds"))
saveRDS(nc_counties, here("data", "processed", "nc_counties.rds"))

message("Data preparation complete.")