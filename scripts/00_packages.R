
# 00_packages.R
# Load and install required packages

packages <- c(
  "dplyr",
  "tidyr",
  "leaflet",
  "sf",
  "ggplot2",
  "factoextra",
  "randomForest", 
  "corrplot",
  "effects",
  "here",
  "readr"
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

message("All required packages loaded successfully.")
