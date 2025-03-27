# North Carolina Food Insecurity Analysis

This project analyzes food insecurity, food environments, and health outcomes in North Carolina.

Please note that the shapefiles were too large to track with GIT, so they must be downloaded separately if you wish to run this code

## Project Structure

- `data/` - Data files
  - `raw/` - Original, unmodified data files
  - `processed/` - Cleaned and merged datasets
- `scripts/` - R scripts for analysis
  - `00_packages.R` - Loads required packages
  - `01_data_preparation.R` - Prepares and merges data
  - `02_exploratory_analysis.R` - Initial data exploration
  - `03_pca_analysis.R` - Principal Component Analysis
  - `04_regression_analysis.R` - Regression modeling
- `output/` - Results and outputs
  - `figures/` - Generated plots and maps
  - `tables/` - Summary tables and results
  - `models/` - Saved model objects
- `docs/` - Documentation and notes

## Running the Analysis

1. Place raw data files in `data/raw/`
2. Run individual scripts in order (01-04)

## Required Data Files

The following raw data files should be placed in the `data/raw/` directory:

1. "Food Access Research Atlas.csv" - USDA Food Access Research Atlas data
2. "StateAndCountyData.csv" - USDA Food Environment Atlas data
3. "500_Cities__Census_Tract-level_Data.csv" - CDC PLACES health data
4. Shapefiles for NC census tracts and counties in appropriate subdirectories

