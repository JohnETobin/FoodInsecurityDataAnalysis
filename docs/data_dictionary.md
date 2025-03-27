# Data Dictionary for North Carolina Food Insecurity Analysis

## Food Access Research Atlas Variables

- `CensusTract` - Census tract ID
- `State` - State name
- `County` - County name
- `Urban` - Flag for urban tract
- `POP2010` - Population count from 2010 census
- `LILATracts_1And10` - Flag for low-income and low access at 1/10 miles
- `LILATracts_halfAnd10` - Flag for low-income and low access at 0.5/10 miles
- `LowIncomeTracts` - Flag for low income tract
- `PovertyRate` - Tract poverty rate
- `MedianFamilyIncome` - Tract median family income
- `LATracts_half` - Flag for low access at 0.5 mile
- `LATracts1` - Flag for low access at 1 mile

## Food Environment Atlas Variables

- `GROC16` - Grocery stores, 2016
- `CONVS16` - Convenience stores, 2016
- `FFR16` - Fast-food restaurants, 2016
- `FMRKT18` - Farmers' markets, 2018
- `SUPERC16` - Supercenters & club stores, 2016
- `SwampRatio` - Ratio of unhealthy to healthy food retailers
- `Swamp` - Binary flag for food swamp (SwampRatio > 3.89)

## Health Data Variables

- `OBESITY_CrudePrev` - Obesity prevalence
- `DIABETES_CrudePrev` - Diabetes prevalence

## Derived Variables

- `food_desert` - Binary classification of food desert status
- `PC1`, `PC2`, `PC3` - Principal components from PCA analysis
- `lisa_cluster` - LISA spatial cluster classification

