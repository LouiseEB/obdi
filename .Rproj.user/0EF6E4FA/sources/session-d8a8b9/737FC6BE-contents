# Obesity and diabetes alone and combined on risk of cardiovascular disease in women and men: cohort studies in Denmark and the UK

## Overview

This project contains the data wrangling, analysis, and results generation for studies of obesity and diabetes in the CGPS and UK Biobank cohorts. This project investigates the combined effects of diabetes and obesity on cardiovascular disease risk, using data from both the **Copenhagen General Population Study (CGPS)** and the **UK Biobank (UKB)**.  

Although the two cohorts are stored and processed in **separate environments** (UKB within its secure research environment using DNAnexus/UKB RAP, CGPS within local secure infrastructure), the analysis workflow is designed to be **parallel and reproducible across both datasets**.  

---

## Directory Structure
```
├── data-raw/
│ └── UKB import raw data and define variables.R # Import UKB dataset and create variables from ICD and OPCS codes
│
├── doc/
│ └── data wrangling and production of results.R # Main script for data preparation & results generation
│
├── R/
│ └── functions.R # Shared functions used across CGPS and UKB analyses
│
├── DESCRIPTION # Metadata for reproducibility
├── obdi.Rproj # RStudio project file
└── README.md # Project documentation (this file)
```

## Analysis Workflow

1. **Functions (`R/functions.R`)**  
   - Contains all reusable helper functions.  
   - These are sourced at the beginning of the analysis scripts.
   - Functions cover tasks such as:  
     - Outputting results, plots and counts for the analyses
   - There is a function for each display item in the manuscript (table of characteristics and figures).
   - An additional function to extract the highest level of education for each UK Biobank participant used in the data wrangling and production of results script.
   

2. **Data Import(`data-raw/UKB import raw data and define variables.R`)**  
   - Downloads and imports raw UKB data (`dataset.csv`).  
   - Creates ASCVD and diabetes variables from ICD and OPCS codes
   - Saves and uploads main data for the project to UKB RAP
  

3. **Data Wrangling & Results (`doc/data wrangling and production of results.R`)**  
   - Performs all transformations of data required for analysis.  
   - Defines key analytic variables:  
     - **DMall** (default; includes all diabetes types)  
     - **dm2** (alternative; type 2 diabetes only)  
     - Obesity categories  
     - Competing risks
     - Risk group definitions (3x2 matrix of BMI category × diabetes status)
     - Covariates as educational level, income, and current smoking     - 
   - Produces all results (tables and figures), written to `.csv` for reproducibility.  

---

## Running the Analyses

- **Default**: Analyses are run with `DMall` as the diabetes variable.  
- **Alternative**: To restrict to type 2 diabetes only, set `dm_c = "dm2"` in the results script.  
- Both CGPS and UKB scripts rely on the same functions and produce **parallel outputs**, allowing direct comparability across the two cohorts.  

---

## Outputs

- Cleaned, harmonized analysis datasets (not shared in this repo).(The final figures in the manuscript are created from these results files)
- Results tables exported as `.csv` into manuscript folders.  
- Figures generated from results scripts (e.g., forest plots, spline curves).  

---

## Reproducibility

- All packages required are listed in the scripts 
- `DESCRIPTION` is used to keep track of project metadata.  
- Analyses are run in separate environments (UKB secure environment vs CGPS secure infrastructure), but results scripts and functions are universal.  

---

**Note:** This repository contains the analysis scripts and functions, but **raw data are not shared** due to cohort-specific access restrictions. Final results, figures, and manuscript are not shared in this repository.  

