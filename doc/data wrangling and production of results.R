#---------------------------------------------
# RESULTS SCRIPT WITH COMMENTED STEPS
#---------------------------------------------

# Install and load relevant packages for data manipulation, analysis, and reporting
install.packages("xfun")
install.packages("here")
install.packages("tidyverse")
install.packages(c("ggsurvfit", "gt"))
install.packages("survival")
install.packages("gtsummary")
install.packages("rms")
install.packages("glue")
install.packages("kableExtra")

library(xfun)       # Utility functions for R
library(glue)       # For constructing strings
library(tidyverse)  # For data wrangling (dplyr, ggplot2, etc.)
library(survival)   # Survival analysis
library(ggsurvfit)  # Plotting survival curves
library(broom)      # Tidy model outputs
library(rms)        # Regression modeling strategies
library(gtsummary)  # Summary tables
library(gt)         # Presentation of tables
library(kableExtra) # Enhanced table formatting

# Source custom functions from separate R script
source(here::here("R/functions.R"))

#---------------------------------------------
# Set analysis type
#---------------------------------------------
# Choose between "dm2" (type 2 diabetes) or "DMall" (all diabetes types)
dm_c = "DMall"

# Choose the risk variable depending on diabetes type
if(dm_c == "DMall"){risk_c = "risk_var"}
if(dm_c == "dm2"){risk_c = "risk_vardm2"}

#---------------------------------------------
# Define education levels for harmonization
#---------------------------------------------
edu_levels <- c(
    "None of the above",
    "O levels/GCSEs or equivalent",
    "CSEs or equivalent",
    "A levels/AS levels or equivalent",
    "NVQ or HND or HNC or equivalent",
    "Other professional qualifications eg: nursing, teaching",
    "College or University degree",
    "Prefer not to answer"
)

#---------------------------------------------
# Read and wrangle dataset
#---------------------------------------------
dataset <- read.csv(here::here("data-raw", "dataset.csv"))

data <- dataset %>%
    mutate(
        # Convert DMall to factor
        DMall = as_factor(dmall),
        DMall_dato = dmall_date,

        # Create dm2 variable based on specific diagnosis codes
        dm2 = case_when(
            DMall == 0 ~ 0,
            str_starts(dmall_source, "E11") ~ 1,
            str_starts(dmall_source, "O241") ~ 1,
            str_starts(dmall_source, "25000") ~1,
            TRUE ~ 0
        ),

        # Basic demographics
        alder = age_when_attended_assessment_centre_instance_0,
        bmi = body_mass_index_bmi_instance_0_participant_p21001_i0,

        # Categorize BMI into standard groups
        bmikat = case_when(
            bmi < 18.5 ~ 1,
            bmi >= 18.5 & bmi < 25 ~ 0,
            bmi >= 25 & bmi < 30 ~ 2,
            bmi >= 30 ~ 3
        ),

        # Blood pressure and lab measurements
        systolic = systolic_blood_pressure_automated_reading_instance_0_array_0,
        diastolic = diastolic_blood_pressure_automated_reading_instance_0_array_0,
        ldl = ldl_direct_instance_0,
        hdl = hdl_cholesterol_instance_0,
        trig = triglycerides_instance_0,
        glucose = glucose_instance_0_participant_p30740_i0,
        hscrp = c_reactive_protein_instance_0,

        # Medication indicator for lipid-lowering drugs
        lipid_lowering = case_when(
            str_detect(
                medication_for_cholesterol_blood_pressure_or_diabetes_instance_0,
                "^Cholesterol"
            ) |
                str_detect(
                    medication_for_cholesterol_blood_pressure_diabetes_or_take_exogenous_hormones_instance_0,
                    "^Cholesterol"
                ) ~ 1,
            TRUE ~  0
        ),

        # Smoking status (current smoker = 1)
        v37 = case_when(
            str_detect(current_tobacco_smoking_instance_0, "^(Yes|Only)") ~ 1,
            str_detect(current_tobacco_smoking_instance_0, "No") ~ 0,
            TRUE ~ NA
        ),

        # Encode sex as numeric (Male=1, Female=2)
        sex = case_when(sex == "Female" ~ 2, sex == "Male" ~ 1),

        # Follow-up time for ASCVD in years
        fu_ascvd = as.numeric(
            as_date(ascvd_date) - as_date(date_of_attending_assessment_centre_instance_0)
        ) / 365.25,

        # Death indicator and death date
        dod = case_when(
            is.na(date_of_death) ~ 0,
            !is.na(date_of_death) ~ 1,
            reason_lost_to_follow_up == "Death reported to UK Biobank by a relative" ~ 1
        ),
        dod_date = case_when(
            !is.na(date_of_death) ~ as_date(date_of_death),
            dod == 0 & is.na(date_of_death) ~ as_date("2022-05-31")
        ),
        dod_date = case_when(
            as_date(date_lost_to_follow_up) < dod_date ~ as_date(date_lost_to_follow_up),
            dod_date > as_date("2022-05-31") ~ as_date("2022-05-31"),
            TRUE ~ dod_date
        ),

        # Follow-up time until death in years
        fu_dod = as.numeric(
            as_date(dod_date) - as_date(date_of_attending_assessment_centre_instance_0)
        ) / 365.25,

        # Status for censoring due to leaving UK
        status = case_when(
            reason_lost_to_follow_up == "NHS records indicate they have left the UK" ~ 2,
            reason_lost_to_follow_up == "UK Biobank sources report they have left the UK" ~ 2
        ),

        # Highest educational attainment using custom function
        highest_edu = map_chr(qualifications_instance_0, get_highest_edu),
        highest_edu = factor(highest_edu, levels = edu_levels),

        # Numeric coding of education for analysis
        educa = case_when(
            highest_edu == "None of the above" ~ 7,
            highest_edu == "CSEs or equivalent" ~ 10,
            highest_edu == "O levels/GCSEs or equivalent" ~ 10,
            highest_edu == "A levels/AS levels or equivalent" ~ 13,
            highest_edu == "NVQ or HND or HNC or equivalent" ~ 13,
            highest_edu == "Other professional qualifications eg: nursing, teaching" ~ 16,
            highest_edu == "College or University degree" ~ 18,
            highest_edu == "Prefer not to answer" ~ NA_real_,
            TRUE ~ NA_real_
        ),

        # Household income coding
        income = case_when(
            average_total_household_income_before_tax_instance_0 == "Less than 18,000"  ~ 0,
            average_total_household_income_before_tax_instance_0 == "18,000 to 30,999" ~ 1,
            average_total_household_income_before_tax_instance_0 == "31,000 to 51,999" ~ 2,
            average_total_household_income_before_tax_instance_0 == "52,000 to 100,000" ~ 3,
            average_total_household_income_before_tax_instance_0 == "Greater than 100,000" ~ 4,
            average_total_household_income_before_tax_instance_0 == "Do not know" ~ 5,
            average_total_household_income_before_tax_instance_0 == "Prefer not to answer" ~ 6
        ),
        income = as_factor(income),

        # Risk group creation for combined DM/BMI categories
        risk_var = case_when(
            DMall == 0 & bmikat == 0 ~ 1,
            DMall == 0 & bmikat == 2 ~ 2,
            DMall == 0 & bmikat == 3 ~ 3,
            DMall == 1 & bmikat == 0 ~ 4,
            DMall == 1 & bmikat == 2 ~ 5,
            DMall == 1 & bmikat == 3 ~ 6
        ),
        risk_var = as.factor(risk_var),

        risk_vardm2 = case_when(
            dm2 == 0 & bmikat == 0 ~ 1,
            dm2 == 0 & bmikat == 2 ~ 2,
            dm2 == 0 & bmikat == 3 ~ 3,
            dm2 == 1 & bmikat == 0 ~ 4,
            dm2 == 1 & bmikat == 2 ~ 5,
            dm2 == 1 & bmikat == 3 ~ 6
        ),
        risk_vardm2 = as.factor(risk_vardm2)
    ) %>%
    filter(fu_ascvd >= 0) # Keep only participants without ASCVD at baseline

#---------------------------------------------
# RESULTS DATA GENERATION UKB BIOBANK
#---------------------------------------------
# Table of baseline characteristics

gtsave(toc(sex_ = 2)[[1]], "ukb_toc_w.html", path = here::here("obdi", "doc/manuscript/results files ukb/"))

gtsave(toc(sex_ = 1)[[1]], "ukb_toc_m.html", path = here::here("obdi", "docs/manuscript/results files ukb/"))

# Distribution table for UK Biobank participants by DMall
ukb_dist_n <- distribution(dm = "DMall")[[2]]
write.csv(ukb_dist_n, here::here("doc/manuscript/results files ukb/ukb_dist_n.csv"), row.names = FALSE)

# Cumulative incidence for women (risk_var groups)
ukb_cum_n_w <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =2)[[3]]
write.csv(ukb_cum_n_w, here::here("doc/manuscript/results files ukb/ukb_cum_n_w.csv"), row.names = FALSE)

# Cumulative incidence for men
ukb_cum_n_m <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =1)[[3]]
write.csv(ukb_cum_n_m, here::here("doc/manuscript/results files ukb/ukb_cum_n_m.csv"), row.names = FALSE)

# Number of participants by sex
ukb_n_w <- toc(sex_=2)[[2]]
write.csv(ukb_n_w, here::here("cvd_obesity_diabetes_ukbrap", "doc/manuscript/results files ukb/ukb_n_w.csv"), row.names = FALSE)

ukb_n_m <- toc(sex_=1)[[2]]
write.csv(ukb_n_m,  here::here("cvd_obesity_diabetes_ukbrap", "doc/manuscript/results files ukb/ukb_n_m.csv"), row.names = FALSE)

# Follow-up summary (min, max, mean, median) excluding BMI category 1
ukb_fu <- data %>% filter(bmikat !=1) %>% summarise(min(fu_ascvd), max(fu_ascvd), mean(fu_ascvd), median(fu_ascvd))
write.csv(ukb_fu, here::here("cvd_obesity_diabetes_ukbrap", "doc/manuscript/results files ukb/ukb_fu.csv"), row.names = FALSE)

# Cox regression results for women
cox_w <- forestplot_lej(fu= "fu_ascvd", sex_ = 2, endpoint = "ascvd", multifac_adj = FALSE, risk_var = "risk_var")[[3]]
write.csv(cox_w,  here::here("doc/manuscript/results files ukb/ukb_cox_w.csv"), row.names = FALSE)

# Cox regression results for men
cox_m <- forestplot_lej(fu= "fu_ascvd", sex_ = 1, endpoint = "ascvd", multifac_adj = FALSE, risk_var = "risk_var")[[3]]
write.csv(cox_m,  here::here("doc/manuscript/results files ukb/ukb_cox_m.csv"), row.names = FALSE)

# Absolute risk (Poisson regression) for women
abs_w <- poisson_barplot(dm = dm_c, sex_ = 2, futime = "fu_ascvd", endpoint = "ascvd")[[3]]
write.csv(abs_w,  here::here("cvd_obesity_diabetes_ukbrap","doc/manuscript/results files ukb/ukb_abs_w.csv"), row.names = FALSE)

# Absolute risk (Poisson regression) for men
abs_m <- poisson_barplot(dm = dm_c, sex_ = 1, futime = "fu_ascvd", endpoint = "ascvd")[[3]]
write.csv(abs_m,  here::here("cvd_obesity_diabetes_ukbrap","doc/manuscript/results files ukb/ukb_abs_m.csv"), row.names = FALSE)

# Cumulative incidence curves for women and men
cuminc_w <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =2)[[2]]
write.csv(cuminc_w, here::here("doc/manuscript/results files ukb/ukb_cuminc_w.csv"), row.names = FALSE)

cuminc_m <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =1)[[2]]
write.csv(cuminc_m, here::here("doc/manuscript/results files ukb/ukb_cuminc_m.csv"), row.names = FALSE)

# Spline function results for women and men
spline_w <- spline_function(dm = dm_c, sex_ = 2, futime = "fu_ascvd", endpoint = "ascvd")[[2]]
write.csv(spline_w, here::here("doc/manuscript/results files ukb/ukb_spline_w.csv"), row.names = FALSE)

spline_m <- spline_function(dm = dm_c, sex_ = 1, futime = "fu_ascvd", endpoint = "ascvd")[[2]]
write.csv(spline_m, here::here("doc/manuscript/results files ukb/ukb_spline_m.csv"), row.names = FALSE)

#---------------------------------------------
# RESULTS DATA GENERATION CGPS
#---------------------------------------------

# Load CGPS dataset (done in a secure environment) to run the same functions to generate results

# Table of baseline characteristics

gtsave(toc(sex_ = 2)[[1]], "cgps_toc_w.html", path = here::here("obdi", "doc/manuscript/results files cgps/"))

gtsave(toc(sex_ = 1)[[1]], "cgps_toc_m.html", path = here::here("obdi", "docs/manuscript/results files cgps/"))

# Distribution table for CGPS participants by DMall
cgps_dist_n <- distribution(dm = "DMall")[[2]]
write.csv(cgps_dist_n, here::here("doc/manuscript/results files cgps/cgps_dist_n.csv"), row.names = FALSE)

# Cumulative incidence for women (risk_var groups)
cgps_cum_n_w <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =2)[[3]]
write.csv(cgps_cum_n_w, here::here("doc/manuscript/results files cgps/cgps_cum_n_w.csv"), row.names = FALSE)

# Cumulative incidence for men
cgps_cum_n_m <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =1)[[3]]
write.csv(cgps_cum_n_m, here::here("doc/manuscript/results files cgps/cgps_cum_n_m.csv"), row.names = FALSE)

# Number of participants by sex
cgps_n_w <- toc(sex_=2)[[2]]
write.csv(cgps_n_w, here::here("cvd_obesity_diabetes_cgpsrap", "doc/manuscript/results files cgps/cgps_n_w.csv"), row.names = FALSE)

cgps_n_m <- toc(sex_=1)[[2]]
write.csv(cgps_n_m,  here::here("cvd_obesity_diabetes_cgpsrap", "doc/manuscript/results files cgps/cgps_n_m.csv"), row.names = FALSE)

# Follow-up summary (min, max, mean, median) excluding BMI category 1
cgps_fu <- data %>% filter(bmikat !=1) %>% summarise(min(fu_ascvd), max(fu_ascvd), mean(fu_ascvd), median(fu_ascvd))
write.csv(cgps_fu, here::here("cvd_obesity_diabetes_cgpsrap", "doc/manuscript/results files cgps/cgps_fu.csv"), row.names = FALSE)

# Cox regression results for women
cox_w <- forestplot_lej(fu= "fu_ascvd", sex_ = 2, endpoint = "ascvd", multifac_adj = FALSE, risk_var = "risk_var")[[3]]
write.csv(cox_w,  here::here("doc/manuscript/results files cgps/cgps_cox_w.csv"), row.names = FALSE)

# Cox regression results for men
cox_m <- forestplot_lej(fu= "fu_ascvd", sex_ = 1, endpoint = "ascvd", multifac_adj = FALSE, risk_var = "risk_var")[[3]]
write.csv(cox_m,  here::here("doc/manuscript/results files cgps/cgps_cox_m.csv"), row.names = FALSE)

# Absolute risk (Poisson regression) for women
abs_w <- poisson_barplot(dm = dm_c, sex_ = 2, futime = "fu_ascvd", endpoint = "ascvd")[[3]]
write.csv(abs_w,  here::here("cvd_obesity_diabetes_cgpsrap","doc/manuscript/results files cgps/cgps_abs_w.csv"), row.names = FALSE)

# Absolute risk (Poisson regression) for men
abs_m <- poisson_barplot(dm = dm_c, sex_ = 1, futime = "fu_ascvd", endpoint = "ascvd")[[3]]
write.csv(abs_m,  here::here("cvd_obesity_diabetes_cgpsrap","doc/manuscript/results files cgps/cgps_abs_m.csv"), row.names = FALSE)

# Cumulative incidence curves for women and men
cuminc_w <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =2)[[2]]
write.csv(cuminc_w, here::here("doc/manuscript/results files cgps/cgps_cuminc_w.csv"), row.names = FALSE)

cuminc_m <- cum_inc_cr(futime = "fu_ascvd", endpoint ="ascvd", by_cat = "risk_var", sex_ =1)[[2]]
write.csv(cuminc_m, here::here("doc/manuscript/results files cgps/cgps_cuminc_m.csv"), row.names = FALSE)

# Spline function results for women and men
spline_w <- spline_function(dm = dm_c, sex_ = 2, futime = "fu_ascvd", endpoint = "ascvd")[[2]]
write.csv(spline_w, here::here("doc/manuscript/results files cgps/cgps_spline_w.csv"), row.names = FALSE)

spline_m <- spline_function(dm = dm_c, sex_ = 1, futime = "fu_ascvd", endpoint = "ascvd")[[2]]
write.csv(spline_m, here::here("doc/manuscript/results files cgps/cgps_spline_m.csv"), row.names = FALSE)

