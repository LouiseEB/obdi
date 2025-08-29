# UKB import raw data and define variables

# - Raw data, containing baseline data and ICD and OPCS codes, previously exported on the UKB RAP, is loaded
# - ICD and OPCS cols are split and the ascvd and diabetes variables are made
# - ascvd and diabetes variables are added to the main dataset which is saved to the UKB RAP


# ------------------------------------------------------
# Download raw data files from DNAnexus (command line)
# ------------------------------------------------------
# Example: dx download "table-exporter_2025-03-05_12-09-27_data.csv" \
#                 "table-exporter_2025-03-06_10-37-19_data.csv" \
#                 "table-exporter_2025-03-18_08-25-49_data.csv" \
#                 "table-exporter_2025-03-18_11-43-17_data.csv" \
#                 -o "data-raw"
#

# ------------------------------------------------------
# Install and load required R packages
# ------------------------------------------------------
install.packages("here")        # For managing file paths relative to project root
install.packages("tidyverse")   # Core data manipulation and visualization package suite
install.packages("janitor")     # For cleaning column names and simple tabulations

library(tidyverse)
# ------------------------------------------------------
# Load raw datasets exported in different steps from UKB RAP
# ------------------------------------------------------
# Main clinical dataset (drop main ICD and main OPCS raw strings. Correct ICD and OPCS variables will be loaded in the next step.)
full_data <- read.csv(here::here("obdi", "data-raw", "table-exporter_2025-03-05_12-09-27_data.csv")) %>%
    select(-matches("main.icd"), -matches("operative_procedures_main_opcs"))

# ICD/OPCS data
icd_op <- read.csv(here::here("obdi", "data-raw", "table-exporter_2025-03-06_10-37-19_data.csv"))

# Extra participant variables
addvars <- read.csv(here::here("obdi", "data-raw", "table-exporter_2025-03-18_08-25-49_data.csv"))

# HbA1c measures from baseline and first follow-up instances
hba1c <- read.csv(here::here("obdi", "data-raw", "table-exporter_2025-03-18_11-43-17_data.csv")) %>%
    select(Participant.ID, Glycated.haemoglobin..HbA1c....Instance.0, Glycated.haemoglobin..HbA1c....Instance.1)

# ------------------------------------------------------
# Construct working dataset
# ------------------------------------------------------
# Full join across datasets and name cleaning:
dataset <- full_join(full_data, icd_op) %>%
  full_join(addvars) %>%
  full_join(hba1c) %>%
  janitor::clean_names()


# Export intermediate dataset
write.csv(dataset, "dataset.csv", row.names = F, quote = F)

# Terminal upload: dx upload dataset.csv

# Remove intermediate objects to free memory
rm(full_data, icd_op, addvars, hba1c)


# ------------------------------------------------------
# Function to split ICD/OPCS columns into multiple fields
# ------------------------------------------------------
# UKB stores ICD/OPCS codes concatenated with "|" as delimiter.
# This function expands such fields into separate array-style columns.
split_icd_columns <- function(data, icd_col_name) {
    max_instances <- data[[icd_col_name]] %>%
        replace_na("") %>%
        str_count("\\|") %>%
        max(na.rm = TRUE) + 1  # maximum number of codes per participant

    data <- data %>%
        separate(
            icd_col_name,
            into = paste0(icd_col_name, "_array_", 0:(max_instances - 1)),
            sep = "\\|",
            fill = "right"
        )

    return(data)
}

# Apply splitting to ICD-10, ICD-9, and OPCS columns
dataset <- dataset %>%
    split_icd_columns("diagnoses_icd10") %>%
    split_icd_columns("diagnoses_icd9") %>%
    split_icd_columns("operative_procedures_opcs4") %>%
    split_icd_columns("operative_procedures_opcs3")

# ------------------------------------------------------
# Define ICD-10, ICD-9 and OPCS codes of interest, run separately for ASCVD and
# diabetes (comment out the irrelevant one while running code)
# ------------------------------------------------------
# ASCVD-related ICD-10 codes
icd10_codes <- c("DE10.5", "DE11.5", "DE13.5", "DE14.5", "G45", "DI20", "DI21",
                 "DI22", "DI23", "DI24", "DI25", "I63", "I69.3",
                 "DI70.2", "DI70.2A", "DI73.9", "L97")
icd10_codes <- str_replace(icd10_codes, "^D", "")  # remove "D" prefix if present


# ASCVD-related ICD-9 codes
icd9_codes <- c("413", "410", "4297", "411", "412", "414", "433",
                "434", "435", "4377", "2497", "2507", "4402", "7071", "7079")

# ASCVD-related OPCS-4 procedure codes (revascularisation etc.)
opcs4_codes <- c("K40", "K41", "K42", "K43", "K44", "K45", "K46", "K48", "K49", "K50", "K75")


# # Diabetes-related ICD-10 codes
# icd10_codes <- c("E10", "E11", "E12", "E13", "E14", "H36.0",
#                  "O24.0", "O24.1", "O24.2", "O24.3", "O24.5", "O24.9")
#
# # ICD-9 diabetes codes
# icd9_codes <- c("250", "3620", "3572", "249")

# ------------------------------------------------------
# Function to process diagnosis/procedure codes
# ------------------------------------------------------
# Extracts participants with specified codes, reshapes data to long format,
# and keeps date of first occurrence.
process_codes <- function(
        codes = icd10_codes,
        prefix = "diagnoses_icd10",
        date_prefix = "date_of_first_in_patient_diagnosis_icd10",
        row_range,
        new_col_name = "ascvd"
) {
    pattern <- paste0("^(", paste0(codes, collapse = "|"), ")")

    dataset[row_range, ] %>%
        select(participant_id, starts_with(prefix), starts_with(date_prefix)) %>%
        pivot_longer(
            cols = -participant_id,
            names_to = c(".value", "array_index"),
            names_pattern = "(.*)_array_(\\d+)"
        ) %>%
        filter(!is.na(!!sym(prefix)), !!sym(prefix) != "") %>%
        mutate(
            !!sym(new_col_name) := as.integer(str_detect(!!sym(prefix), pattern))
        ) %>%
        filter(!!sym(new_col_name) == 1) %>%
        rename(
            dia_proc = !!sym(prefix),
            date = !!sym(paste0(date_prefix))
        ) %>%
        mutate(date = as_date(date))
}

# ------------------------------------------------------
# Identify ASCVD cases (ICD-10, ICD-9, OPCS)
# ------------------------------------------------------
# Split into batches to handle large dataset sizes (UKB ~500k participants)
icd10_batch1 <- process_codes(new_col_name = "ascvd", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 1:200000)
icd10_batch2 <- process_codes(new_col_name = "ascvd", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 200001:400000)
icd10_batch3 <- process_codes(new_col_name = "ascvd", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 400001:502129)

icd9_batch1 <- process_codes(new_col_name = "ascvd", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 1:200000)
icd9_batch2 <- process_codes(new_col_name = "ascvd", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 200001:400000)
icd9_batch3 <- process_codes(new_col_name = "ascvd", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 400001:502129)

opcs4_batch1 <- process_codes(new_col_name = "ascvd", codes = opcs4_codes, prefix = "operative_procedures_opcs4", row_range = 1:200000 )
opcs4_batch2 <- process_codes(new_col_name = "ascvd", codes = opcs4_codes, prefix = "operative_procedures_opcs4", row_range = 200001:400000)
opcs4_batch3 <- process_codes(new_col_name = "ascvd", codes = opcs4_codes, prefix = "operative_procedures_opcs4", row_range = 400001:502129)

# Combine and filter ASCVD diagnoses
ascvd_col2 <- bind_rows(icd10_batch1, icd10_batch2, icd10_batch3,
                        icd9_batch1, icd9_batch2, icd9_batch3,
                        opcs4_batch1, opcs4_batch2, opcs4_batch3) %>%
    filter(dia_proc != "4309 Subarachnoid haemorrhage",   # Exclude haemorrhagic stroke
           dia_proc != "7854 Gangrene") %>%               # Exclude nonspecific gangrene
    group_by(participant_id) %>%
    arrange(date) %>%
    slice_head(n = 1) %>%                                 # Keep earliest event
    ungroup() %>%
    rename(ascvd_date = date,
           ascvd_array_index = array_index,
           ascvd_dia_proc = dia_proc) %>%
    arrange(participant_id)

# Save ASCVD dataset
write_csv(ascvd_col2, "obdi/data-raw/ascvd_col2.csv")

# Terminal upload: dx upload ascvd_col2.csv
# ------------------------------------------------------
# Identify Diabetes Mellitus (all types) cases
# ------------------------------------------------------
icd10_batch1 <- process_codes(new_col_name = "dmall", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 1:200000)
icd10_batch2 <- process_codes(new_col_name = "dmall", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 200001:400000)
icd10_batch3 <- process_codes(new_col_name = "dmall", codes = icd10_codes, prefix = "diagnoses_icd10", row_range = 400001:502129)

icd9_batch1 <- process_codes(new_col_name = "dmall", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 1:200000)
icd9_batch2 <- process_codes(new_col_name = "dmall", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 200001:400000)
icd9_batch3 <- process_codes(new_col_name = "dmall", codes = icd9_codes, prefix = "diagnoses_icd9", row_range = 400001:502129)

dmall_col <- bind_rows(icd10_batch1, icd10_batch2, icd10_batch3,
                       icd9_batch1, icd9_batch2, icd9_batch3) %>%
    group_by(participant_id) %>%
    arrange(date) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    rename(dmall_date = date,
           dmall_array_index = array_index,
           dmall_source = dia_proc) %>%
    arrange(participant_id)

# Save DM dataset
write_csv(dmall_col, "obdi/data-raw/dmall_col.csv")

# Terminal upload: dx upload dmall_col.csv

# ------------------------------------------------------
# Merge ASCVD and DM variables into main dataset
# ------------------------------------------------------
# Download prepared event datasets back into R session
# dx download dataset.csv
# dx download "ascvd_col2.csv" "dmall_col.csv" -o "obdi/data-raw"

# Reload cleaned base dataset
dataset <- read.csv(here::here("obdi", "data-raw", "dataset.csv"))

# Reload ASCVD and DM datasets (dates in proper format)
ascvd_col2 <- read.csv(here::here("obdi","data-raw", "ascvd_col2.csv")) %>%
    mutate(ascvd_date = as_date(ascvd_date))
dmall_col <- read.csv(here::here("obdi","data-raw", "dmall_col.csv")) %>%
    mutate(dmall_date = as_date(dmall_date))

# ------------------------------------------------------
# Construct final dataset with outcomes
# ------------------------------------------------------
dataset_diagnoses <- dataset %>%
    left_join(ascvd_col2) %>%
    mutate(date_lost_to_follow_up = as_date(date_lost_to_follow_up),
           date_of_death = as_date(date_of_death),
           # Define ASCVD indicator and censoring
           ascvd = case_when(
               is.na(ascvd) ~ 0,
               ascvd_date > as_date("2022-05-31") ~ 0,
               ascvd == 1 & date_lost_to_follow_up < ascvd_date ~ 0,
               TRUE ~ ascvd),
           ascvd_date = case_when(
               ascvd == 0 & !is.na(date_of_death) ~ date_of_death,
               ascvd == 0 & is.na(date_of_death) ~ as_date("2022-05-31"),
               is.na(ascvd_date) & ascvd == 1 ~ NA,
               TRUE ~ ascvd_date),
           ascvd_date = case_when(
               date_lost_to_follow_up < ascvd_date ~ date_lost_to_follow_up,
               ascvd_date > as_date("2022-05-31") ~ as_date("2022-05-31"),
               TRUE ~ ascvd_date)) %>%
    left_join(dmall_col) %>%
    mutate(date_lost_to_follow_up = as_date(date_lost_to_follow_up),
           # Define DM indicator and censoring
           dmall = case_when(
               is.na(dmall) ~ 0,
               dmall_date > as_date("2022-05-31") ~ 0,
               dmall == 1 & date_lost_to_follow_up < dmall_date ~ 0,
               TRUE ~ dmall),
           dmall_date = case_when(
               dmall == 0 & !is.na(date_of_death) ~ date_of_death,
               dmall == 0 & is.na(date_of_death) ~ as_date("2022-05-31"),
               is.na(dmall_date) & dmall == 1 ~ NA,
               TRUE ~ dmall_date),
           dmall_date = case_when(
               date_lost_to_follow_up < dmall_date ~ date_lost_to_follow_up,
               dmall_date > as_date("2022-05-31") ~ as_date("2022-05-31"),
               TRUE ~ dmall_date))


# Save final dataset
write_csv(dataset_diagnoses, "dataset.csv")

# Delete intermediary data from UKB RAP
# Terminal upload final dataset: dx upload dataset.csv

