##### Data Processing Script #####

# Install/load required packages ---------------------------------------------------------
if(!require(atRfunctions)) install_github("atrcodebase/atRfunctions")
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
# if(!require(lubridate)) install.packages("lubridate")
if(!require(writexl)) install.packages("writexl")
if(!require(tidyr)) install.packages("tidyr")
source("R/functions/custom_functions.R")

# Read data ------------------------------------------------------------------------------
# file.edit("R/read_data.R")
data_path <- "input/raw_data/" # data path
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
geo_data <- read_excel("input/geo_updated.xlsx", sheet="Changes")
source("R/read_data.R") # read data


# Applying filters & adding new columns --------------------------------------------------
# file.edit("R/filter_and_add.R")
source("R/filter_and_add.R")

# read qa-log, correction log and L13_coding_log -----------------------------------------
url <- "https://docs.google.com/spreadsheets/d/15kSTFTadHeS16_DAa10vUzInIqXBBRN9FnrwExyIb-g/edit?pli=1#gid=1472734570"
googlesheets4::gs4_deauth()
correction_log <- googlesheets4::read_sheet(url, sheet = "Correction_log", col_types = "c")
L13_coding_log <- googlesheets4::read_sheet(url, sheet = "L13_Coding_Sheet", col_types = "c")
QA_log <- googlesheets4::read_sheet(url, sheet = "QA_Log_R2", col_types = "c")
rejection_log <- googlesheets4::read_sheet(url, sheet = "Tobe_dropped", col_types = "c")

# apply correction/translation log -------------------------------------------------------
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")

# remove rejected keys -------------------------------------------------------------
# file.edit("R/remove_rejected_keys.R")
source("R/remove_rejected_keys.R")

# Filter Data ----------------------------------------------------------------------------
data <- data %>% 
  left_join(QA_log %>% select(KEY=UUID, qa_status=`Final QA Status`), by="KEY")
data <- data %>% 
  filter(qa_status %in% "Approved")

# Filter repeating groups based on the mainsheet -----------------------------------------
# file.edit("R/filter_repeating_groups.R")
source("R/filter_repeating_groups.R")

# remove extra columns -------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R")

# export results -------------------------------------------------------------------------
### Raw Data
awms_raw <- list(
  data = data_raw, 
  Roster_Verification = Roster_Verification_raw, 
  New_HH_Roster = New_HH_Roster_raw, 
  Labor = Labor_raw,
  Education = Education_raw, 
  Health = Health_raw,
  Agriculture = Agriculture_raw,
  Basic_needs = Basic_needs_raw, 
  HH_Welfare = HH_Welfare_raw,
  Covid19 = Covid19_raw, 
  Market = Market_raw,
  Closing_Group = Closing_Group_raw)

### Cleaned Data
awms_cleaned <- list(
  data = data, 
  Roster_Verification = Roster_Verification, 
  New_HH_Roster = New_HH_Roster, 
  Labor = Labor,
  Education = Education, 
  Health = Health,
  Agriculture = Agriculture,
  Basic_needs = Basic_needs, 
  HH_Welfare = HH_Welfare,
  Covid19 = Covid19, 
  Market = Market,
  Closing_Group = Closing_Group)

# export raw data ------------------------------------------------------------------------
if (!file.exists("output/raw_data")) {
  dir.create("output/raw_data", showWarnings = TRUE, recursive = TRUE)
  cat("Created 'output/raw_data' folder")
} else {
  cat("The 'output/raw_data' folder already exists")
}

## export raw datasets
writexl::write_xlsx(awms_raw, "output/raw_data/AWMS_R2_Final_Tool_raw_data.xlsx", format_headers = F) # AWMS raw data

# export cleaned data ------------------------------------------------------------------------
if (!file.exists("output/cleaned_data")) {
  dir.create("output/cleaned_data", showWarnings = TRUE, recursive = TRUE)
  cat("Created 'output/cleaned_data' folder")
} else {
  cat("The 'output/cleaned_data' folder already exists")
}

## export cleaned datasets
writexl::write_xlsx(awms_cleaned, "output/cleaned_data/AWMS_R2_Final_Tool_cleaned_data.xlsx", format_headers = F) # AWMS cleaned data
## export Correction Log Issues
writexl::write_xlsx(correction_log, glue::glue("output/Correction_log_{Sys.Date()}.xlsx")) # correction log
writexl::write_xlsx(correction_log_issues, "output/Correction_log_issues.xlsx", format_headers = F) # correction log issues

