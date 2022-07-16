# Install/load required packages ---------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(glue)) install.packages("glue")
if(!require(writexl)) install.packages("writexl")
if(!require(tidyr)) install.packages("tidyr")
source("R/functions/custom_functions.R")

# Read data ------------------------------------------------------------------------------
data_path <- "output/cleaned_approved_data/" # data path
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
source("R/read_data.R") # read data
tool_relevancy <- read_excel("input/tools/tool_relevancies.xlsx")

#apply function --------------------------------------------------------------------------
main_relevancy <- relevancy_check(data, tool_relevancy, "main")
Roster_Verification_relevancy <- relevancy_check(Roster_Verification, tool_relevancy, "Roster_Verification")
New_HH_Roster_relevancy <- relevancy_check(New_HH_Roster, tool_relevancy, "New_HH_Roster")
Labor_relevancy <- relevancy_check(Labor, tool_relevancy, "Labor")
Education_relevancy <- relevancy_check(Education, tool_relevancy, "Education")
Health_relevancy <-  relevancy_check(Health, tool_relevancy, "Health")
Agriculture_relevancy <- relevancy_check(Agriculture, tool_relevancy, "Agriculture")
Basic_needs_relevancy <- relevancy_check(Basic_needs, tool_relevancy, "Basic_needs")
HH_Welfare_relevancy <- relevancy_check(HH_Welfare, tool_relevancy, "HH_Welfare")
Covid19_relevancy <- relevancy_check(Covid19, tool_relevancy, "Covid19")
Market_relevancy <- relevancy_check(Market, tool_relevancy, "Market")
Closing_Group_relevancy <- relevancy_check(Closing_Group, tool_relevancy, sheet="Closing_Group")

relevancy_issues <- rbind(
  main_relevancy,
  Roster_Verification_relevancy,
  New_HH_Roster_relevancy,
  Labor_relevancy,
  Education_relevancy,
  Health_relevancy,
  Agriculture_relevancy,
  Basic_needs_relevancy,
  HH_Welfare_relevancy,
  Covid19_relevancy,
  Market_relevancy,
  Closing_Group_relevancy
)
#temporary
relevancy_issues <- relevancy_issues %>% 
  filter(!(relevant_q %in% "HH_Mem_Age" &  as.numeric(relev_val) >= 18))

#Export ----------------------------------------------------------------------------------
writexl::write_xlsx(relevancy_issues, "output/Tool_relevancy_issues.xlsx")
