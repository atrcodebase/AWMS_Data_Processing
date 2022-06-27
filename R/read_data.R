# pull file names --------------------------------------------------
files <- list.files(data_path, pattern = ".xls")

for (data_name in files) {
  # AWMS
  if(str_detect(data_name, "AWMS")){
    awms <- data_name
  }
}

# Read data --------------------------------------------------
## AWMS
if (exists("awms")) {
  data <- read_excel(glue("{data_path}{awms}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  Roster_Verification <- read_excel(glue("{data_path}{awms}"), sheet = "Roster_Verification", guess_max = 100000, na = convert_to_na)
  New_HH_Roster <- read_excel(glue("{data_path}{awms}"), sheet = "New_HH_Roster", guess_max = 100000, na = convert_to_na)
  Labor <- read_excel(glue("{data_path}{awms}"), sheet = "Labor", guess_max = 100000, na = convert_to_na)
  Education <- read_excel(glue("{data_path}{awms}"), sheet = "Education", guess_max = 100000, na = convert_to_na)
  Health <- read_excel(glue("{data_path}{awms}"), sheet = "Health", guess_max = 100000, na = convert_to_na)
  Agriculture <- read_excel(glue("{data_path}{awms}"), sheet = "Agriculture", guess_max = 100000, na = convert_to_na)
  Basic_needs <- read_excel(glue("{data_path}{awms}"), sheet = "Basic_needs", guess_max = 100000, na = convert_to_na)
  HH_Welfare <- read_excel(glue("{data_path}{awms}"), sheet = "HH_Welfare", guess_max = 100000, na = convert_to_na)
  Covid19 <- read_excel(glue("{data_path}{awms}"), sheet = "Covid19", guess_max = 100000, na = convert_to_na)
  Market <- read_excel(glue("{data_path}{awms}"), sheet = "Market", guess_max = 100000, na = convert_to_na)
  Closing_Group <- read_excel(glue("{data_path}{awms}"), sheet = "Closing_Group", guess_max = 100000, na = convert_to_na)
}

# remove extra objects --------------------------------------------------
rm(convert_to_na, data_name, data_path, files, awms)



