# Filter Data ----------------------------------------------------------------------------
garbage_values <- c(
  "0",
  "،",
  ":،،،",
  "،،،،",
  "0")
key <- "new_uuid"

# Roster_Verification --------------------------------------------------------------------
Roster_Verification <- Roster_Verification %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))

# New_HH_Roster --------------------------------------------------------------------------
New_HH_Roster <- New_HH_Roster %>% 
  filter(!is.na(HH_Mem_Name) & 
           HH_Mem_Name %notin% garbage_values) %>% 
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))

# Labor ----------------------------------------------------------------------------------
Labor <- Labor %>%
  filter(!is.na(Family_Member_Labor_English) & 
           Family_Member_Labor_English %notin% garbage_values) %>% 
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))

# Education ------------------------------------------------------------------------------
Education <- Education %>%
  filter(!is.na(Family_Member_Education_English) & 
           Family_Member_Education_English %notin% garbage_values) %>% 
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))

# Health ---------------------------------------------------------------------------------
Health <- Health %>%
  filter(!is.na(Family_Member_Health_English) & 
           Family_Member_Health_English %notin% garbage_values) %>% 
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))

# Agriculture ----------------------------------------------------------------------------
Agriculture <- Agriculture %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
Agriculture <- Agriculture %>% 
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
    age_verification_final = 
      if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
    HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# Basic_needs ----------------------------------------------------------------------------
Basic_needs <- Basic_needs %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
Basic_needs <- Basic_needs %>% 
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# HH_Welfare -----------------------------------------------------------------------------
HH_Welfare <- HH_Welfare %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
HH_Welfare <- HH_Welfare %>%
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# Covid19 --------------------------------------------------------------------------------
Covid19 <- Covid19 %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
Covid19 <- Covid19 %>%
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# Market  --------------------------------------------------------------------------------
Market <- Market %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
Market <- Market %>%
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# Closing_Group  -------------------------------------------------------------------------
Closing_Group <- Closing_Group %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", ""))
#adds Family member name and age
Closing_Group <- Closing_Group %>%
  left_join(
    Roster_Verification %>% 
      select(Name_Roster, age_verification_final, new_uuid), by = key) %>% 
  left_join(
    New_HH_Roster %>% 
      select(HH_Mem_Name, HH_Mem_Age, new_uuid), by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .before = 1)

# remove extra objects -------------------------------------------------------------------
rm(garbage_values, key)
