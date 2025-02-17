# Update Total Family members in Main sheet ----------------------------------------------
n_family <- rbind(Roster_Verification %>% 
                    filter(Roster_Name_Ver %notin% c("0", "98")) %>% 
                    select(PARENT_KEY),
                  New_HH_Roster %>% 
                    select(PARENT_KEY)) %>% 
  count(KEY=PARENT_KEY, name="n_family_sample_new")
data <- data %>% 
  left_join(n_family, by = "KEY") %>% 
  relocate(n_family_sample_new, .after = n_family_sample)

#Update District Name based on geo
for(i in 1:nrow(data)){
  prov = data$Province_Final[i]
  dist = data$Distric_Final[i]
  d_prov = data$database_province[i]
  d_dist = data$database_district[i]
  
  #Updates District Final
  if(prov %in% geo_data$Province & dist %in% geo_data$Previous_Name[geo_data$Province %in% prov]){
    new_dist <- geo_data$Revised_Name[geo_data$Province %in% prov & 
                                        geo_data$Previous_Name %in% dist]
    data$Distric_Final[i] <- new_dist
  }
  
  #Updates Database District
  if(d_prov %in% geo_data$Province & d_dist %in% geo_data$Previous_Name[geo_data$Province %in% d_prov]){
    new_dist <- geo_data$Revised_Name[geo_data$Province %in% d_prov & 
                                        geo_data$Previous_Name %in% d_dist]
    data$database_district[i] <- new_dist
  }
}
#print in case province not in geo_data
missing_prov <- unique(data$database_province[data$database_province %notin% geo_data$Province])
if(length(missing_prov) > 0){
  print("The following provinces are not in Geo-Data:")
  print(missing_prov)
}

# Create A subset of main sheet to be merged with child sheets
key <- "new_uuid"
cols <- "caseid|Region_Final|Province_Final|Distric_Final|Respondent_Type_f|userid" #include after log
data_sub <- data %>%
  select(grep(cols,names(data)), KEY)

# Roster_Verification --------------------------------------------------------------------
Roster_Verification <- Roster_Verification %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Roster_Verification))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#subset to join with other sheets
roster_verf_sub <- Roster_Verification %>% 
  select(Name_Roster, age_verification_final, new_uuid)

# New_HH_Roster --------------------------------------------------------------------------
New_HH_Roster <- New_HH_Roster %>% 
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(New_HH_Roster))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#subset to join with other sheets
new_roster_sub <- New_HH_Roster %>% 
  select(HH_Mem_Name, HH_Mem_Age, new_uuid)

# Labor ----------------------------------------------------------------------------------
Labor <- Labor %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Labor))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
Labor <- Labor %>%
  filter(Family_Member_Age_Labor >= 14) %>% 
  mutate(L13_Translation = NA_character_, .after = L13)

# Education ------------------------------------------------------------------------------
Education <- Education %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Education))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
Education <- Education %>%
  filter(Family_Member_Age_Education >= 6 & Family_Member_Age_Education <= 20)

# Health ---------------------------------------------------------------------------------
Health <- Health %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Health))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)

# Agriculture ----------------------------------------------------------------------------
Agriculture <- Agriculture %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Agriculture))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# adds Family member name and age
Agriculture <- Agriculture %>% 
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
    age_verification_final = 
      if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
    HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)
# Filter
agr_cols <- c(
  "phone_response_short",
  "Am_I_Speaking_to_Household_Head",
  "Can_I_Speack_to_Household_Head"
)
Agriculture <- Agriculture %>% 
  left_join(data %>% 
              select(all_of(agr_cols), KEY), 
            by = c("PARENT_KEY" = "KEY")) %>% 
  filter(phone_response_short == "Complete" & (Am_I_Speaking_to_Household_Head == 1 | Can_I_Speack_to_Household_Head == 1)) %>% 
  select(-all_of(agr_cols))
  
# Basic_needs ----------------------------------------------------------------------------
Basic_needs <- Basic_needs %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Basic_needs))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#adds Family member name and age
Basic_needs <- Basic_needs %>% 
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)

# HH_Welfare -----------------------------------------------------------------------------
HH_Welfare <- HH_Welfare %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(HH_Welfare))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#adds Family member name and age
HH_Welfare <- HH_Welfare %>%
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)

# Covid19 --------------------------------------------------------------------------------
Covid19 <- Covid19 %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Covid19))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#adds Family member name and age
Covid19 <- Covid19 %>%
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)

# Market  --------------------------------------------------------------------------------
Market <- Market %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Market))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#adds Family member name and age
Market <- Market %>%
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>%  
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)

# Closing_Group  -------------------------------------------------------------------------
Closing_Group <- Closing_Group %>%
  mutate(new_uuid = str_replace(KEY, "(?<=Roster)(.*?)(?=\\[)", "")) %>% 
  select(-grep(cols,names(Closing_Group))) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
#adds Family member name and age
Closing_Group <- Closing_Group %>%
  left_join(roster_verf_sub, by = key) %>% 
  left_join(new_roster_sub, by = key) %>% 
  mutate(Name_Roster = 
           if_else(is.na(Name_Roster), HH_Mem_Name,Name_Roster),
         age_verification_final = 
           if_else(is.na(age_verification_final), HH_Mem_Age,age_verification_final), 
         HH_Mem_Name=NULL, HH_Mem_Age=NULL) %>% 
  relocate(Name_Roster, age_verification_final, .after = Distric_Final)

# remove extra objects -------------------------------------------------------------------
rm(key, cols, data_sub, agr_cols, n_family, roster_verf_sub, new_roster_sub)
