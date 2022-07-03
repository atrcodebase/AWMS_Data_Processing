# clean the cleaning log -------------------------------------------
options(scipen = 999)
#dput(excel_sheets("input/raw_data/AWMS R2 - Final Tool.xlsx"))
awms_sheets <- c("data", "Roster_Verification", "New_HH_Roster", "Labor", "Education", 
                 "Health", "Agriculture", "Basic_needs", "HH_Welfare", "Covid19", 
                 "Market", "Closing_Group")
correction_log$not_found <- NA
#check non-existent logs
for(i in 1:nrow(correction_log)){
  tab_name <- correction_log$tab_name[i]
  key <- correction_log$KEY[i]
  question <- correction_log$question[i]
  
  #null sheet
  if(is.na(tab_name) | tab_name %notin% awms_sheets){
    correction_log$not_found[i] <- "tab_name"
  } else {
    #question not in sheet
    if(question %notin% names(get(tab_name))){
      correction_log$not_found[i] <- "question"
    }
    #key not in sheet
    if(key %notin% get(tab_name)$KEY){
      correction_log$not_found[i] <- "KEY"
    }
  }
}
correction_log$duplicates <- duplicated(correction_log[, c("KEY", "question")], fromLast = T) | duplicated(correction_log[, c("KEY", "question")])

correction_log_issues <- correction_log %>% 
  filter(!is.na(not_found) | duplicates == TRUE)

correction_log <- correction_log %>% 
  filter(is.na(not_found) & duplicates == FALSE)

# apply the correction-log ---------------------------------------------------------------
data_cleaned <- atRfunctions::apply_log(data = data, 
                                log = correction_log)
Roster_Verification_cleaned <- atRfunctions::apply_log(data = Roster_Verification, 
                                               log = correction_log)
New_HH_Roster_cleaned <- atRfunctions::apply_log(data = New_HH_Roster, 
                                         log = correction_log)
Labor_cleaned <- atRfunctions::apply_log(data = Labor, 
                                 log = correction_log)
Education_cleaned <- atRfunctions::apply_log(data = Education, 
                                     log = correction_log)
Health_cleaned <- atRfunctions::apply_log(data = Health, 
                                  log = correction_log)
Agriculture_cleaned <- atRfunctions::apply_log(data = Agriculture, 
                                       log = correction_log)
Basic_needs_cleaned <- atRfunctions::apply_log(data = Basic_needs, 
                                       log = correction_log)
HH_Welfare_cleaned <- atRfunctions::apply_log(data = HH_Welfare, 
                                      log = correction_log)
Covid19_cleaned <- atRfunctions::apply_log(data = Covid19, 
                                   log = correction_log)
Market_cleaned <- atRfunctions::apply_log(data = Market, 
                                  log = correction_log)
Closing_Group_cleaned <- atRfunctions::apply_log(data = Closing_Group, 
                                         log = correction_log)

#Apply L13 Coding ------------------------------------------------------------------------
for(i in 1:nrow(L13_coding_log)){
  uuid_i <- L13_coding_log$KEY[i]

  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "L13_Translation"] <- L13_coding_log$L13_Translation[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "L13_Job_Code"] <- L13_coding_log$L13_Job_Coding[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "L13_Label"] <- L13_coding_log$L13_Label[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "ISIC_Code"] <- L13_coding_log$ISIC_Code[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "ISIC_Label"] <- L13_coding_log$ISIC_Label[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "ISCO_Code"] <- L13_coding_log$ISCO_Code[i]
  Labor_cleaned[Labor_cleaned[["KEY"]] %in% uuid_i, "ISCO_Label"] <- L13_coding_log$ISCO_Label[i]
}


# Verify correction log ------------------------------------------------------------------
correction_log_discrep <- rbind(
  compare_dt(df1 = data, df2 = data_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "data"),
  compare_dt(df1 = Roster_Verification, df2 = Roster_Verification_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Roster_Verification"),
  compare_dt(df1 = New_HH_Roster, df2 = New_HH_Roster_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "New_HH_Roster"),
  compare_dt(df1 = Labor, df2 = Labor_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Labor"),
  compare_dt(df1 = Education, df2 = Education_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Education"),
  compare_dt(df1 = Health, df2 = Health_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Health"),
  compare_dt(df1 = Agriculture, df2 = Agriculture_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Agriculture"),
  compare_dt(df1 = Basic_needs, df2 = Basic_needs_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Basic_needs"),
  compare_dt(df1 = HH_Welfare, df2 = HH_Welfare_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "HH_Welfare"),
  compare_dt(df1 = Covid19, df2 = Covid19_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Covid19"),
  compare_dt(df1 = Market, df2 = Market_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Market"),
  compare_dt(df1 = Closing_Group, df2 = Closing_Group_cleaned,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(tab_name = "Closing_Group")
) %>%
  anti_join(correction_log, c("KEY",
                              "question",
                              # "old_value",
                              "new_value")) 
#Excluding L13 differences
correction_log_discrep <- correction_log_discrep %>% 
  filter(KEY %notin% L13_coding_log$KEY & !grepl("L13_|_Code|_Label", question))

if(nrow(correction_log_discrep) == 0){
  print("All logs applied correctly!")
} else {
  print("The following logs are not applied correctly:")
  print(correction_log_discrep)
}


# re-updating columns in cleaned sheet ---------------------------------------------------
cols <- c("caseid",
          "Region_Final",
          "Province_Final",
          "Distric_Final",
          "Respondent_Type_f",
          "userid")
data_sub <- data_cleaned %>%
  select(all_of(cols), KEY)

# Roster_Verification
Roster_Verification_cleaned <- Roster_Verification_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# New_HH_Roster
New_HH_Roster_cleaned <- New_HH_Roster_cleaned %>% 
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Labor
Labor_cleaned <- Labor_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Education
Education_cleaned <- Education_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Health
Health_cleaned <- Health_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Agriculture
Agriculture_cleaned <- Agriculture_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Basic_needs
Basic_needs_cleaned <- Basic_needs_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# HH_Welfare
HH_Welfare_cleaned <- HH_Welfare_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Covid19
Covid19_cleaned <- Covid19_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Market
Market_cleaned <- Market_cleaned %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)
# Closing_Group
Closing_Group <- Closing_Group %>%
  select(-all_of(cols)) %>%
  left_join(data_sub, by = c("PARENT_KEY" = "KEY"), .before = 1) %>% 
  relocate(caseid:Distric_Final, .before = 1)


# remove extra objects -------------------------------------------
rm(awms_sheets, tab_name, key, question, cols, data_sub)

