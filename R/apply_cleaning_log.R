# clean the cleaning log -------------------------------------------
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

#Raw Data for QA -------------------------------------------------------------------------
data_raw <- data
Roster_Verification_raw <- Roster_Verification
New_HH_Roster_raw <- New_HH_Roster
Labor_raw <- Labor
Education_raw <- Education
Health_raw <- Health
Agriculture_raw <- Agriculture
Basic_needs_raw <- Basic_needs
HH_Welfare_raw <- HH_Welfare
Covid19_raw <- Covid19
Market_raw <- Market
Closing_Group_raw <- Closing_Group

# apply the correction-log ---------------------------------------------------------------
data <- atRfunctions::apply_log(data = data, 
                                log = correction_log)
Roster_Verification <- atRfunctions::apply_log(data = Roster_Verification, 
                                               log = correction_log)
New_HH_Roster <- atRfunctions::apply_log(data = New_HH_Roster, 
                                         log = correction_log)
Labor <- atRfunctions::apply_log(data = Labor, 
                                 log = correction_log)
Education <- atRfunctions::apply_log(data = Education, 
                                     log = correction_log)
Health <- atRfunctions::apply_log(data = Health, 
                                  log = correction_log)
Agriculture <- atRfunctions::apply_log(data = Agriculture, 
                                       log = correction_log)
Basic_needs <- atRfunctions::apply_log(data = Basic_needs, 
                                       log = correction_log)
HH_Welfare <- atRfunctions::apply_log(data = HH_Welfare, 
                                      log = correction_log)
Covid19 <- atRfunctions::apply_log(data = Covid19, 
                                   log = correction_log)
Market <- atRfunctions::apply_log(data = Market, 
                                  log = correction_log)
Closing_Group <- atRfunctions::apply_log(data = Closing_Group, 
                                         log = correction_log)

#Apply L13 Coding ------------------------------------------------------------------------
for(i in 1:nrow(L13_coding_log)){
  uuid_i <- L13_coding_log$KEY[i]

  Labor[Labor[["KEY"]] %in% uuid_i, "L13_Job_Code"] <- L13_coding_log$L13_Job_Coding[i]
  Labor[Labor[["KEY"]] %in% uuid_i, "L13_Label"] <- L13_coding_log$L13_Label[i]
  Labor[Labor[["KEY"]] %in% uuid_i, "ISIC_Code"] <- L13_coding_log$ISIC_Code[i]
  Labor[Labor[["KEY"]] %in% uuid_i, "ISIC_Label"] <- L13_coding_log$ISIC_Label[i]
  Labor[Labor[["KEY"]] %in% uuid_i, "ISCO_Code"] <- L13_coding_log$ISCO_Code[i]
  Labor[Labor[["KEY"]] %in% uuid_i, "ISCO_Label"] <- L13_coding_log$ISCO_Label[i]
}

# Verify correction log -------------------------------------------
correction_log_discrep <- rbind(
  compare_dt(df1 = data_raw, df2 = data,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "data"),
  compare_dt(df1 = Roster_Verification_raw, df2 = Roster_Verification,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Roster_Verification"),
  compare_dt(df1 = New_HH_Roster_raw, df2 = New_HH_Roster,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "New_HH_Roster"),
  compare_dt(df1 = Labor_raw, df2 = Labor,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Labor"),
  compare_dt(df1 = Education_raw, df2 = Education,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Education"),
  compare_dt(df1 = Health_raw, df2 = Health,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Health"),
  compare_dt(df1 = Agriculture_raw, df2 = Agriculture,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Agriculture"),
  compare_dt(df1 = Basic_needs_raw, df2 = Basic_needs,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Basic_needs"),
  compare_dt(df1 = HH_Welfare_raw, df2 = HH_Welfare,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "HH_Welfare"),
  compare_dt(df1 = Covid19_raw, df2 = Covid19,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Covid19"),
  compare_dt(df1 = Market_raw, df2 = Market,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Market"),
  compare_dt(df1 = Closing_Group_raw, df2 = Closing_Group,
                           unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(sheet_name = "Closing_Group")
) %>%
  anti_join(correction_log, c("KEY",
                              "question",
                              # "old_value",
                              "new_value")) %>% 
  filter(KEY %notin% L13_coding_log$KEY & !grepl("L13_|_Code|_Label", question))

if(nrow(correction_log_discrep) == 0){
  print("All logs applied correctly!")
} else {
  print("Some logs are not applied correctly!!!")
}


# remove extra objects -------------------------------------------
rm(awms_sheets, tab_name, key, question)
# rm(data_raw, Roster_Verification_raw, New_HH_Roster_raw, Labor_raw, Health_raw, 
#    Education_raw, Agriculture_raw, Basic_needs_raw, HH_Welfare_raw, Market_raw, 
#    Closing_Group_raw, Covid19_raw)

