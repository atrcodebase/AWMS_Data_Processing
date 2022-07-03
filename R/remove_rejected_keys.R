### Remove rejected keys from Cleaned Approved data --------------------------------------
data_cleaned_filtered <- data_cleaned_filtered %>% 
  filter(KEY %notin% rejection_log$KEY)
Roster_Verification_cleaned_filtered <- Roster_Verification_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
New_HH_Roster_cleaned_filtered <- New_HH_Roster_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Labor_cleaned_filtered <- Labor_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Education_cleaned_filtered <- Education_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Health_cleaned_filtered <- Health_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Agriculture_cleaned_filtered <- Agriculture_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Basic_needs_cleaned_filtered <- Basic_needs_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
HH_Welfare_cleaned_filtered <- HH_Welfare_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Covid19_cleaned_filtered <- Covid19_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Market_cleaned_filtered <- Market_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Closing_Group_cleaned_filtered <- Closing_Group_cleaned%>%
  filter(KEY %notin% rejection_log$KEY)

