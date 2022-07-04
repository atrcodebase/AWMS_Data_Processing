### Remove rejected keys from Cleaned Approved data --------------------------------------
data_cleaned <- data_cleaned %>% 
  filter(KEY %notin% rejection_log$KEY)
Roster_Verification_cleaned <- Roster_Verification_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
New_HH_Roster_cleaned <- New_HH_Roster_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Labor_cleaned <- Labor_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Education_cleaned <- Education_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Health_cleaned <- Health_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Agriculture_cleaned <- Agriculture_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Basic_needs_cleaned <- Basic_needs_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
HH_Welfare_cleaned <- HH_Welfare_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Covid19_cleaned <- Covid19_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Market_cleaned <- Market_cleaned %>%
  filter(KEY %notin% rejection_log$KEY)
Closing_Group_cleaned <- Closing_Group_cleaned%>%
  filter(KEY %notin% rejection_log$KEY)

