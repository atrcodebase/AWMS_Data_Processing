### filter repeating group sheets based on the main sheet --------------------------------
Roster_Verification_cleaned_filtered <- Roster_Verification_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
New_HH_Roster_cleaned_filtered <- New_HH_Roster_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Labor_cleaned_filtered <- Labor_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Education_cleaned_filtered <- Education_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Health_cleaned_filtered <- Health_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Agriculture_cleaned_filtered <- Agriculture_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Basic_needs_cleaned_filtered <- Basic_needs_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
HH_Welfare_cleaned_filtered <- HH_Welfare_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Covid19_cleaned_filtered <- Covid19_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Market_cleaned_filtered <- Market_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
Closing_Group_cleaned_filtered <- Closing_Group_cleaned_filtered %>%
  filter(PARENT_KEY %in% data_cleaned_filtered$KEY)
