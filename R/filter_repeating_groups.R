### filter repeating group sheets based on the main sheet --------------------------------
Roster_Verification <- Roster_Verification %>%
  filter(PARENT_KEY %in% data$KEY)
New_HH_Roster <- New_HH_Roster %>%
  filter(PARENT_KEY %in% data$KEY)
Labor <- Labor %>%
  filter(PARENT_KEY %in% data$KEY)
Education <- Education %>%
  filter(PARENT_KEY %in% data$KEY)
Health <- Health %>%
  filter(PARENT_KEY %in% data$KEY)
Agriculture <- Agriculture %>%
  filter(PARENT_KEY %in% data$KEY)
Basic_needs <- Basic_needs %>%
  filter(PARENT_KEY %in% data$KEY)
HH_Welfare <- HH_Welfare %>%
  filter(PARENT_KEY %in% data$KEY)
Covid19 <- Covid19 %>%
  filter(PARENT_KEY %in% data$KEY)
Market <- Market %>%
  filter(PARENT_KEY %in% data$KEY)
Closing_Group <- Closing_Group %>%
  filter(PARENT_KEY %in% data$KEY)

# remove extra objects -------------------------------------------------------------------
