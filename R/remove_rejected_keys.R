### Remove rejected keys -----------------------------------------------------------------
data <- data %>% 
  filter(KEY %notin% rejection_log$KEY)
Roster_Verification <- Roster_Verification %>%
  filter(KEY %notin% rejection_log$KEY)
New_HH_Roster <- New_HH_Roster %>%
  filter(KEY %notin% rejection_log$KEY)
Labor <- Labor %>%
  filter(KEY %notin% rejection_log$KEY)
Education <- Education %>%
  filter(KEY %notin% rejection_log$KEY)
Health <- Health %>%
  filter(KEY %notin% rejection_log$KEY)
Agriculture <- Agriculture %>%
  filter(KEY %notin% rejection_log$KEY)
Basic_needs <- Basic_needs %>%
  filter(KEY %notin% rejection_log$KEY)
HH_Welfare <- HH_Welfare %>%
  filter(KEY %notin% rejection_log$KEY)
Covid19 <- Covid19 %>%
  filter(KEY %notin% rejection_log$KEY)
Market <- Market %>%
  filter(KEY %notin% rejection_log$KEY)
Closing_Group <- Closing_Group %>%
  filter(KEY %notin% rejection_log$KEY)

