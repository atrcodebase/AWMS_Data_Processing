# Filter Data ----------------------------------------------------------------------------
garbage_values <- c(
  "0",
  "،",
  ":،،،",
  "،،،،",
  "0")

# Labor
Labor <- Labor %>%
  filter(!is.na(Family_Member_Labor_English) & 
           Family_Member_Labor_English %notin% garbage_values)

# Education
Education <- Education %>%
  filter(!is.na(Family_Member_Education_English) & 
           Family_Member_Education_English %notin% garbage_values)

# Health
Health <- Health %>%
  filter(!is.na(Family_Member_Health_English) & 
           Family_Member_Health_English %notin% garbage_values)
