### extra columns ------------------------------------------------------------------------
data_extra_columns <- c(
  "deviceid",
  "subscriberid",
  "simid",
  "devicephonenum",
  "instance_time",
  "audio_audit_full",
  "pct_conversation",
  "audio_audit_introduction",
  "phone_call_log",
  "phone_call_duration",
  "collect_phone_app",
  "device_info",
  "pub_to_users",
  "call_datetime",
  "call_time",
  "new_sortby",
  "num_calls",
  "call_num",
  "stop_at",
  "review_status",
  "needs_review",
  "Respondent_Type_dari",
  "Respondent_Type_pashto",
  "intro_part",
  "intro_partd",
  "intro_partp",
  "resp_name_f",
  "resp_name_fd",
  "resp_name_fp",
  "calltime",
  "Payment",
  "randomization",
  "Credit",
  "info_respondent",
  "call_respondent",
  "new",
  "new_Dari",
  "new_Pashto",
  "res_dari",
  "res_pashto",
  "now_c_dari",
  "now_c_pashto",
  "formdef_version",
  "review_quality",
  "review_comments",
  "review_corrections"
)
new_hh_roster_extra_columns <- c(
  "Name_Wording_English",
  "Name_Wording_Dari",
  "Name_Wording_Pashto",
  "Relationship_Wording_English",
  "Relationship_Wording_Dari",
  "Relationship_Wording_Pashto",
  "Age_Wording_English",
  "Age_Wording_Dari",
  "Age_Wording_Pashto",
  "Gender_Wording_English",
  "Gender_Wording_Dari",
  "Gender_Wording_Pashto",
  "Read_Write_Wording_English",
  "Read_Write_Wording_Dari",
  "Read_Write_Wording_Pashto",
  "Basic_Calculations_Wording_English",
  "Basic_Calculations_Wording_Dari",
  "Basic_Calculations_Wording_Pashto",
  "new_uuid"
)
labor_extra_columns <- c(
  "Family_Member_Labor_Dari",
  "Family_Member_Labor_Pashto",
  "Labor_Resp_English",
  "Labor_Resp_Dari",
  "Labor_Resp_Pashto",
  "Labor_Wording1_English",
  "Labor_Wording2_English",
  "Labor_Wording3_English",
  "Labor_Wording4_English",
  "Labor_Wording1_Dari",
  "Labor_Wording2_Dari",
  "Labor_Wording3_Dari",
  "Labor_Wording4_Dari",
  "Labor_Wording1_Pashto",
  "Labor_Wording2_Pashto",
  "Labor_Wording3_Pashto",
  "new_uuid"
)
education_extra_columns <- c(
  "Family_Member_Education_Dari",
  "Family_Member_Education_Pashto",
  "Education_Wording2_English",
  "Education_Wording3_English",
  "Education_Wording2_Dari",
  "Education_Wording3_Dari",
  "Education_Wording4_Dari",
  "Education_Wording2_Pashto",
  "Education_Wording3_Pashot",
  "new_uuid"
)
health_extra_columns <- c(
  "Family_Member_Health_Dari",
  "Family_Member_Health_Pashto",
  "Health_Wording4_English",
  "Health_Wording5_English",
  "Health_Wording4_Dari",
  "Health_Wording5_Dari",
  "Health_Wording5_Pashto",
  "Health_Wording4_Pashto",
  "new_uuid"
)

### remove extra columns -----------------------------------------------------------------
data <- data %>% 
  select(-all_of(data_extra_columns))
# Roster_Verification
Roster_Verification <- Roster_Verification %>%
  select(-new_uuid)
# New_HH_Roster
New_HH_Roster <- New_HH_Roster %>% 
  select(-all_of(new_hh_roster_extra_columns))
# Labor
Labor <- Labor %>% 
  select(-all_of(labor_extra_columns))
# Education
Education <- Education %>% 
  select(-all_of(education_extra_columns))
# Health
Health <- Health %>% 
  select(-all_of(health_extra_columns))
# Agriculture
Agriculture <- Agriculture %>%
  select(-new_uuid)
# Basic_needs
Basic_needs <- Basic_needs %>%
  select(-new_uuid)
# HH_Welfare
HH_Welfare <- HH_Welfare %>%
  select(-new_uuid)
# Covid19
Covid19 <- Covid19 %>%
  select(-new_uuid)
# Market
Market <- Market %>%
  select(-new_uuid)
# Closing_Group
Closing_Group <- Closing_Group %>%
  select(-new_uuid)

# remove extra objects -------------------------------------------------------------------
rm(data_extra_columns, new_hh_roster_extra_columns, labor_extra_columns, education_extra_columns, health_extra_columns)
