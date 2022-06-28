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
  "Family_Member_Health_English",
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

### remove extra columns from Raw Data -----------------------------------------------
data_raw <- data_raw %>% select(-all_of(data_extra_columns))
Roster_Verification_raw <- Roster_Verification_raw %>% select(-new_uuid)
New_HH_Roster_raw <- New_HH_Roster_raw %>% select(-all_of(new_hh_roster_extra_columns))
Labor_raw <- Labor_raw %>% select(-all_of(labor_extra_columns))
Education_raw <- Education_raw %>% select(-all_of(education_extra_columns))
Health_raw <- Health_raw %>% select(-all_of(health_extra_columns))
Agriculture_raw <- Agriculture_raw %>% select(-new_uuid)
Basic_needs_raw <- Basic_needs_raw %>% select(-new_uuid)
HH_Welfare_raw <- HH_Welfare_raw %>% select(-new_uuid)
Covid19_raw <- Covid19_raw %>% select(-new_uuid)
Market_raw <- Market_raw %>% select(-new_uuid)
Closing_Group_raw <- Closing_Group_raw %>% select(-new_uuid)

### remove extra columns from Cleaned Data & Rename --------------------------------------
data <- data %>% 
  select(-all_of(data_extra_columns))

# Roster_Verification
Roster_Verification <- Roster_Verification %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_RV,
    Region_Final = Region_Final_RV,
    Province_Final = Province_Final_RV,
    Distric_Final = Distric_Final_RV,
    Respondent_Type_f = Respondent_Type_f_RV,
    userid = userid_RV)

# New_HH_Roster
New_HH_Roster <- New_HH_Roster %>% 
  select(-all_of(new_hh_roster_extra_columns)) %>% 
  rename(
    caseid = caseid_NR,
    Region_Final = Region_Final_NR,
    Province_Final = Province_Final_NR,
    Distric_Final = Distric_Final_NR,
    Respondent_Type_f = Respondent_Type_f_NR,
    userid = userid_NR)

# Labor
Labor <- Labor %>% 
  select(-all_of(labor_extra_columns)) %>% 
  rename(
    caseid = caseid_L,
    Region_Final = Region_Final_L,
    Province_Final = Province_Final_L,
    Distric_Final = Distric_Final_L,
    Respondent_Type_f = Respondent_Type_f_L,
    userid = userid_L)

# Education
Education <- Education %>% 
  select(-all_of(education_extra_columns)) %>% 
  rename(
    caseid = caseid_ED,
    Region_Final = Region_Final_ED,
    Province_Final = Province_Final_ED,
    Distric_Final = Distric_Final_ED,
    Respondent_Type_f = Respondent_Type_f_ED,
    userid = userid_ED)

# Health
Health <- Health %>% 
  select(-all_of(health_extra_columns)) %>% 
  rename(
    caseid = caseid_HE,
    Region_Final = Region_Final_HE,
    Province_Final = Province_Final_HE,
    Distric_Final = Distric_Final_HE,
    Respondent_Type_f = Respondent_Type_f_HE,
    userid = userid_HE)

# Agriculture
Agriculture <- Agriculture %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_A,
    Region_Final = Region_Final_A,
    Province_Final = Province_Final_A,
    Distric_Final = Distric_Final_A,
    Respondent_Type_f = Respondent_Type_f_A,
    userid = userid_A)

# Basic_needs
Basic_needs <- Basic_needs %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_B,
    Region_Final = Region_Final_B,
    Province_Final = Province_Final_B,
    Distric_Final = Distric_Final_B,
    Respondent_Type_f = Respondent_Type_f_B,
    userid = userid_B)

# HH_Welfare
HH_Welfare <- HH_Welfare %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_W,
    Region_Final = Region_Final_W,
    Province_Final = Province_Final_W,
    Distric_Final = Distric_Final_W,
    Respondent_Type_f = Respondent_Type_f_W,
    userid = userid_W)

# Covid19
Covid19 <- Covid19 %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_C,
    Region_Final = Region_Final_C,
    Province_Final = Province_Final_C,
    Distric_Final = Distric_Final_C,
    Respondent_Type_f = Respondent_Type_f_C,
    userid = userid_C)

# Market
Market <- Market %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_M,
    Region_Final = Region_Final_M,
    Province_Final = Province_Final_M,
    Distric_Final = Distric_Final_M,
    Respondent_Type_f = Respondent_Type_f_M,
    userid = userid_M)

# Closing_Group
Closing_Group <- Closing_Group %>%
  select(-new_uuid) %>% 
  rename(
    caseid = caseid_CL,
    Region_Final = Region_Final_CL,
    Province_Final = Province_Final_CL,
    Distric_Final = Distric_Final_CL,
    Respondent_Type_f = Respondent_Type_f_CL,
    userid = userid_CL)

# remove extra objects -------------------------------------------------------------------
rm(data_extra_columns, new_hh_roster_extra_columns, labor_extra_columns, education_extra_columns, health_extra_columns)
