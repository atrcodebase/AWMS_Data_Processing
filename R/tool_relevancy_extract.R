# if(!require(koboquest)) install_github("mabafaba/koboquest")


tool <- read_excel("input/tools/AWMS R2 - Final DCT.xlsx")
tool <- tool %>% filter(!is.na(relevance) & !grepl("web", relevance)) %>% 
  select(type, name, relevance) %>% 
  mutate(logical_opr=NA_character_, 
         operator=NA_character_,
         operator_order= NA_integer_, .after=relevance)
# unique(tool$relevance)

tool <- tool %>% 
  filter(grepl("selected", relevance))
tool %>% View

tool <- tool %>% 
  filter(!grepl("selected", relevance) & !grepl("\\(|\\)", relevance))

tool <- tool %>% 
  filter(!grepl("selected", relevance) & grepl("\\(|\\)", relevance) & !grepl("web", relevance))
tool %>% View

# tool %>% 
#   filter(!(grepl(" and ", relevance) | grepl("  or ", relevance)) & !grepl("selected", relevance)) %>% 
#   mutate(logical_opr=NA_character_, .after=relevance)
# Regex ----------------------------------------------------------------------------------
# Logical Operators
log_opr_reg <- "(?<=\\s)and(?<!\\s)|(?<=\\s)or(?<!\\s)"
# Groups
group_reg <- "(?<=\\()(.*?)(?=\\))"
# Question
question_reg <- "(?<!\\=\\$\\{|<\\s\\$\\{|>=\\s\\$\\{)(?<=\\$\\{)(.*?)(?=\\})"
# Value
selected_val_reg <- "((?<=,[\"'])|(?<=,\\s[\"']))(.*?)(?=[\"'])"
value_reg <- '(?<=\\=|<\\s|>=\\s)(.+?)(?!\\S)'
# Operator
opr_reg <- "<=|>=|!=|=|>|<"


extract_relevance <-  function(tool, matched_group, val, opr, log_opr, selected=F) {
  prev_col = ""
  for(j in 1:length(matched_group[[1]])){
    col_name = question[[1]][j]
    if(col_name %notin% names(tool)){
      tool[[col_name]] <- NA_character_
    }
    
    if(col_name %in% prev_col){
      tool[[col_name]][i] <- paste0(tool[[col_name]][i], " - ", val[[1]][j])
      tool[["operator"]][i] <- paste0(tool[["operator"]][i], " - ", opr[[1]][j])
      tool[["logical_opr"]][i] <- paste0(tool[["logical_opr"]][i], " - ", log_opr[[1]][j])
      tool[["operator_order"]][i] <- "first"
    } else {
      tool[[col_name]][i] <- val[[1]][j]
      
      if(j>1 & !selected){
        if(opr[[1]][j] != opr[[1]][j-1]){
          tool[["operator"]][i] <- paste0(tool[["operator"]][i], " - ", opr[[1]][j])
        }
      } else if(selected) {
        tool[["operator"]][i] <- opr
      } else {
        tool[["operator"]][i] <- opr[[1]][j]
      }
      
      tool[["logical_opr"]][i] <- log_opr[[1]][j]
      tool[["operator_order"]][i] <- "second"
    }
    prev_col <- col_name
  }
  return(tool)
}

#Code ------------------------------------------------------------------------------------

for(i in 1:nrow(tool)){
  relevance = tool$relevance[i]
  
  if(grepl("selected", relevance)){
    
    matched_group = str_extract_all(relevance, group_reg)
    log_opr = str_extract(relevance, log_opr_reg)
    question = str_extract_all(relevance, question_reg)
    val = str_extract_all(relevance, selected_val_reg)
    opr = list("=")
    tool <- extract_relevance(tool, matched_group, val, opr, log_opr, selected = T)
    # prev_col = ""
    # for(j in 1:length(question[[1]])){
    #   col_name = question[[1]][j]
    #   if(col_name %notin% names(tool)){
    #     tool[[col_name]] <- NA_character_
    #   }
    #   
    #   if(col_name %in% prev_col){
    #     tool[[col_name]][i] <- paste0(tool[[col_name]][i], " - ", val[[1]][j])
    #     tool[["operator"]][i] <- paste0(tool[["operator"]][i], " - ", opr[[1]][j])
    #     tool[["logical_opr"]][i] <- log_opr
    #     tool[["operator_order"]][i] <- "first"
    #   } else {
    #     tool[[col_name]][i] <- val[[1]][j]
    #     if(j>1 & group != "selected"){
    #       if(opr[[1]][j] != opr[[1]][j-1]){
    #         tool[["operator"]][i] <- paste0(tool[["operator"]][i], " - ", opr[[1]][j])
    #       }
    #     } else {
    #       tool[["operator"]][i] <- opr[[1]][j]
    #     }
    #     tool[["logical_opr"]][i] <- log_opr[[1]][j]
    #     tool[["operator_order"]][i] <- "second"
    #   }
    #   prev_col = col_name
    #   
    # }
    
  } else {
    
    if(!grepl("\\(|\\)", relevance)){
      question = str_extract_all(relevance, question_reg)
      val = str_extract_all(relevance, value_reg)
      opr = str_extract_all(relevance, opr_reg)
      log_opr = str_extract_all(relevance, log_opr_reg)
      
      tool <- extract_relevance(tool, question, val, opr, log_opr)
    } else {
      
      relevance = tool$relevance[i]
      
      question = str_extract_all(relevance, question_reg)
      val = str_extract_all(relevance, value_reg)
      opr = str_extract_all(relevance, opr_reg)
      log_opr = str_extract_all(relevance, log_opr_reg)
      
      tool <- extract_relevance(tool, question, val, opr, log_opr)
      
    } 
    # else {
    #   
    #   matched_group = str_extract_all(relevance, group_reg)
    #   col_name = str_extract(matched_group[[1]][1], question_reg)
    #   opr = str_extract(matched_group[[1]][1], opr_reg)
    #   log_opr = str_extract(relevance, log_opr_reg)
    #   val = str_extract(matched_group[[1]][1], value_reg)
    #   
    #   if(col_name %notin% names(tool)){
    #     tool[[col_name]] <- NA_character_
    #   }
    #   
    #   tool[[col_name]][i] <- val
    #   tool[["operator"]][i] <- opr
    #   tool[["logical_opr"]][i] <- log_opr
    #   tool[["operator_order"]][i] <- "second"
    #   
    #   q = str_extract_all(matched_group[[1]][2], question_reg)
    #   opr = str_extract_all(matched_group[[1]][2], opr_reg)
    #   log_opr = str_extract(matched_group[[1]][2], log_opr_reg)
    #   val = str_extract_all(matched_group[[1]][2], value_reg)
    #   
    #   
    # }
  }
}
# if(i %notin% c(2, 3, 4, 148))
#Longer


tool_longer <- tool %>% 
  pivot_longer(-c(type:operator_order), names_to = "q", values_to = "val") %>%
  filter(!is.na(val)) %>% 
  mutate(across(c(logical_opr, operator), function(x) str_replace_all(x, " - NA|NA - ", "")))

tool_longer %>% View

writexl::write_xlsx(tool_longer, "input/tools/tool_relevancies.xlsx")
#Regex ----------------------------------------------------------------------------------
# Logical Operators
# log_opr_reg <- "(?<=\\s)(.*?)(?=\\s)"
# log_opr_reg <- "(?<=\\)\\s)(.*?)(?=\\s)"
# log_opr_reg <- "AND|OR|and|or|XOR|&&|<=|<|>|>=|!=|==|&|OR*|!|[||]{2}|\\|"
# log_opr_reg <- "AND|OR|and|or"
log_opr_reg <- "(?<=\\s)and(?<!\\s)|(?<=\\s)or(?<!\\s)"
# Groups
group_reg <- "(?<=\\()(.*?)(?=\\))"

# Question
# question_reg <- "(?<=\\{)(.*?)(?=\\})"
# question_reg <- "((?<=^\\$\\{)|((?<=\\s\\$\\{)))(.*?)(?=\\})"
question_reg <- "(?<!\\=\\$\\{|<\\s\\$\\{|>=\\s\\$\\{)(?<=\\$\\{)(.*?)(?=\\})"

# Value
# selected_val_reg <- '(?<=\\")(.*?)(?=\\")'
selected_val_reg <- "((?<=,[\"'])|(?<=,\\s[\"']))(.*?)(?=[\"'])"
value_reg <- '(?<=\\=|<\\s|>=\\s)(.+?)(?!\\S)'
# value_reg <- "((?<=\\=)|(?<=\\s\\=))(.*?)(?!\\S)"

# Operator
# opr_reg <- "(?<=\\}|\\s).?[=]"
opr_reg <- "<=|>=|!=|=|>|<"

#Test ------------------------------------------------------------------------------------
# tool %>% mutate("{test[[1]][1]}" := "t")

str <- "(${Intro_01_Age}>=14) and (${Respondent_Type}=1 or ${Respondent_Type}=25)"
test <- str_extract_all(str, group_reg)
test
### 1
#q
str_extract_all(str, question_reg)
#logical operator
str_extract(str, log_opr_reg)
#operator
str_extract(str, opr_reg)
#val
str_extract(test[[1]][1], value_reg)
### 2
#q
str_extract_all(test[[1]][2], question_reg)
#logical operator
str_extract(test[[1]][2], log_opr_reg)
#operator
str_extract_all(test[[1]][2], opr_reg)
#val
str_extract_all(test[[1]][2], value_reg)

str <- "${caseid} = null"
str_extract_all(str, value_reg)

#work on this string
str <- '${Intro_01}!="1" and (${Not_reached_out_to_respondent}=1 or ${Reached_out_to_respondent}=1)'
str_extract_all(str, group_reg)

test <- str_extract_all(str, group_reg)
test
### 1
#q
str_extract_all(str, question_reg)
#operator
str_extract_all(str, opr_reg)
#val
str_extract_all(str, value_reg)
### 2
#q
str_extract_all(test[[1]][2], question_reg)
#operator
str_extract_all(test[[1]][2], opr_reg)
#val
str_extract_all(test[[1]][2], '(?<=\\=)(.+?)')


#And Or ------------------------------------------------
str <- "${Am_I_Speaking_to_Household_Head}=12 or ${Can_I_Speack_to_Household_Head}=14"
#q
str_extract_all(str, question_reg)
#logical operator
str_extract(str, log_opr_reg)
#operator
str_extract_all(str, opr_reg)
#val
str_extract_all(str, value_reg)

str <- '${Name_Roster}=${resp_name_f} and ${Reached_out_to_respondent}=1'
#q
str_extract_all(str, question_reg)
#logical operator
str_extract(str, log_opr_reg)
#operator
str_extract_all(str, opr_reg)
#val
str_extract_all(str, value_reg)

str <- "${Name_Roster}=${resp_name_f} and ${Reached_out_to_respondent}=1"
str <- "${call_num} < ${stop_at}"
#Selected one -----------------------------------------------------
str <- 'selected(${Interview_language},"7777")'
str_extract_all(str, question_reg)
str_extract_all(str, selected_val_reg)

#Selected multiple
str <- 'selected(${A01_Agri},"1") or selected(${A01_garden},"1")'
test <- str_extract_all(str, group_reg)
test
#logical operator
str_extract(str, log_opr_reg)
#1
str_extract(test[[1]][1], question_reg)
str_extract(test[[1]][1], selected_val_reg)
#2
str_extract(test[[1]][2], question_reg)
str_extract(test[[1]][2], selected_val_reg)

str <- "(selected(${phone_response}, '45') or selected(${phone_response}, '55'))"
str_extract_all(str, selected_val_reg)
#logical operator
str_extract_all(str, log_opr_reg)

# 
# tool %>% 
#   mutate(q = case_when(
#     !grepl("selected", relevance) ~ str_extract_all(relevance, "(?<=\\{)(.*?)(?=\\})"),
#     TRUE ~ NA_character_
#   )) %>% View


#