# Function -------------------------------------------------------------------------------
relevancy_check <- function(data, tool_relevancy, sheet){
  # start.time <- Sys.time()
  tool_sub <- tool_relevancy %>% 
    filter(name %in% names(data)) 
  
  relevancy_log <- data.frame(KEY=NA,question=NA,q_val=NA,relevancy=NA,relevant_q=NA,
                              relev_val=NA)
  #loop through rows and tool
  for(i in 1:nrow(data)){
    
    prev_log_res <- NA
    final_match <- NA
    for(j in 1:nrow(tool_sub)){
      var_name <- tool_sub$name[j]
      
      data_q_val <- data[[var_name]][i]
      
      if(!is.na(data_q_val)){
        
        split_opr <- str_split(tool_sub$operator[j], " - ", simplify = T)
        # log_opr <- tool_sub$logical_opr[j]
        split_log_opr <- str_split(tool_sub$logical_opr[j], " - ", simplify = T)
        opr_order <- tool_sub$operator_order[j]
        relev_var <- tool_sub$q[j]
        relev_var_val <- tool_sub$val[j]
        rep <- tool_sub$rep[j]
        last_rep <- tool_sub$last_rep[j]
        #Data relevant question value
        dt_relev_q_val <- data[[relev_var]][i]
        
        #Using grepl for "Selected()" relevancies 
        if(split_opr[1] == "selected"){
          
          curr_log_res <- grepl(relev_var_val, dt_relev_q_val)
          
        } else if(grepl(" - ", relev_var_val)){
          
          curr_log_res <- multi_val(dt_relev_q_val, relev_var_val, split_opr, relev_var, split_val, split_log_opr)
          
        } else if(grepl("\\$\\{", relev_var_val)){
          
          relev_var_val <- str_extract(relev_var_val, "(?<=\\$\\{)(.*?)(?=\\})")
          curr_log_res <- match.fun(split_opr[1])(dt_relev_q_val, data[[relev_var_val]][i])
          
        } else {
          #in case there are 3 values
          if(length(split_opr) == 2 & rep > 1){
            curr_log_res <- match.fun(split_opr[2])(dt_relev_q_val, relev_var_val)
          } else {
            curr_log_res <- match.fun(split_opr[1])(dt_relev_q_val, relev_var_val)
          }
        }
        #For questions that has more than one relevancy
        if(rep > 1){
          final_match <- match.fun(split_log_opr[1])(prev_log_res, curr_log_res)
        } else {
          final_match <- curr_log_res
        }
        
        
        if(last_rep & !final_match){
          log <- c(data$KEY[i], var_name, data_q_val, tool_sub$relevance[j], tool_sub$q[j], dt_relev_q_val)
          relevancy_log <- rbind(relevancy_log, log)
        }
        #storing Current Logical Result for next check
        prev_log_res <- curr_log_res
      }
    }
  }
  relevancy_log <- relevancy_log[-1,] %>% 
    mutate(sheet_name = sheet)
  
  if(nrow(relevancy_log) == 0){
    print(paste0("No relevancy related issues found in: ", sheet))
  } else {
    print(paste0("Relevancy issues found in: ", sheet))
  }
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # print(time.taken)
  return(relevancy_log)
}
multi_val <- function(data, relev_var_val, split_opr, relev_var, split_val, split_log_opr){
  split_val <- str_split(relev_var_val, " - ", simplify = T)
  
  check1 <- match.fun(split_opr[1])(data, split_val[1])
  check2 <- match.fun(split_opr[2])(data, split_val[2])
  
  #in case there are 3 values
  if(length(split_val) == 3){
    curr_log_res <- match.fun(split_log_opr[1])(check1, check2)
    #re-calculting current logical result with the third value
    check3 <- match.fun(split_opr[3])(data, split_val[3])
    curr_log_res <- match.fun(split_log_opr[2])(curr_log_res, check3)
    
  } else {
    curr_log_res <- match.fun(split_log_opr[1])(check1, check2)
  }
  return(curr_log_res)
}

# Read Tool ------------------------------------------------------------------------------
tool_relevancy <- read_excel("input/tools/tool_relevancies.xlsx")

#apply function --------------------------------------------------------------------------
main_relevancy <- relevancy_check(data, tool_relevancy, "main")
Roster_Verification_relevancy <- relevancy_check(Roster_Verification, tool_relevancy, "Roster_Verification")
New_HH_Roster_relevancy <- relevancy_check(New_HH_Roster, tool_relevancy, "New_HH_Roster")
Labor_relevancy <- relevancy_check(Labor, tool_relevancy, "Labor")
Education_relevancy <- relevancy_check(Education, tool_relevancy, "Education")
Health_relevancy <-  relevancy_check(Health, tool_relevancy, "Health")
Agriculture_relevancy <- relevancy_check(Agriculture, tool_relevancy, "Agriculture")
Basic_needs_relevancy <- relevancy_check(Basic_needs, tool_relevancy, "Basic_needs")
HH_Welfare_relevancy <- relevancy_check(HH_Welfare, tool_relevancy, "HH_Welfare")
Covid19_relevancy <- relevancy_check(Covid19, tool_relevancy, "Covid19")
Market_relevancy <- relevancy_check(Market, tool_relevancy, "Market")
Closing_Group_relevancy <- relevancy_check(Closing_Group, tool_relevancy, sheet="Closing_Group")

relevancy_issues <- rbind(
  main_relevancy,
  Roster_Verification_relevancy,
  New_HH_Roster_relevancy,
  Labor_relevancy,
  Education_relevancy,
  Health_relevancy,
  Agriculture_relevancy,
  Basic_needs_relevancy,
  HH_Welfare_relevancy,
  Covid19_relevancy,
  Market_relevancy,
  Closing_Group_relevancy
)

#Export ----------------------------------------------------------------------------------
writexl::write_xlsx(relevancy_issues, "output/Tool_relevancy_issues.xlsx")
