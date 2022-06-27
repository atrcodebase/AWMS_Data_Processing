`%notin%` <- Negate(`%in%`)


apply_log <- function (data, log, data_KEY = "KEY", log_columns = c(question = "question", 
                                                       old_value = "old_value", new_value = "new_value", KEY = "KEY")){
  for (rowi in 1:nrow(log)) {
    var_i <- log[[log_columns[["question"]]]][rowi]
    old_i <- log[[log_columns[["old_value"]]]][rowi]
    new_i <- log[[log_columns[["new_value"]]]][rowi]
    uuid_i <- log[[log_columns[["KEY"]]]][rowi]
    if (var_i %in% colnames(data)) {
      var_type <- class(data[[var_i]])
      if (var_type %in% "character") {
        data[data[[data_KEY]] %in% uuid_i, var_i] <- as.character(new_i)
      }
      else {
        data[data[[data_KEY]] %in% uuid_i, var_i] <- as.numeric(new_i)
      }
      print(paste("uuid:", uuid_i, "Old value: ", old_i, 
                  "changed to", new_i, "for", var_i))
    }
  }
  return(data)
}

#Checks if correction is applied correctly
verify_log_changes <- function(raw_data, cleaned_data, identifier){
  uuid <- vector()
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  
  for(col_name in colnames(cleaned_data) ){
    
    for(i in 1:length(cleaned_data[[col_name]])) {
      id <- cleaned_data[[identifier]][i]
      oldVal <- raw_data[[col_name]][raw_data[[identifier]] %in% id]
      newVal <- cleaned_data[[col_name]][i]
      
      if(col_name %in% c("start", "end")){
        oldVal <- as.character(oldVal)
        newVal <- as.character(newVal)
      }
      
      if(newVal %notin% oldVal){
        uuid <- c(uuid, id)
        question <- c(question, col_name)
        old_value <- c(old_value, oldVal)
        new_value <- c(new_value, newVal)
      }  
    }
  }
  log <- data.frame(uuid, question, old_value, new_value)
  return(log)
}
