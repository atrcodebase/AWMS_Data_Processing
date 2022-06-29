`%notin%` <- Negate(`%in%`)

compare_dt <- function (df1, df2, unique_id_df1, unique_id_df2, compare_all = TRUE) 
{
  if (compare_all == FALSE) {
    df1 <- df1[, colnames(df1) %in% colnames(df2)]
    df2 <- df2[, colnames(df2) %in% colnames(df1)]
  }
  if ("KEY" %in% colnames(df1) && unique_id_df1 != "KEY") {
    df1 <- df1 %>% rename(key = KEY)
  }
  df1 <- df1 %>% select(KEY = all_of(unique_id_df1), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_1") %>% mutate(value_1 = str_squish(value_1))
  if ("KEY" %in% colnames(df2) && unique_id_df2 != "KEY") {
    df2 <- df2 %>% rename(key = KEY)
  }
  df2 <- df2 %>% select(KEY = all_of(unique_id_df2), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_2") %>% mutate(value_2 = str_squish(value_2))
  df_both <- full_join(df1, df2, by = c("KEY", "name"))
  diff <- df_both %>% filter((value_1 != value_2) | (is.na(value_1) & 
                                                       !is.na(value_2)) | (!is.na(value_1) & is.na(value_2))) %>% 
    rename(question = name, old_value = value_1, new_value = value_2) %>% 
    mutate(question = ifelse(question == "key", "KEY", question))
  if (nrow(diff) == 0) {
    paste0("No difference in df1 and df2")
    return(diff)
  }
  else {
    return(diff)
  }
}
