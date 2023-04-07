#' Summarize home delivery data consisently

#' @param day_df data frame with the day table observations
#' @return day_df_update day table with new delivery columns for summarization
#' @import dplyr
#' @export
summarize_delivery <- function(day_df){
   day_df_update <- day_df %>%
    mutate(delivery_food_all= dplyr::case_when((pernum==1 & is.na(delivery_food_freq) & is.na(deliver_food)) ~ 'No HH Response',
                                        # pernum == 1 removes households where multiple members answered the question
                                        (pernum>1) ~ 'Not Person One, not the responder',
                                        delivery_food_freq == "0 (none)"  ~ 'No Delivery',
                                        deliver_food=='No' ~ 'No Delivery',
                                        
                                        TRUE ~ 'Delivery Received'))%>%
    mutate(delivery_pkgs_all= dplyr::case_when((pernum==1 & is.na(delivery_pkgs_freq) & is.na(deliver_package)) ~ 'No HH Response',
                                        (pernum>1) ~ 'Not Person One, not the responder',
                                        deliver_package=='No' ~ 'No Delivery',
                                        delivery_pkgs_freq == "0 (none)"  ~ 'No Delivery',
                                        TRUE ~ 'Delivery Received'))%>%
    mutate(delivery_grocery_all=dplyr::case_when((pernum==1 & is.na(delivery_grocery_freq) & is.na(deliver_grocery)) ~ 'No HH Response',
                                          (pernum>1) ~ 'Not Person One, not the responder',
                                          delivery_grocery_freq == "0 (none)"  ~ 'No Delivery',
                                          deliver_grocery=='No' ~ 'No Delivery',
                                          TRUE ~ 'Delivery Received'))%>%
    mutate(delivery_work_all= dplyr::case_when((pernum==1 & is.na(delivery_work_freq) & is.na(deliver_work)) ~ 'No HH Response',
                                        (pernum>1) ~ 'Not Person One, not the responder',
                                        deliver_work =='No' ~ 'No Delivery',
                                        delivery_work_freq == "0 (none)"  ~ 'No Delivery',
                                        TRUE ~ 'Delivery Received'))
  return(day_df_update)
  
}



#' Group commonly combined values
#' @param df disaggregate survey observation data frame result of get_hhts
#' @param var the variable for which you would like to group values based on the value_metadata table
#' @return df_val_group: a data frame with the survey observations with a column added for the grouped variable
#'         named yourvariablename_group
#' 
#' @import dplyr
#' @import DBI
#' @export
group_vals<- function(df, var, survey_yr){
  # code to get values associated with var
  sql_code <- paste0("SELECT variable, value_text, value_group_1 
                     FROM HHSurvey.v_value_metadata 
                     WHERE HHSurvey.v_value_metadata.variable='", var, "' AND survey_year = '", survey_yr, "';")
  var_values <- DBI::dbGetQuery(db_connection, DBI::SQL(sql_code))
  df_w_vals<-dplyr::left_join(df, var_values, by=setNames('value_text', var))
  df_val_group<-df_w_vals%>% dplyr::mutate(!!(paste0(sym(var), '_group')):=value_group_1)
  return(df_val_group)
}