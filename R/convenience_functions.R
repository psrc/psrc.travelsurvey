
#' Group commonly combined values
#'
#' Gets requested variable attributes--e.g. data type, associated weight name & priority
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


