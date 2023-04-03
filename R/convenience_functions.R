
value_groups<-read.csv(hh_survey_value_groups) (or put in elmer)

# run this after get_hhts, before stat functions
group_vals<- function(tbl, value_groups, variable_name){
  #do commonly needed groupings
  variable_val_groups<-value_groups%>%filter(variable== get(variable_name))
  tbl<-left_join(tbl, value_groups, by.x=(get(variable_name), by.y=value) )
  tbl<-tbl%>% rename(group= paste(get(variable_name), '_group')
  tbl<-tbl_grouped
  return(tbl_grouped)
}



code_sov_mode<-
  
  
summarize_deliveries<-
  
  
  whatever it is i was doing here
  
  mover_sum<- list()
mover_sum <- lapply(vars_to_summarize, FUN=function(x) hhts_count(df=hhs_2021,group_vars=c(x))) %>%
  lapply(FUN=function(y){mutate(y, "var_name"=colnames(y)[2])}) %>% data.table::rbindlist(use.names=FALSE) %>% 
  rename(var_value=colnames(.)[2]) %>% relocate(var_name)

mover_shares_2021<-mover_sum%>% select(var_name, var_value, share,survey, share_moe)%>%filter(var_value=='Very important')