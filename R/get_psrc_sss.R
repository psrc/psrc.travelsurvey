#' Retrieve PSRC transportation safety & security survey data
#'
#' @param survey_vars desired variables (in addition to person and household ids plus `person_weight`, which are returned by default)
#' @return data.table of queried safety survey responses
#' @author Michael Jensen
#' @import data.table
#'
#' @export
get_psrc_sss <- function(survey_vars = NULL){
  query_all <- "*" %in% survey_vars
  query_vars <- if(query_all){
    "*"
  }else{
    unique(c("person_id", "hhid", "sample_segment", survey_vars, "person_weight"))
  }
  sql <- paste0(
    "SELECT ",
    paste(query_vars, collapse = ", "),
    " FROM Elmer.safety_survey.v_responses_2025",
    " WHERE person_weight>0;"
  )
  rs <- psrcelmer::get_query(sql) %>% setDT()
  setnames(rs, "hhid", "hh_id")
  rs %<>%
    psrc_survey_recode_na() %>%
    psrc_survey_to_factor() %>%
    psrc_survey_desc_labels()
  rs
}