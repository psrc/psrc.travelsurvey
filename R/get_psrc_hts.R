#' @importFrom magrittr %<>% %>%
NULL

globalVariables(c(":=", "!!", ".", "enquos","..."))
`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]
stuff <- function(x){unique(x) %>% paste(collapse=",")}

#' Retrieve PSRC travel survey data
#'
#' @param survey_years vector of 4-digit survey years. Specify any combinations explicitly (e.g. 1719 for combined 2017/2019)
#' @param survey_vars desired variables (in addition to id fields, which are returned by default)
#' @return surveyDataTools data object, i.e. named list 
#' @author Michael Jensen
#' @import data.table
#' @importFrom dplyr case_when
#' @importFrom rlang is_empty set_names
#' @importFrom labelled var_label
#'
#' @export
get_psrc_hts <- function(survey_years=c(2017,2019,2021,2023,2025), survey_vars){
  tblnames <- tblname <- variable <- hts_split_vars <- hts_query_elmer <-  NULL # For CMD check
  hh_id <- NULL # For CMD check
  tblnames <- c("household","person","day","trip","vehicle")
  
  # Helper function; identifies which tables the desired variables are in, using codebook
  hts_split_vars <- function(tblname, survey_vars){
    rs <- copy(variable_list) %>% setnames("hh","household") %>%
    .[get(tblname)==1 & variable %in% c("sample_segment", "survey_year", survey_vars), variable] %>% unlist()
  }
  # Helper function; queries Elmer for specified variables
  hts_query_elmer <- function(tblname, tblvars){
    if(!rlang::is_empty(unlist(tblvars))){
      if(tblname=="vehicle"){
        id_vars <- c("household_id","vehicle_id")
        wgt_filter <- ""
      }else{
        id_vars <- paste0(tblnames[1:(which(tblnames==tblname))], "_id")
        wgt_filter <- paste0(" AND ", hts_wgt_var(tblname), ">0;")
      }
      tblvars %<>% .[. %not_in% id_vars]
      sql_vars <- paste(c(paste0("CAST(", id_vars, " AS nvarchar) AS ", id_vars, collapse=", "), 
                        paste(unlist(tblvars), collapse=", ")), collapse=", ")
      if(tblname!="vehicle"){
        sql_vars %<>% paste(", ", hts_wgt_var(tblname))
      }
      sql <- paste0("SELECT ", sql_vars, 
                    " FROM HHSurvey.v_", tblname, "s", 
                    " WHERE survey_year IN(", stuff(survey_years),")",
                    wgt_filter)
      rs <- psrcelmer::get_query(sql) %>% setDT() %>% 
        setnames("household_id","hh_id") %>%                                    # travelSurveyTools uses "hh_id"
        psrc_survey_recode_na() %>% 
        psrc_survey_to_factor() %>% 
        psrc_survey_desc_labels()
      return(rs)
    }else{
     return(NULL)
    }
  }
  # Main workflow
  split_vars <- sapply(tblnames, hts_split_vars, survey_vars, simplify = FALSE, USE.NAMES = TRUE)
  hts_data <- mapply(hts_query_elmer, names(split_vars), split_vars, USE.NAMES = TRUE) %>%
  rlang::set_names(c("hh","person","day","trip","vehicle"))                     # travelSurveyTools uses "hh"
  valid_hh_ids <- hts_data$hh$hh_id                                            # Drop orphan records from child tables
  for(tbl in c("person","day","trip","vehicle")){                               # (e.g. trips from zero-weight households)
    if(!is.null(hts_data[[tbl]])){
      hts_data[[tbl]] <- hts_data[[tbl]][hh_id %in% valid_hh_ids]
    }
  }
  return(hts_data)
}

#' Return weight variable name
#' 
#' @param tblname table name
#' @return weight variable name
#' @import data.table
#'
hts_wgt_var <- function(tblname){
  wgt_var <- NULL
  if(tblname!="vehicle"){
    wgtname <- dplyr::case_when(tblname=="household" ~"hh_weight", 
                                TRUE ~ paste0(tblname, "_weight"))
  }
}

#' Search PSRC travel survey variable definitions
#'
#' Look for a variable name using a search term
#' @param regex search term
#' @return data.table of filtered variable attributes
#' @author Michael Jensen
#' @import data.table
#'
#' @export
psrc_hts_varsearch <- function(regex){
  variable_list <- variable <- description <- NULL
  rs <- psrc.travelsurvey:::variable_list %>%
    .[(grepl(regex, description, ignore.case=TRUE)|
         grepl(regex, variable, ignore.case=TRUE)), 
      .(variable, description)] %>%
    unique()
  return(rs)
}
