#' @importFrom magrittr %<>% %>%
NULL

globalVariables(c(":=", "!!", ".", "enquos","..."))
`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]
stuff <- function(x){unique(x) %>% paste(collapse=",")}

#' Retrieve PSRC travel survey data
#'
#' @param survey_years vector with values that may include 2017, 2019, 2021, 2023
#' @param survey_vars desired variables (in addition to id fields, which are returned by default)
#' @return surveyDataTools data object, i.e. named list 
#' @author Michael Jensen
#' @import data.table
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace_all
#' @importFrom rlang is_empty
#' @importFrom travelSurveyTools factorize_df
#'
#' @export
get_psrc_hts <- function(survey_years, survey_vars){
  tblnames <- tblname <- hts_split_vars <- hts_query_elmer <- variable <- data_type <- NULL # For CMD check
  tblnames <- c("household","person","day","trip","vehicle")
  # Helper function; identifies which tables the desired variables are in, using codebook
  hts_split_vars <- function(tblname, survey_vars){
    rs <- init_variable_list[get(tblname)==1 & variable %in% (survey_vars), variable] %>% unlist()
  }
  # Helper function; queries Elmer for specified variables
  hts_query_elmer <- function(tblname, tblvars){
    if(!rlang::is_empty(unlist(tblvars))){
      id_vars <- if(tblname=="vehicle"){
        c("household_id","vehicle_id")
      }else{
        paste0(tblnames[1:(which(tblnames==tblname))], "_id")
      }
      sql_vars <- paste(c(paste0("CAST(", id_vars, " AS nvarchar) AS ", id_vars, collapse=", "), 
                        paste(unlist(tblvars), collapse=", ")), collapse=", ")
      if(tblname!="vehicle"){
        sql_vars %<>% paste(", ", dplyr::case_when(tblname=="household" ~"hh_weight", 
                                                   TRUE ~ paste0(tblname, "_weight")))
      }
      sql <- paste0("SELECT ", sql_vars, 
                  " FROM HHSurvey.v_", tblname, "s_labels", 
                  " WHERE survey_year IN(", stuff(survey_years),");")
      cols_to_factor <- init_variable_list[data_type=="integer/categorical", variable] %>% 
        unlist() %>% paste(collapse="|") %>% grep(tblvars, value = TRUE)
      rs <- psrcelmer::get_query(sql) %>% setDT() %>% psrc_hts_recode_na() #%>% travelSurveyTools::factorize_df(init_value_labels)
      return(rs)
    }else{
     return(NULL)
    }
  }
  split_vars <- sapply(tblnames, hts_split_vars, survey_vars, simplify = FALSE, USE.NAMES = TRUE)
  hts_data <- mapply(hts_query_elmer, names(split_vars), split_vars, USE.NAMES = TRUE)
}

#' Recode missing values to NA
#' 
#' @param dt the data.table
#' @return the data.table with missing values recoded NA
#' @author Michael Jensen
#' @import data.table
#'
psrc_hts_recode_na <- function(dt){
  na_codes <- c("^Missing: Technical Error$",
                "^Missing: Non-response$",
                "^Missing: Skip Logic$",
                "^Children or missing$",
                "^$") %>%
    unique() %>% paste0(collapse="|")
  for(col in colnames(dt))
    set(dt, i=grep(na_codes, dt[[col]]), j=col, value=NA)
  return (dt)
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
  rs <- init_variable_list %>%
    .[grepl(regex, description, ignore.case=TRUE)|
      grepl(regex, variable,    ignore.case=TRUE), 
      .(variable, description)] %>% unique()
  return(rs)
}