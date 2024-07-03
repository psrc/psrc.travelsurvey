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
#'
#' @export
get_psrc_hts <- function(survey_years, survey_vars){
  tblnames <- tblname <- survey_vars <- hts_split_vars <- variable <- NULL # For CMD check
  tblnames <- c("household","person","day","trip","vehicle")
  # Helper function; identifies which tables the desired variables are in, using codebook
  hts_split_vars <- function(tblname, survey_vars){
    rs <- init_variable_list[get(tblname)==1 & variable %in% (survey_vars), variable] %>% unlist()
  }
  # Helper function; queries Elmer for specified variables
  hts_query_elmer <- function(tblname, tblvars, survey_years){
    if(is.null(tblvars)){
      NULL
    }else{
      id_vars <- if(tblname=="vehicle"){
        "vehicle_id"
      }else{
        paste0(tblnames[1:(which(tblnames==tblname))], "_id")
      }
      sql <- paste0("SELECT ", paste0("CAST(", id_vars, " AS nvarchar)", collapse=", "), 
                    stuff(tblvars), paste0(tblname, "_weight"),
                    " FROM HHSurvey.v_", tblname, "s_labels", 
                    " WHERE survey_year IN(", stuff(survey_years),");")
      rs <- psrcelmer::get_query(sql) %>% setDT() %>% psrc_hts_recode_na()
    }
  }
  split_vars <- sapply(tblnames, hts_split_vars, survey_vars, simplify = FALSE, USE.NAMES = TRUE)
  hts_data <- mapply(hts_query_elmer, names(split_vars), split_vars, survey_years, USE.NAMES = TRUE)
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
  description <- var_name <- NULL
  rs <- variable_list %>%
    .[grepl(regex, description, ignore.case=TRUE)|grepl(regex, variable, ignore.case=TRUE), ] %>% unique()
  return(rs)
}