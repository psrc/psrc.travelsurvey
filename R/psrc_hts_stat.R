#' @importFrom magrittr %<>% %>%
NULL

`%not_in%` <- Negate(`%in%`)

#' Summarize PSRC travel survey data
#'
#' @param hts_data the data object, a list with either data.table or NULL for hh, person, day, trip, vehicle
#' @param analysis_unit string, either "household", "person", "day", "trip", or "vehicle"
#' @param group_vars vector with names of one or more grouping variables, in order
#' @param stat_var string, name of numeric variable for sum, median, mean; implicit for count
#' @param incl_na logical, whether NA should be included in results--including calculated shares
#' @return summary table
#' @author Michael Jensen
#' @import data.table
#' @importFrom stringr str_replace
#' @importFrom travelSurveyTools hts_prep_variable hts_summary_cat hts_summary_num
#'
#' @export
psrc_hts_stat <- function(hts_data, analysis_unit, group_vars=NULL, stat_var=NULL, incl_na=TRUE){
  statvar <- grpvars <- found_idx <- found_tbl <- found_classes <- NULL
  found_dtype <- codebook_vars <- var_row <- newvars <- newrows <- NULL
  statvartype <- prepped_dt <- summary_dt <- NULL # For CMD check
  if(is.null(stat_var)){                                                       # Separate last grouping var for counts   
    statvar <- group_vars[length(group_vars)]
    grpvars <- group_vars[-length(group_vars)]
  }else{
    statvar <- stat_var
    grpvars <- group_vars 
  }
  if("survey_year" %not_in% grpvars){grpvars <- c("survey_year", grpvars)}
  # Helper function to add variable row to codebook         
  add_var <- function(var){
    found_idx <- lapply(hts_data, function(x) any(var %in% colnames(x))==TRUE) %>% unlist()
    found_tbl <- names(hts_data[found_idx])
    found_classes <- class(hts_data[[found_tbl]][[var]]) 
    found_dtype <- if("numeric" %in% found_classes){"numeric"
                 }else if("Date" %in% found_classes){"date"    
                 }else if(any(c("POSIXct","POSIXt") %in% found_classes)){"date-time" 
                 }else if(any(c("character","factor") %in% found_classes)){"integer/categorical"
                 }
    var_row <- data.frame(variable=var, 
                          is_checkbox=0, 
                          hh=     if('hh'      %in% found_tbl){1}else{0},
                          person= if('person'  %in% found_tbl){1}else{0},
                          day=    if('day'     %in% found_tbl){1}else{0},
                          trip=   if('trip'    %in% found_tbl){1}else{0},
                          vehicle=if('vehicle' %in% found_tbl){1}else{0},
                          location=0,
                          data_type=found_dtype,
                          description="Added",
                          shared_name=var)
    return(var_row)
  }
  codebook_vars <- init_variable_list                                          # mutable copy
  newvars <- NULL                                                              # find any new variables
  newvars <- setdiff(c(grpvars, statvar), codebook_vars$variable) 
  if(!is.null(newvars)){                                                       # add new variables to codebook
    newrows <- lapply(newvars, add_var) %>% rbindlist()
    codebook_vars %<>% rbind(newrows)
  }
  prepped_dt <- suppressMessages(
            hts_prep_variable(summarize_var = statvar,
                              summarize_by = grpvars,
                              variables_dt = codebook_vars,
                              data = hts_data,
                              remove_outliers = FALSE,
                              remove_missing = !incl_na,
                              weighted =TRUE,
                              strataname = "sample_segment"))
  if(is.null(stat_var)){                                                       # count
    statvartype <- codebook_vars[variable==(statvar), data_type] %>% unique()
    if(incl_na==FALSE){prepped_dt$cat %<>% .[!is.na(get(statvar))]}
    summary_dt <- hts_summary_cat(prepped_dt = prepped_dt$cat,
                              summarize_var = statvar,
                              summarize_by = grpvars,
                              summarize_vartype = statvartype,
                              weighted = TRUE,
                              wtname = hts_wgt_var(analysis_unit),
                              strataname = "sample_segment",
                              se = TRUE) 
  }else{
    if(incl_na==FALSE){prepped_dt$num %<>% .[!is.na(get(statvar))]}
    summary_dt <- hts_summary_num(prepped_dt = prepped_dt$num,                 # min/max/median/mean
                              summarize_var = statvar,
                              summarize_by = grpvars,
                              weighted = TRUE,
                              wtname = hts_wgt_var(analysis_unit),
                              strataname = "sample_segment",
                              se = TRUE)  
  }
  summary_dt$wtd %<>%                                                          # convert se to moe
    .[, grep("_se", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se", colnames(.))] %>%
    setnames(grep("_se", colnames(.)), str_replace(grep("_se", colnames(.), value=TRUE), "_se", "_moe"))
  return(summary_dt$wtd)
}
