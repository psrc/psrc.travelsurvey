#' @importFrom magrittr %<>% %>%
NULL

#' Summarize PSRC travel survey data
#'
#' @param hts_data the data object, a list with either data.table or NULL for hh, person, day, trip, vehicle
#' @param analysis_unit string, either "hh", "person", "day", "trip", or "vehicle"
#' @param group_vars vector with names of one or more grouping variables, in order
#' @param stat_var string, name of numeric variable for sum, median, mean; implicit for count
#' @param incl_na logical, whether NA should be included in results--including calculated shares
#' @return summary table
#' @author Michael Jensen
#' @import data.table
#' @importFrom tidyr drop_na
#' @importFrom stringr str_replace
#' @importFrom rlang is_empty
#' @importFrom pkgcond suppress_warnings
#' @importFrom travelSurveyTools hts_prep_variable hts_prep_triprate hts_summary_cat hts_summary_num
#'
#' @export
psrc_hts_stat <- function(hts_data, analysis_unit, group_vars=NULL, stat_var=NULL, incl_na=TRUE){
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  hts_tblnames <- c("hh", "person", "day", "trip", "vehicle")
  if(!"survey_year" %in% group_vars){group_vars <- c("survey_year", group_vars)}
  if(is.null(stat_var)){                                                       # Separate last grouping var for counts   
    statvar <- group_vars[length(group_vars)]
    grpvars <- if(rlang::is_empty(group_vars[-length(group_vars)])){NULL}else{group_vars[-length(group_vars)]}
  }else{
    statvar <- stat_var
    grpvars <- group_vars
  }
  # Helper function to add variable row to codebook         
  add_var <- function(var){
    found_idx <- lapply(hts_data, function(x) any(var %in% colnames(x))==TRUE) %>% unlist()
    if(!any(found_idx)){
      NULL
    }else if(any(found_idx)){
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
    }
    return(var_row)
  }
  codebook_vars <- copy(psrc.travelsurvey:::variable_list) %>% setDT()         # mutable copy
  newvars <- setdiff(c(grpvars, statvar), 
                     c(codebook_vars$variable, "num_trips_wtd", "vmt_wtd"))    # Identify any new variables
  if(!rlang::is_empty(newvars)){
    newrows <- lapply(newvars, add_var) %>% rbindlist()
    codebook_vars %<>% rbind(newrows)                                          # Add new variables to codebook
  }
  mark_vars <- setdiff(                                                        # Identify any variables transferred to analysis unit table
    colnames(hts_data[[{{analysis_unit}}]]), 
    unique(c(codebook_vars[get(analysis_unit)==1,variable], "hh_id")))
  if(!rlang::is_empty(mark_vars)){
    codebook_vars %<>% .[variable %in% (mark_vars), (hts_tblnames):=0]
    codebook_vars %<>% .[variable %in% (mark_vars), (analysis_unit):=1]        # Denote transferred variables in codebook
  }
  if(analysis_unit=="vehicle"){                                                # keep only tables relevant to analysis unit
    hts_data_relevant <- copy(hts_data)[c("hh","vehicle")]                     # for vehicle, hh is only other table
    codebook_vars %<>% .[(hh==1|vehicle==1)]
  }else{
    keep_range <- 1:(which(names(hts_data)==analysis_unit))                    # otherwise, keep hierarchically higher tables
    hts_data_relevant <- copy(hts_data)[{{ keep_range }}]                      # e.g. day keeps day, person, hh, but not trip
    filter_cols <- names(hts_data_relevant)
    codebook_vars %<>% .[.[, Reduce(`|`, lapply(.SD, `==`, 1)), .SDcols = filter_cols]] # filter variable list so prep doesn't complain
  }
  if(any(c("num_trips_wtd", "vmt_wtd") %in% c(statvar, grpvars))){
    analysis_unit <- "day"
  }
  pk_id <- paste0(analysis_unit,"_id")
  if("num_trips_wtd" %in% c(statvar, grpvars)){
    prepped_dt <- suppressMessages(
      travelSurveyTools::hts_prep_triprate(
        summarize_by = grpvars,
        variables_dt = codebook_vars,
        hts_data = hts_data_relevant,
        trip_name = "trip",
        day_name = "day",
        ids = paste0(names(hts_data_relevant),"_id"),
        wts = paste0(names(hts_data_relevant),"_weight"),
        weighted = TRUE,
        remove_outliers = FALSE,
        strataname = "sample_segment")) %>% lapply(setDT)
  }else if("vmt_wtd" %in% c(statvar, grpvars)){
    prepped_dt <- suppressMessages(
      travelSurveyTools::hts_prep_vmtrate(
        summarize_by = grpvars,
        variables_dt = codebook_vars,
        hts_data = hts_data_relevant,
        trip_name = "trip",
        day_name = "day",
        ids = paste0(names(hts_data_relevant),"_id"),
        traveler_count_var="travelers_num",
        dist_var="distance_miles",
        mode_var="mode_class",
        mode_regex="^Ride|Drive",
        wts = paste0(names(hts_data_relevant),"_weight"),
        weighted = TRUE,
        remove_outliers = FALSE,
        strataname = "sample_segment")) %>% lapply(setDT)
  }else{
    prepped_dt <- suppressMessages(
       travelSurveyTools::hts_prep_variable(
        summarize_var = statvar,
        summarize_by = grpvars,
        variables_dt = codebook_vars,
        data = hts_data_relevant,
        id_cols = paste0(names(hts_data_relevant),"_id"),
        wt_cols = paste0(names(hts_data_relevant),"_weight"),
        weighted = TRUE,
        remove_outliers = FALSE,
        remove_missing = !incl_na,
        strataname = "sample_segment")) %>% lapply(setDT) %>%
      lapply(unique, by=pk_id, na.rm=!incl_na)
  }
  if(is.null(stat_var)){
    statvartype <- codebook_vars[variable==(statvar), data_type] %>% unique()
    if(incl_na==FALSE){prepped_dt$cat %<>% tidyr::drop_na()}
    pkgcond::suppress_warnings(
      summary_dt <- travelSurveyTools::hts_summary_cat(                        # count
        prepped_dt = prepped_dt$cat,                 
        summarize_var = statvar,
        summarize_by = grpvars,
        summarize_vartype = statvartype,
        weighted = TRUE,
        wtname = hts_wgt_var(analysis_unit),
        strataname = "sample_segment",
        se = TRUE,
        id_cols = pk_id),
      pattern="(NAs introduced by coercion|PSU)"
    )
  }else{
    if(incl_na==FALSE){prepped_dt$num %<>% tidyr::drop_na()}
    pkgcond::suppress_warnings(    
      summary_dt <- travelSurveyTools::hts_summary_num(                        # min/max/median/mean
        prepped_dt = prepped_dt$num,                 
        summarize_var = statvar,
        summarize_by = grpvars,
        weighted = TRUE,
        wtname = hts_wgt_var(analysis_unit),
        strataname = "sample_segment",
        se = TRUE),
      pattern="(NAs introduced by coercion|PSU)"
    )
  }
  summary_dt$wtd %<>%                                                          # convert se to moe
    .[, grep("_se$", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se$", colnames(.))] %>%
    setnames(grep("_se$", colnames(.)), stringr::str_replace(grep("_se$", colnames(.), value=TRUE), "_se$", "_moe"))
  return(summary_dt$wtd)
}

#' Summarize PSRC travel survey trip rates
#'
#' @param hts_data the data object, a list with either data.table or NULL for hh, person, day, trip, vehicle
#' @param group_vars vector with names of one or more grouping variables, in order
#' @param incl_na logical, whether NA should be included in results--including calculated shares
#' @return summary table
#' @author Michael Jensen
#' @import data.table
#'
#' @export
psrc_hts_triprate <- function(hts_data, group_vars=NULL, incl_na=TRUE){
  rs <- psrc_hts_stat (hts_data, analysis_unit="trip", group_vars=group_vars, stat_var="num_trips_wtd", incl_na=incl_na)
}

#' Summarize PSRC travel survey vmt rates
#'
#' @param hts_data the data object, a list with either data.table or NULL for hh, person, day, trip, vehicle
#' @param group_vars vector with names of one or more grouping variables, in order
#' @param incl_na logical, whether NA should be included in results--including calculated shares
#' @return summary table
#' @author Michael Jensen
#' @import data.table
#' @importFrom stringr str_detect str_extract
#'
#' @export
psrc_hts_vmtrate <- function(hts_data, group_vars=NULL, incl_na=TRUE){
  if(!any(grepl("^distance_miles$|^travelers_total$|^mode_class$", colnames(hts_data$trip)))){
    print("`distance_miles`, `travelers_total` and/or `mode_class` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>%
      .[, travelers_num:=as.numeric(ifelse(str_detect(as.character(travelers_total), "^\\d+"), 
                                           str_extract(as.character(travelers_total), "^\\d+"), 1))]
  }
  rs <- psrc_hts_stat (hts_data, analysis_unit="trip", group_vars=group_vars, stat_var="vmt_wtd", incl_na=incl_na)
  return(rs)
}

## quiets concerns of R CMD check
utils::globalVariables(c("statvar","grpvars","codebook_vars","found_idx",
    "found_tbl","found_classes","found_dtype","var_row","newvars","newrows","hh",
    "vehicle","variable","data_type ","statvartype","prepped_dt","summary_dt"))
