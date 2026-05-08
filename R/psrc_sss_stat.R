#' Summarize PSRC transportation safety & security survey data
#'
#' @param sss_data data.table returned by `get_psrc_sss()`
#' @param group_vars vector with names of one or more grouping variables, in order
#' @param stat_var string, name of numeric variable for min/max/median/mean; implicit for count/share
#' @param incl_na logical, whether NA should be included in results--including calculated shares
#' @return summary table
#' @author Michael Jensen
#' @import data.table
#'
#' @export
psrc_sss_stat <- function(sss_data, group_vars = NULL, stat_var = NULL, incl_na = TRUE){
  options(survey.adjust.domain.lonely = TRUE)
  options(survey.lonely.psu = "adjust")
  person_weight <- sample_segment <- .sss_nonmissing <- .sss_stat <- NULL
  drop_missing_rows <- function(dt, cols){
    cols <- intersect(cols, colnames(dt))
    if(is.null(dt) || rlang::is_empty(cols)){
      return(dt)
    }
    dt[stats::complete.cases(dt[, ..cols])]
  }
  min_or_na <- function(x){
    if(all(is.na(x))){
      return(NA_real_)
    }
    min(x, na.rm = TRUE)
  }
  max_or_na <- function(x){
    if(all(is.na(x))){
      return(NA_real_)
    }
    max(x, na.rm = TRUE)
  }
  if(is.null(stat_var)){
    if(rlang::is_empty(group_vars)){
      stop("`group_vars` must include at least one variable when `stat_var` is NULL.")
    }
    statvar <- group_vars[length(group_vars)]
    grpvars <- if(rlang::is_empty(group_vars[-length(group_vars)])){NULL}else{group_vars[-length(group_vars)]}
  }else{
    statvar <- stat_var
    grpvars <- group_vars
  }
  needed_vars <- unique(c("person_weight", "sample_segment", grpvars, statvar))
  missing_vars <- setdiff(needed_vars, colnames(sss_data))
  if(!data.table::is.data.table(sss_data)){
    sss_data <- data.table::copy(data.table::as.data.table(sss_data))
  }else{
    sss_data <- data.table::copy(sss_data)
  }
  if(!rlang::is_empty(missing_vars)){
    stop(sprintf("Missing required variable(s): %s", paste(missing_vars, collapse = ", ")))
  }
  if(incl_na == FALSE){
    sss_data <- drop_missing_rows(sss_data, unique(c(grpvars, statvar)))
  }
  if(!is.null(stat_var)){
    sss_data[, .sss_stat := get(statvar)]
    sss_data[, .sss_nonmissing := !is.na(get(statvar))]
  }
  design <- srvyr::as_survey_design(
    .data = sss_data,
    ids = 1,
    strata = sample_segment,
    weights = person_weight,
    nest = TRUE
  )
  if(is.null(stat_var)){
    summary_dt <- pkgcond::suppress_warnings(
      if(is.null(grpvars)){
        design %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(statvar))) %>%
          dplyr::summarize(
            count = as.integer(srvyr::unweighted(dplyr::n())),
            prop = srvyr::survey_prop(vartype = "se", proportion = TRUE),
            est = srvyr::survey_total(vartype = "se"),
            .groups = "drop"
          )
      }else{
        design %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(grpvars, statvar)))) %>%
          dplyr::summarize(
            count = as.integer(srvyr::unweighted(dplyr::n())),
            prop = srvyr::survey_prop(vartype = "se", proportion = TRUE),
            est = srvyr::survey_total(vartype = "se"),
            .groups = "drop"
          )
      },
      pattern = "PSU"
    )
  }else{
    summary_dt <- pkgcond::suppress_warnings(
      if(is.null(grpvars)){
        design %>%
          dplyr::summarize(
            count = as.integer(srvyr::unweighted(sum(.sss_nonmissing))),
            min = min_or_na(.sss_stat),
            max = max_or_na(.sss_stat),
            mean = srvyr::survey_mean(.sss_stat, vartype = "se", na.rm = TRUE),
            median = srvyr::survey_median(.sss_stat, vartype = "se", na.rm = TRUE)
          )
      }else{
        design %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(grpvars))) %>%
          dplyr::summarize(
            count = as.integer(srvyr::unweighted(sum(.sss_nonmissing))),
            min = min_or_na(.sss_stat),
            max = max_or_na(.sss_stat),
            mean = srvyr::survey_mean(.sss_stat, vartype = "se", na.rm = TRUE),
            median = srvyr::survey_median(.sss_stat, vartype = "se", na.rm = TRUE),
            .groups = "drop"
          )
      },
      pattern = "PSU"
    )
  }
  summary_dt <- data.table::as.data.table(summary_dt)
  se_cols <- grep("_se$", colnames(summary_dt), value = TRUE)
  if(length(se_cols) > 0){
    summary_dt[, (se_cols) := lapply(.SD, function(x) x * 1.645), .SDcols = se_cols]
    data.table::setnames(summary_dt, se_cols, sub("_se$", "_moe", se_cols))
  }
  double_cols <- colnames(summary_dt)[vapply(summary_dt, is.double, logical(1))]
  if(length(double_cols) > 0){
    summary_dt[, (double_cols) := lapply(.SD, function(x){
      x[is.nan(x)] <- NA_real_
      x
    }), .SDcols = double_cols]
  }
  return(summary_dt[])
}

## quiets concerns of R CMD check
utils::globalVariables(c("person_weight", "sample_segment", "statvar", "grpvars", ".sss_nonmissing", ".sss_stat"))