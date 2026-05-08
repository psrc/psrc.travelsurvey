psrc_survey_na_regex <- function(){
  paste(
    unique(c(
      "^Missing(:? ([tT]echnical [eE]rror|[nN]on-response|[sS]kip [lL]ogic|[dD]ata|[rR]esponse))?$",
      "Not [iI]mputable",
      "^Children or [mM]issing$",
      "^-999[78]$",
      "^-?995$",
      "^$"
    )),
    collapse = "|"
  )
}

psrc_survey_recode_na <- function(dt, na_rgx = psrc_survey_na_regex()){
  if(is.null(dt)){
    return(dt)
  }
  for(col in colnames(dt)){
    set(dt, i = grep(na_rgx, dt[[col]]), j = col, value = NA)
  }
  dt
}

psrc_survey_to_factor <- function(
    dt,
    variables_dt = if(exists("variable_list", inherits = TRUE)) get("variable_list", inherits = TRUE) else NULL,
    labels_dt = if(exists("value_labels", inherits = TRUE)) get("value_labels", inherits = TRUE) else NULL,
    na_rgx = psrc_survey_na_regex()){
  data_type <- variable <- label <- NULL
  if(is.null(dt) || is.null(variables_dt) || is.null(labels_dt) || nrow(dt) == 0){
    return(dt)
  }
  variables_dt <- data.table::as.data.table(data.table::copy(variables_dt))
  labels_dt <- data.table::as.data.table(data.table::copy(labels_dt))
  if(!all(c("variable", "data_type") %in% colnames(variables_dt)) ||
     !all(c("variable", "label") %in% colnames(labels_dt))){
    return(dt)
  }
  lbl_ftrs <- unique(labels_dt$variable)
  factor_vars <- unique(variables_dt[data_type == "integer/categorical" & variable %in% lbl_ftrs, variable])
  ftr_cols <- intersect(colnames(dt), factor_vars)
  for(col in ftr_cols){
    levels <- labels_dt[variable == col & !grepl(na_rgx, label), label] %>%
      as.vector() %>% unique() %>% trimws()
    datavalues <- dt[!is.na(get(col)), get(col)] %>%
      as.vector() %>% unique() %>% as.character() %>% trimws()
    if(length(levels) > 0 && all(datavalues %in% levels)){
      set(dt, i = NULL, j = col, value = factor(dt[[col]], levels = levels, exclude = NA, ordered = TRUE))
    }
  }
  dt
}

psrc_survey_desc_labels <- function(
    dt,
    variables_dt = if(exists("variable_list", inherits = TRUE)) get("variable_list", inherits = TRUE) else NULL){
  variable <- description <- NULL
  if(is.null(dt) || is.null(variables_dt)){
    return(dt)
  }
  variables_dt <- data.table::as.data.table(data.table::copy(variables_dt))
  if(!all(c("variable", "description") %in% colnames(variables_dt))){
    return(dt)
  }
  for(col in intersect(colnames(dt), variables_dt$variable)){
    labelled::var_label(dt[[col]]) <- variables_dt[variable == col, description][[1]]
  }
  dt
}