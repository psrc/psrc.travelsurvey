#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen

NULL

`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]
stuff <- function(x){unique(x) %>% paste(collapse=",")}

elmer_connect <- function(){DBI::dbConnect(odbc::odbc(),
                                   driver = "ODBC Driver 17 for SQL Server",
                                   server = "AWS-PROD-SQL\\Sockeye",
                                   database = "Elmer",
                                   trusted_connection = "yes",
                                   port = 1433)
}

#' Search HHTS variable definitions
#'
#' Look for a variable name using a search term
#' @param regex search term
#' @return data.table of filtered variable attributes
#' 
#' @import data.table
#' @export
hhts_varsearch <- function(regex){
  varsql <- paste("SELECT [variable] AS var_name, variable_name AS var_desc, survey_year",
                  "FROM HHSurvey.variable_metadata",
                  "GROUP BY [variable], variable_name, survey_year;")
  elmer_connection <- elmer_connect()
  rs <- DBI::dbGetQuery(elmer_connection, DBI::SQL(varsql)) %>% setDT() %>% 
    .[grepl(regex,var_desc, ignore.case=TRUE)|grepl(regex,var_name, ignore.case=TRUE)] %>%
    .[, .(years=stuff(survey_year)), by=.(var_name, var_desc)] %>% unique()
  DBI::dbDisconnect(elmer_connection)
  return(rs)
}

#' Retrieve HHTS variable definitions
#'
#' Gets requested variable attributes--e.g. data type, associated weight name & priority
#' @param dyear data year or data year vector, e.g. c(2017, 2019)
#' @param vars character vector with requested variables
#' @return data.table of filtered variable attributes
#' 
#' @import data.table
get_var_defs <- function(dyear, vars){
var_def_sql <- paste("SELECT survey_year, [variable] AS var_name, table_name, dtype, weight_name, weight_priority, levels",
                      "FROM HHSurvey.variable_metadata;")
elmer_connection <- elmer_connect()
var_defs <- DBI::dbGetQuery(elmer_connection, DBI::SQL(var_def_sql)) %>% setDT() %>% .[var_name %in% vars & survey_year %in% dyear]
DBI::dbDisconnect(elmer_connection)
return(var_defs)
}

#' HHTS recode missing as NA
#' 
#' Recode missing values to NA
#' @param dt the data.table
#' @return the data.table with missing values recoded NA
#' 
#' @import data.table
hhts_recode_na <- function(dt){
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

#' HHTS data retrieval
#'
#' Gets requested Household Travel Survey variables
#' 
#' @param dyear data year or data year vector, e.g. c(2017, 2019)
#' @param level either "h" (household), "p" (person), "d" (day), "t" (trip), or "v" (vehicle) 
#' @param vars character vector with requested variables
#' @return dataframe with variables and necessary weights
#'  
#' @import data.table
#' @export
get_hhts <- function(dyear, level, vars){
    elmer_hhts_lookup <- data.frame(
                            abbr    =c("h","p","t","d","v"),
                            tbl_ref =c("HHSurvey.v_households",
                                      "HHSurvey.v_persons",
                                      "HHSurvey.v_trips",
                                      "HHSurvey.v_day",
                                      "HHSurvey.v_vehicle")) %>% setDT()
    elmer_tbl_ref <- elmer_hhts_lookup[abbr==level, .(tbl_ref)][[1]]
    elmer_sql <- paste("SELECT * FROM",elmer_tbl_ref,"WHERE survey_year IN(",unique(dyear) %>% paste(collapse=","),");")
    keep_vars <- c("survey_year", unlist(vars))
    elmer_connection <- elmer_connect()
    df <- DBI::dbGetQuery(elmer_connection, DBI::SQL(elmer_sql)) %>% setDT() %>%                   # Retrieve table by year/s
        .[, colnames(.) %in% keep_vars | grepl("_weight_", colnames(.)), with=FALSE] %>%           # Filter columns
      hhts_recode_na() %>% setDF()                                                                 # Recode NA
    is.na(df) <- is.null(df)                                                                       # Recode NULL
    DBI::dbDisconnect(elmer_connection)
    return(df)   
}    

#' HHTS to srvyr
#'
#' Creates srvyr object from HHTS
#' @param df the dataframe returned by \code{\link{get_hhts}}
#' @param dyear data year or data year vector, e.g. c(2017, 2019)
#' @param vars character vector with requested variables
#' @param spec_wgt optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.
#' @param incl_na option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)
#' @return srvyr object with sampling weights
#'
#' @import data.table
#' @importFrom tidyselect all_of
#' @importFrom rlang is_empty
hhts2srvyr <- function(df, dyear, vars, spec_wgt=NULL, incl_na=TRUE){
  na_exclude <- if(incl_na==FALSE){NA}else{NULL}
  var_defs <- get_var_defs(dyear, vars) %>% setkeyv("var_name")
  num_vars <- copy(var_defs) %>% .[dtype=="fact", .(var_name)] %>% unique() %>% .[[1]]
  ftr_vars <- copy(var_defs) %>% .[dtype=="dimension", .(var_name)] %>% unique() %>% .[[1]]
  if(!is.null(spec_wgt)){
    wgt_var <- spec_wgt                                                                            # Option for power-users to determine the expansion weight  
  }else if (2021 %in% dyear){
    wgt_var  <- copy(var_defs) %>% .[var_name %in% vars, .(weight_name, weight_priority)] %>%      # Weird 2021 weighting determined by variable hierarchy
      unique() %>% setorder(weight_priority) %>% .[1, .(weight_name)] %>% .[[1]]
  }else{
    tbl_names <- copy(var_defs) %>% .[var_name %in% vars, .(table_name)] %>% unique()              # Standard weighting by table; construct w/ rules
    level <- if("Trip" %in% tbl_names){"trip_weight_"}else{"hh_weight_"}
    yearz <- paste0(dyear, collapse="_")
    wgt_var <- paste0(level, yearz)
  }
  if(mean(dyear) %between% c(2017,2019)){wgt_var %<>% paste0("_v2021")}
  keep_vars <- c("survey_year", unlist(vars), wgt_var)
  df2 <- copy(df) %>% setDT() %>% 
    .[(!is.null(get(wgt_var)) & !is.na(get(wgt_var))), colnames(.) %in% keep_vars, with=FALSE]     # Keep only necessary elements/records 
  if(!is_empty(num_vars)){df2[, (num_vars):=lapply(.SD, as.numeric), .SDcols=num_vars]}
  if(!is_empty(ftr_vars)){                                                                         # srvyr package requires grouping variables as factors;
    for (f in ftr_vars){
      if(f %in% var_defs$var_name){
        setkeyv(df2, f)
        for_level <- var_defs[var_name==rlang::as_string(f), .(levels)][[1]] %>% 
          strsplit(", ") %>% unique() %>% .[[1]]
        df2[, (f):=factor(get(f), levels=for_level, exclude=na_exclude)]                           # Factor level ordering from metadata
      }else{
        df2[, (f):=factor(get(f), exclude=na_exclude)]                                             # Default factor level ordering
      } 
    }
  }
  df2 %<>% setDF()
  so <- srvyr::as_survey_design(df2, variables=all_of(keep_vars), weights=all_of(wgt_var))
  return(so)
}    

#' Generic call for HHTS summary statistics
#'
#' Given specific form by related \code{\link{hhts_stat}} functions.
#' @param df the dataframe returned by \code{\link{get_hhts}}
#' @param stat_type Desired survey statistic
#' @param stat_var The numeric variable to summarize 
#' @param group_vars Categorical variable/s for grouping
#' @param geographic_unit Geographic grouping, i.e. units smaller than PSRC region
#' @param spec_wgt optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.
#' @param incl_na option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)
#' @return A summary tibble, including variable names, summary statistic and margin of error
#'
#' @importFrom srvyr interact cascade survey_tally survey_total survey_median survey_mean survey_prop
hhts_stat <- function(df, stat_type, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  vars <- c(geographic_unit, stat_var, unlist(group_vars)) %>% unique()
  data_year <- df$survey_year %>% unique()
  so <- hhts2srvyr(df, data_year, vars, spec_wgt, incl_na) %>% dplyr::ungroup()
  prefix <- if(stat_type %in% c("count","share")){""}else{paste0(stat_var,"_")}
  if(!is.null(group_vars)){
    so %<>% srvyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))                         # Apply grouping
  }
  if(!is.null(geographic_unit)){so %<>% srvyr::group_by(!!as.name(geographic_unit), .add=TRUE)}
  if(stat_type=="count"){
    rs <- suppressMessages(
            cascade(so,
              count:=survey_total(na.rm=TRUE),
              share:=survey_prop(),
              sample_size:=srvyr::unweighted(dplyr::n()),
              .fill="Total"))
  }else if(stat_type=="summary"){
    rs <- suppressMessages(
            cascade(so, count:=survey_total(na.rm=TRUE),
              !!paste0(prefix,"total"):=survey_total(!!as.name(stat_var), na.rm=TRUE),
              !!paste0(prefix,"median"):=survey_median(!!as.name(stat_var), na.rm=TRUE),
              !!paste0(prefix,"mean"):=survey_mean(!!as.name(stat_var), na.rm=TRUE),
              .fill="Total"))
  }else{
    srvyrf_name <- as.name(paste0("survey_",stat_type))                                            # Specific srvyr function name
    rs <- suppressMessages(
            cascade(so,
              !!paste0(prefix, stat_type):=(as.function(!!srvyrf_name)(!!as.name(stat_var), na.rm=TRUE)),
              .fill="Total"))
  }
  rs %<>% purrr::modify_if(is.factor, as.character) %>% setDT() %>%
    .[, grep("_se", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se", colnames(.))] %>%
    setnames(grep("_se", colnames(.)), stringr::str_replace(grep("_se", colnames(.), value=TRUE), "_se", "_moe"))
  if(!is.null(geographic_unit)){
    setcolorder(rs, c(geographic_unit))
    setorder(rs, geographic_unit)
    rs[is.na(geographic_unit), (geographic_unit):="Region"]
  }
  so %<>% dplyr::ungroup()
  return(rs)
}

#' Household Survey summary statistics
#'
#' Separate function for total, count, median, mean' 
#' 
#' @param df the dataframe returned by \code{\link{get_hhts}}
#' @param stat_var The numeric variable to summarize 
#' @param group_vars Categorical variable/s for grouping
#' @param geographic_unit Default="region"
#' @param spec_wgt optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.
#' @param incl_na option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)
#' @return A table with the variable names and labels, summary statistic and margin of error
#' 
#' @name hhts_stat
NULL

#' @rdname hhts_stat
#' @title HHTS counts
#' 
#' @export
hhts_count <- function(df, stat_var=NULL, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  rs <- hhts_stat(df=df, 
                  stat_type="count", 
                  stat_var=NULL, 
                  group_vars=group_vars, 
                  geographic_unit=geographic_unit, 
                  spec_wgt=spec_wgt, 
                  incl_na=incl_na
                  )
  return(rs)
}
#' @rdname hhts_stat
#' @title HHTS totals
#' @export
hhts_sum <- function(df, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  rs <- hhts_stat(df=df, 
                  stat_type="total", 
                  stat_var=stat_var, 
                  group_vars=group_vars, 
                  geographic_unit=geographic_unit, 
                  spec_wgt=spec_wgt, 
                  incl_na=incl_na)
  return(rs)
}

#' @rdname hhts_stat
#' @title HHTS medians
#'
#' @export
hhts_median <- function(df, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  rs <- hhts_stat(df=df, 
                  stat_type="median", 
                  stat_var=stat_var, 
                  group_vars=group_vars, 
                  geographic_unit=geographic_unit, 
                  spec_wgt=spec_wgt, 
                  incl_na=incl_na)
  return(rs)
}

#' @rdname hhts_stat
#' @title HHTS means
#' @export
hhts_mean <- function(df, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  rs <- hhts_stat(df=df, 
                  stat_type="mean", 
                  stat_var=stat_var, 
                  group_vars=group_vars, 
                  geographic_unit=geographic_unit, 
                  spec_wgt=spec_wgt, 
                  incl_na=incl_na)
  return(rs)
}

#' @rdname hhts_stat
#' @title HHTS combined summary statistics
#' @export
hhts_summary <- function(df, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  rs <- hhts_stat(df=df, 
                  stat_type="summary", 
                  stat_var=stat_var, 
                  group_vars=group_vars, 
                  geographic_unit=geographic_unit, 
                  spec_wgt=spec_wgt, 
                  incl_na=incl_na)
  return(rs)
}

#' Household Survey summary statistics
#'
#' Generate a statistic separately for a list of grouping variables
#' List input items can be multiple, i.e. character vector
#'
#' @param df the dataframe returned by \code{\link{get_hhts}}
#' @param stat_type Desired survey statistic
#' @param stat_var The numeric variable to summarize 
#' @param group_var_list Factor variable/s for grouping
#' @param geographic_unit Default="region"
#' @param spec_wgt optional user-specified expansion weight, i.e. in place of the standard expansion weight determined by the variable hierarchy. Only possible if the variable name is included in the \code{\link{get_hhts}} call. 
#' @param incl_na option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)
#' @return A table with the variable names and labels, summary statistic and margin of error
#' 
#' @importFrom data.table rbindlist setDF
#' @importFrom dplyr mutate rename relocate
#' @export
hhts_bulk_stat <- function(df, stat_type, stat_var=NULL, group_var_list=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  list_stat <- function(x){
    rsub <- hhts_stat(df=df, 
                      stat_type=stat_type, 
                      stat_var=stat_var, 
                      group_vars=x, 
                      geographic_unit=geographic_unit, 
                      spec_wgt=spec_wgt,
                      incl_na=incl_na)
  }
  df <- list()
  df <- lapply(group_var_list, list_stat) %>%
    lapply(FUN=function(y){mutate(y, "var_name"=colnames(y)[1])}) %>% rbindlist(use.names=FALSE) %>%
    rename(var_value=colnames(.)[1]) %>% relocate(var_name)
  return(df)
}

#' Z Score
#'
#' Stat to determine if two estimates are different
#'
#' @param x numeric vector, first estimate and corresponding MOE to compare
#' @param y numeric vector, second estimate and corresponding MOE to compare
#' @return Z score; if larger than 1, difference is significant
#'
#' @export
z_score <- function(x, y){
  z <- abs(x[1] - y[1]) / sqrt(x[2]^2 - y[2]^2)
  return(z)
}
