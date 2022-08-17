#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen

NULL

`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]
stuff <- function(x){unique(x) %>% paste(collapse=",")}

elmer_connect <- function(connection = NULL){
  if(!is.null(connection)) return(connection)
  DBI::dbConnect(odbc::odbc(),
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
hhts_varsearch <- function(regex, ...){
  varsql <- paste("SELECT [variable] AS var_name, description,",
                          "CONCAT(IIF([household]=1,' household',''),",
                                 "IIF([person]=1,' person',''),",
                                 "IIF([vehicle]=1,' vehicle',''),",
                                 "IIF([day]=1,' day',''),",
                                 "IIF([trip]=1,' trip','')) AS views,",
                          "CONCAT(IIF([year_2017]=1,' 2017',''),",
                                 "IIF([year_2019]=1,' 2019',''),",
                                 "IIF([year_2021]=1,' 2021','')) AS surveys",
                  "FROM HHSurvey.variable_metadata2",
                  "ORDER BY [variable];")
  elmer_connection <- elmer_connect(...)
  rs <- DBI::dbGetQuery(elmer_connection, DBI::SQL(varsql)) %>% setDT() %>% 
    .[grepl(regex, description, ignore.case=TRUE)|grepl(regex, var_name, ignore.case=TRUE)] %>% unique()
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
get_var_defs <- function(vars, ...){
var_def_sql <- paste("SELECT [variable] AS var_name, base_table_type",
                      "FROM HHSurvey.variable_metadata2;")
elmer_connection <- elmer_connect(...)
var_defs <- DBI::dbGetQuery(elmer_connection, DBI::SQL(var_def_sql)) %>% setDT() %>% .[var_name %in% vars]
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
#' @param survey character string denoting single or combined survey year--if the latter, years separated by underscore, e.g."2017_2019"
#' @param level either "h" ("households"), "p" ("persons"), "d" ("days"), "t" ("trips"), or "v" ("vehicles") 
#' @param vars character vector with requested variables
#' @return dataframe with variables and necessary weights
#'  
#' @import data.table
#' @export
get_hhts <- function(survey, level, vars, ...){
    dyears <- if(survey %in% (c("2017","2019","2017_2019","2021"))){
      strsplit(survey,"_") %>% as.list() %>% lapply(as.integer) %>% unlist()
      }else{c(2017,2019)}
    wgt_str <- paste0("_weight_",survey,"(_\\D|$)") 
    elmer_hhts_lookup <- data.frame(
                            abbr    =c("h","p","t","d","v","households","persons","trips","days","vehicles"),
                            tbl_ref =rep(c("HHSurvey.v_households",
                                      "HHSurvey.v_persons",
                                      "HHSurvey.v_trips",
                                      "HHSurvey.v_days",
                                      "HHSurvey.v_vehicles"),2)) %>% setDT()
    elmer_tbl_ref <- elmer_hhts_lookup[abbr==level, .(tbl_ref)][[1]]                               # Convert level to view name       
    elmer_sql <- paste("SELECT TOP 1 * FROM",elmer_tbl_ref,";")                                     
    elmer_connection <- elmer_connect(...)
    df <- DBI::dbGetQuery(elmer_connection, DBI::SQL(elmer_sql)) %>% setDT()                       # Get first row to have column names
    want_vars <-grep(wgt_str, colnames(df), value=TRUE) %>% unlist() %>% c(unlist(vars), .)        # Determine available weights
    elmer_sql <- paste0("SELECT '", survey, "' AS survey, ",
                       paste(want_vars, collapse=", "), " FROM ",elmer_tbl_ref,                    # Build query for only relevant variables
                       " WHERE survey_year IN(", paste(unique(dyears), collapse=", "),");")
    df <- DBI::dbGetQuery(elmer_connection, DBI::SQL(elmer_sql)) %>% setDT() %>%                   # Retrieve table by year/s
      hhts_recode_na() %>% setDF()                                                                 # Recode NA
    is.na(df) <- is.null(df)                                                                       # Recode NULL
    DBI::dbDisconnect(elmer_connection)
    return(df)   
}    

#' HHTS to srvyr
#'
#' Creates srvyr object from HHTS
#' @param df the dataframe returned by \code{\link{get_hhts}}
#' @param survey the survey year or combination of survey years comprising the survey, e.g. c(2017_2019)
#' @param vars character vector with requested variables
#' @param spec_wgt optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.
#' @return srvyr object with sampling weights
#'
#' @import data.table
#' @importFrom tidyselect all_of
#' @importFrom rlang is_empty
#' @importFrom dplyr case_when
hhts2srvyr <- function(df, survey, vars, spec_wgt=NULL){
  dyear <- stringr::str_split(survey, "_") %>% lapply(as.integer) %>% unlist()
  var_defs <- psrc.travelsurvey:::get_var_defs(vars) %>% setDT() %>% setkeyv("var_name")
  tbl_names <- copy(var_defs) %>% .[var_name %in% vars] %>% .$base_table_type %>% unique()         # Standard weighting by table; construct w/ rules
  if(!is.null(spec_wgt)){
    wgt_var <- spec_wgt                                                                            # Option for power-users to determine the expansion weight
  }else{
    ph_vars <- c("age","age_category","license","school_loc_county",
                 "schooltype","student","school_travel_last_week")
    prefix <- unique(case_when(
      "trip" %in% tbl_names ~ "trip",
      "day" %in% tbl_names & !grepl("2021", survey) ~ "day",
      any(c("person","day") %in% tbl_names) & grepl("2021", survey) & all(vars %not_in% ph_vars) ~ "person",
      TRUE ~ "hh"))
    suffix <- case_when(grepl("2021", survey) ~ "_ABS", 
                        mean(dyear) %between% c(2017,2019) ~"_v2021", 
                        TRUE ~"")
    suffix <- case_when(any(c("person","day") %in% tbl_names) &
                            any(grepl("^employment_change_|^workplace|_freq_pre_covid|_mode_pre_covid", 
                                colnames(df))) ~ paste0(suffix, "_Panel_respondent"), 
                        any(grepl("person|trip|day", tbl_names)) & grepl("2021", survey) & 
                            all(vars %not_in% ph_vars) ~ paste0(suffix, "_Panel_adult"),
                        "trip" %in% tbl_names & survey=="2017_2019" ~ paste0(suffix, "_adult"),
                        TRUE ~ suffix)
    yearz <- paste0(dyear, collapse="_")
    wgt_var <- paste0(prefix, "_weight_", yearz, suffix)                                           # Otherwise weight determined by rules
  }
  keep_vars <- c("survey", unlist(vars), wgt_var)
  df2 <- copy(df) %>% setDT() %>% .[get(wgt_var)>0, colnames(.) %in% keep_vars, with=FALSE]        # Keep only necessary elements/records
  num_vars <- names(Filter(is.numeric, df2))
  ftr_vars <- names(Filter(is.character, df2))
  if(!is_empty(num_vars)){df2[, (num_vars):=lapply(.SD, as.numeric), .SDcols=num_vars]}
  if(!is_empty(ftr_vars)){                                                                         # srvyr package requires grouping variables as factors;
    for (f in ftr_vars){
        df2[, (f):=factor(get(f))]                                                                 # Default factor level ordering
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
#' @importFrom tidyselect all_of
#' @importFrom dplyr filter if_all ungroup across coalesce
#' @importFrom srvyr interact cascade survey_tally survey_total survey_median survey_mean survey_prop
hhts_stat <- function(df, stat_type, stat_var, group_vars=NULL, geographic_unit=NULL, spec_wgt=NULL, incl_na=TRUE){
  vars <- c(geographic_unit, stat_var, unlist(group_vars)) %>% unique()
  survey <- df$survey %>% unique()
  so <- hhts2srvyr(df, survey, vars, spec_wgt)
  if(all(group_vars!="keep_existing")){so %<>% ungroup()}                                          # "keep_existing" is power-user option for srvyr::combine() groupings;
  if(all(!is.null(group_vars) & group_vars!="keep_existing")){                                     # -- otherwise the package ungroups before and afterward
    if(incl_na==FALSE){so %<>% filter(if_all(all_of(group_vars), ~ !is.na(.)))}                    # Allows users to exclude w/o removing observations from the data object itself
    so %<>% srvyr::group_by(across(all_of(group_vars)))                                            # Apply grouping
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
              sample_size:=srvyr::unweighted(dplyr::n()),
              .fill="Total"))
  }else{
    srvyrf_name <- as.name(paste0("survey_",stat_type))                                            # Specific srvyr function name
    rs <- suppressMessages(
            cascade(so,
              !!paste0(prefix, stat_type):=(as.function(!!srvyrf_name)(!!as.name(stat_var), na.rm=TRUE)),
              sample_size:=srvyr::unweighted(dplyr::n()),
              .fill="Total"))
  }
  rs %<>% setDT() %>%
    .[, grep("_se", colnames(.)):=lapply(.SD, function(x) x * 1.645), .SDcols=grep("_se", colnames(.))] %>%
    setnames(grep("_se", colnames(.)), stringr::str_replace(grep("_se", colnames(.), value=TRUE), "_se", "_moe"))
  if(!is.null(geographic_unit)){
    rs[is.na(geographic_unit), (geographic_unit):="Region"]
  }
  rs[, survey:=unique(so[[7]]$survey)]
  setcolorder(rs, c("survey", coalesce(geographic_unit)))
  setorderv(rs, c("survey", coalesce(geographic_unit)))
  so %<>% ungroup()
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
#' @param x a row of aggregate table to compare--includes a count/sum/mean/median and its MOE
#' @param y a row of aggregate table to compare--also includes a count/sum/mean/median and its MOE
#' @return Z score; if larger than 1, difference is significant
#'
#' @export
z_score <- function(x, y){
  reduce <- function(a){
    a %<>% setDT() %>% .[, .SD, .SDcols=patterns("(count|sum|median|mean)(_moe)?")] %>% as.numeric(.[1])
  }
  x1 <- reduce(x)
  y1 <- reduce(y)
  z <- abs(x1[1] - y1[1]) / sqrt(x1[2]^2 + y1[2]^2)
  return(z)
}