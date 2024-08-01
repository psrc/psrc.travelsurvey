#' @importFrom magrittr %<>% %>%
NULL

`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]

safegsub <- function(rgx, x){
  ans <- ifelse(grepl(rgx, x),
                gsub(rgx, "\\1", x),
                "")
  return(ans)
}

#' Add binned income variable
#' Requires `hhincome_broad` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a binned income variable
#' @importFrom dplyr coalesce
#' @author Michael Jensen
#' @export
hts_bin_income <- function(hts_data){
  hhincome_broad <- hhincome_bin5 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^hhincome_broad$", colnames(hts_data$hh)))){
    print("`hhincome_broad` variable missing from data")
  }else{
    hts_data$hh %<>% setDT() %>%
      .[, hhincome_bin5:=factor(
        fcase(grepl("\\$(1|2)00,", as.character(hhincome_broad)), "$100,000 or more",
             !is.na(hhincome_broad),                           as.character(hhincome_broad)),
       levels=c("Under $25,000","$25,000-$49,999","$50,000-$74,999",
                "$75,000-$99,999","$100,000 or more","Prefer not to answer"))]
  }
  return(hts_data)
}

#' Add a binned age variable
#' Requires `age` variable
#'
#' @param hts_data the hts_data list object
#' @param cats number of binned categories--3 or 5
#' @return hts_data with an additional binned age variable
#' @author Michael Jensen
#' @export
hts_bin_age <- function(hts_data, cats){
  age <- age_bin3 <- age_bin5 <- NULL # Bind variables locally for CMD check
  rgx_yr <- ".*(\\d+) years (old)?"
  if(cats %not_in% c(3,5)){
    print("cats must be either 3 or 5")
  }else if(!any(grepl("^age$", colnames(hts_data$person)))){
    print("`age` variable missing from data")
  }else if(cats==3){
    hts_data$person %<>% setDT() %>% 
      .[, age_bin3:=factor(
        fcase(as.integer(safegsub(rgx_yr, as.character(age)))<=18, "Under 18 Years",
              as.integer(safegsub(rgx_yr, as.character(age)))<=64, "18-64 Years",
              !is.na(age),                                         "65 years or  older"),
        levels=c("Under 18 Years","18-64 Years","65 years or  older"))]
  }else if(cats==5){
    hts_data$person %<>% setDT() %>%
      .[, age_bin5:=factor(
        fcase(as.integer(safegsub(rgx_yr, as.character(age)))<=18, "Under 18 Years",
              as.integer(safegsub(rgx_yr, as.character(age)))<=24, "18-24 Years",
              as.integer(safegsub(rgx_yr, as.character(age)))<=44, "25-44 Years",
              as.integer(safegsub(rgx_yr, as.character(age)))<=64, "45-64 Years",
              !is.na(age),                                         "65+"),
        levels=c("Under 18 Years","18-24 Years","25-44 Years",
                "45-64 Years","65 years or  older"))]
  }
  return(hts_data)
}

#' Add simplified worker status variable
#' Requires `employment` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified worker status variable
#' @author Michael Jensen
#' @export
hts_bin_worker <- function(hts_data){
  employment <- worker <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^employment$", colnames(hts_data$person)))){
    print("`employment` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, worker:=factor(
        fcase(grepl("^(Self-)?[eE]mployed", as.character(employment)), "Worker",
              !is.na(employment),                                      "Not Worker"),
        levels=c("Worker","Not Worker"))]
  }
  return(hts_data)
}

#' Add simplified education variable
#' Requires `education` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified education variable
#' @author Michael Jensen
#' @export
hts_bin_edu <- function(hts_data){
  education <- edu_bin2 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^education$", colnames(hts_data$person)))){
    print("`education` variable missing from data")

  }else{
  hts_data$person %<>% setDT() %>%
    .[, edu_bin2:=factor(
      fcase(grepl("^(Bach|Grad)", as.character(education)), "Bachelors or higher",
            !is.na(education),                              "Less than Bachelors degree"),
      levels=c("Less than Bachelors degree","Bachelors or higher"))]
  }
  return(hts_data)
}

#' Add simplified household size variable
#' Requires `hhsize` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified household size variable
#' @author Michael Jensen
#' @export
hts_bin_hhsize <- function(hts_data){
  hhsize <- hhsize_bin4 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^hhsize$", colnames(hts_data$hh)))){
    print("`hhsize` variable missing from data")
  }else{
    rgx_size <- "^(\\d+) (or more )?pe(ople|rson)"
    hts_data$hh %<>% setDT() %>%
      .[, hhsize_bin4:=factor(
        fcase(as.integer(safegsub(rgx_size, as.character(hhsize))) > 3, "4+ people",
              as.integer(safegsub(rgx_size, as.character(hhsize))) %between% c(1,3), as.character(hhsize)),
        levels=c("1 person","2 people","3 people","4+ people"))]
  }
  return(hts_data)
}

#' Add simplified gender variable
#' Requires `gender` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified gender variable
#' @author Michael Jensen
#' @export
hts_bin_gender <- function(hts_data){
  gender <- gender_bin3 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^gender$", colnames(hts_data$person)))){
    print("`gender` variable missing from data")
  }else{
  hts_data$person %<>% setDT() %>%
    .[, gender_bin3:=factor(
      fcase(grepl("^Boy/Man", as.character(gender)),    "Male",
            grepl("^Girl/Woman", as.character(gender)), "Female",
            !is.na(gender), "Non-binary, another, prefer not to answer"),
      levels=c("Male","Female","Non-binary, another, prefer not to answer"))]
  }
  return(hts_data)
}

#' Add simplified commute frequency variable
#' Requires `commute_freq` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified commute frequency variable
#' @author Michael Jensen
#' @export
hts_bin_commute_freq <- function(hts_data){
  commute_freq <- commute_freq_bin6 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^commute_freq$", colnames(hts_data$person)))){
    print("`commute_freq` variable missing from data")
  }else{
    rgx_days <- ".*(\\d+) day.* ?"
    hts_data$person %<>% setDT() %>%
      .[, commute_freq_bin6:=factor(
        fcase(as.integer(safegsub(rgx_days, as.character(commute_freq))) %between% c(1,4), as.character(commute_freq),
              as.integer(safegsub(rgx_days, as.character(commute_freq)))>=5,               "5+ days",
              !is.na(commute_freq),                                                      "0 days or less than weekly"),
                           levels=c("5+ days","4 days a week","3 days a week","2 days a week",
                                    "1 day a week","0 days or less than weekly"))]
  }
  return(hts_data)
}

#' Add simplified commute mode variable
#' Requires `commute_mode` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified commute mode variable
#' @author Michael Jensen
#' @export
hts_bin_commute_mode <- function(hts_data){
  commute_mode <- commute_mode_bin5 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^commute_mode$", colnames(hts_data$person)))){
    print("`commute_mode` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, commute_mode_bin5:=factor(
        fcase(grepl("^(Walk|Bicycle)", as.character(commute_mode)),   "Walk/Bike",
              grepl("^Carpool ",       as.character(commute_mode)),   "Carpool",
              grepl("\\b(bus|rail|ferry|streetcar|paratransit|vanpool)\\b", 
                    as.character(commute_mode), ignore.case=TRUE),    "Transit",
              grepl("^(Drive alone|Household vehicle|Other vehicle)", 
                    as.character(commute_mode)),                      "Drive alone",
              !is.na(commute_mode),                                   "Other"),
        levels=c("Drive alone","Carpool","Walk/Bike","Transit","Other"))]
  }
  return(hts_data)
}

#' Add simplified telework time variable
#' Requires `telework_time` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified telework time variable
#' @author Michael Jensen
#' @export
hts_bin_telework_time <- function(hts_data){
  telework_time <- telework_time_bin3 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^telework_time$", colnames(hts_data$day)))){
    print("`telework_time` variable missing from data")
  }else{
    hts_data$day %<>% setDT() %>%
      .[, telework_time_bin3:=factor(
        fcase(as.integer(safegsub("^(\\d+) hours?( \\d+ minutes)?", 
                                  as.character(telework_time))) >5,               "6+ hours",
              as.integer(safegsub("^(\\d+) hours?( \\d+ minutes)?", 
                                  as.character(telework_time))) %between% c(3,6), "3-6 hours",
              as.character(telework_time)=="1-6 hours",                           "3-6 hours",
              !is.na(telework_time),                                              "0-3 hours"),
        levels=c("0-3 hours","3-6 hours","6+ hours"))]
  }
  return(hts_data)
}

#' Add simplified vehicle count variable
#' Requires `vehicle_count` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified vehicle count variable
#' @author Michael Jensen
#' @export
hts_bin_vehicle_count <- function(hts_data){
  vehicle_count <- vehicle_count_bin4 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^vehicle_count$", colnames(hts_data$hh)))){
    print("`vehicle_count` variable missing from data")
  }else{
    rgx_veh <- "^(\\d+) .*$"
    hts_data$hh %<>% setDT() %>%
      .[, vehicle_count_bin4:=factor(
        fcase(as.integer(safegsub(rgx_veh, as.character(vehicle_count))) >3, "4+",
              !is.na(vehicle_count), safegsub(rgx_veh, as.character(vehicle_count))),
        levels=c("0","1","2","3","4+"))]
  }
  return(hts_data)
}