#' @importFrom magrittr %<>% %>%
NULL

`%not_in%` <- Negate(`%in%`)

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
      .[, hhincome_bin5:=factor(fcase(grepl("$(1|2)00,", as.character(hhincome_broad)), "$100,000 or more",
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
#' @return hts_data with an additional binned age field
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
      .[, age_bin3:=factor(fcase(as.integer(safegsub(rgx_yr, as.character(age)))<=18, "Under 18 Years",
                                 as.integer(safegsub(rgx_yr, as.character(age)))<=64, "18-64 Years",
                                 !is.na(age),                                         "65 years or  older"),
                           levels=c("Under 18 Years","18-64 Years","65 years or  older"))]
  }else if(cats==5){
    hts_data$person %<>% setDT() %>%
      .[, age_bin5:=factor(fcase(as.integer(safegsub(rgx_yr, as.character(age)))<=18, "Under 18 Years",
                                 as.integer(safegsub(rgx_yr, as.character(age)))<=24, "18-24 Years",
                                 as.integer(safegsub(rgx_yr, as.character(age)))<=44, "25-44 Years",
                                 as.integer(safegsub(rgx_yr, as.character(age)))<=64, "45-64 Years",
                                 !is.na(age),                                         "65+"),
                           levels=c("Under 18 Years","18-24 Years","25-44 Years",
                                   "45-64 Years","65 years or  older"))]
  }
  return(hts_data)
}

#' Add simplified worker variable
#' Requires `employment` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified 'worker' variable
#' @author Michael Jensen
#' @export
hts_bin_worker <- function(hts_data){
  employment <- worker <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^employment$", colnames(hts_data$person)))){
    print("`employment` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, worker:=factor(fcase(grepl("^(Self-)?[eE]mployed", as.character(employment)), "Worker",
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
    .[, edu_bin2:=factor(fcase(grepl("^(Bach|Grad)", as.character(education)), "Bachelors or higher",
                               !is.na(education),                              "Less than Bachelors degree"),
                         levels=c("Less than Bachelors degree","Bachelors or higher"))]
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
    .[, gender_bin3:=factor(fcase(grepl("^Boy/Man", as.character(gender)),    "Male",
                                  grepl("^Girl/Woman", as.character(gender)), "Female",
                                 !is.na(gender), "Non-binary, another, prefer not to answer"),
                           levels=c("Male","Female","Non-binary, another, prefer not to answer"))]
  }
  return(hts_data)
}
