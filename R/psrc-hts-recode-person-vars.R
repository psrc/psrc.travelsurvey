#' @importFrom magrittr %<>% %>%
#' @importFrom labelled var_label
NULL

`%not_in%` <- Negate(`%in%`)
`%between%`<- function(x, range) x>=range[1] & x<=range[2]

safegsub <- function(rgx, x){
  ans <- ifelse(grepl(rgx, x),
                gsub(rgx, "\\1", x),
                "")
  return(ans)
}

#' Add binned age variables
#' Requires `age` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with additional binned age variables
#' @author Michael Jensen
#' @export
hts_bin_age <- function(hts_data){
  age <- age_bin3 <- age_bin5 <- adult <- NULL # Bind variables locally for CMD check
  rgx_yr <- "^.*\\b(\\d+) years.*$"
  if("age" %not_in% colnames(hts_data$person)){
    print("`age` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, age_bin5:=factor(
          fcase(as.integer(safegsub(rgx_yr, as.character(age))) <18, "Under 18 Years",
                as.integer(safegsub(rgx_yr, as.character(age)))<=24, "18-24 Years",
                as.integer(safegsub(rgx_yr, as.character(age)))<=44, "25-44 Years",
                as.integer(safegsub(rgx_yr, as.character(age)))<=64, "45-64 Years",
                as.integer(safegsub(rgx_yr, as.character(age))) >64, "65 years or older",
                !is.na(as.character(age)), as.character(age)),
          levels=c("Under 18 Years","18-24 Years","25-44 Years",
                   "45-64 Years","65 years or older"))]        
    labelled::var_label(hts_data$person$age_bin5) <- "Age"       
    hts_data$person %<>%
      .[, age_bin3:=factor(
        fcase(grepl("^(18|25|45)", as.character(age_bin5)), "18-64 Years",
              !is.na(age_bin5), as.character(age_bin5)),
        levels=c("Under 18 Years","18-64 Years","65 years or older"))]
    labelled::var_label(hts_data$person$age_bin3) <- "Age"
    hts_data$person %<>%
      .[, adult:=fcase(any(substr(age_bin3, 1L, 2L) %in% c("18","65")), "Adult", 
                       !is.na(age_bin3), NA_character_)] 
    labelled::var_label(hts_data$person$adult) <- "Age 18 or older"
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
  if("employment" %not_in% colnames(hts_data$person)){
    print("`employment` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, worker:=factor(
        fcase(grepl("^(Self-)?[eE]mployed", as.character(employment)), "Worker",
              !is.na(employment),                                      "Not Worker"),
        levels=c("Worker","Not Worker"))]
    labelled::var_label(hts_data$person$worker) <- "Employment status"
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
  if("education" %not_in% colnames(hts_data$person)){
    print("`education` variable missing from data")
  }else{
  hts_data$person %<>% setDT() %>%
    .[, edu_bin2:=factor(
      fcase(grepl("^(Bach|Grad)", as.character(education)),        "Bachelors or higher",
            as.character(education)=="Prefer not to answer",      "Prefer not to answer",
            !is.na(education),                              "Less than Bachelors degree"),
      levels=c("Less than Bachelors degree","Bachelors or higher"))]
    labelled::var_label(hts_data$person$edu_bin2) <- "Educational attainment"
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
  if("gender" %not_in% colnames(hts_data$person)){
    print("`gender` variable missing from data")
  }else{
  hts_data$person %<>% setDT() %>%
    .[, gender_bin3:=factor(
      fcase(grepl("^Male|Boy", as.character(gender)),    "Boy/Man",
            grepl("^Female|Girl", as.character(gender)), "Girl/Woman",
            !is.na(gender), "Non-binary, another, prefer not to answer"),
      levels=c("Girl/Woman","Boy/Man","Non-binary, another, prefer not to answer"))]
    labelled::var_label(hts_data$person$gender_bin3) <- "Gender"
  }
  return(hts_data)
}

#' Add simplified sexuality variable
#' Requires `sexuality` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified sexuality variable
#' @author Michael Jensen
#' @export
hts_bin_sexuality <- function(hts_data){
  sexuality <- sexuality_bin3 <- NULL # Bind variables locally for CMD check
  if("sexuality" %not_in% colnames(hts_data$person)){
    print("`sexuality` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, sexuality_bin3:=factor(
        fcase(as.character(sexuality)=="Heterosexual (straight)", "Heterosexual (straight)",
              !is.na(sexuality), "Bisexual, gay, lesbian, queer, don't know, something else"),
        levels=c("Bisexual, gay, lesbian, queer, don't know, something else","Heterosexual (straight)"))]
    labelled::var_label(hts_data$person$sexuality_bin3) <- "sexuality"
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
  if("commute_freq" %not_in% colnames(hts_data$person)){
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
    labelled::var_label(hts_data$person$commute_freq_bin6) <- "How often commuted to workplace last week"
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
  if("commute_mode" %not_in% colnames(hts_data$person)){
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
    labelled::var_label(hts_data$person$commute_mode_bin5) <- "Method of commuting to work location/office last week"
  }
  return(hts_data)
}

#' Add simplified telework frequency variable
#' Requires `telecommute_freq` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified telework frequency variable
#' @author Michael Jensen
#' @export
hts_bin_telecommute_freq <- function(hts_data){
  telecommute_freq <- telecommute_freq_bin4 <- NULL # Bind variables locally for CMD check
  if("telecommute_freq" %not_in% colnames(hts_data$person)){
    print("`telecommute_freq` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, telecommute_freq_bin4:=factor(
        fcase(grepl("^(N|Less|A few)", telecommute_freq),             "Never or less than weekly",
              as.integer(safegsub("^(\\d)\\+? days?( a week)?", as.character(telecommute_freq))) >=5, "5+ days",
              as.integer(safegsub("^(\\d)\\+? days?( a week)?", 
                                  as.character(telecommute_freq))) %between% c(3,4),   "3-4 days",
              as.integer(safegsub("^(\\d)\\+? days?( a week)?", 
                                  as.character(telecommute_freq))) %between% c(1,2),  "1-2 days"),
        levels=c("5+ days","3-4 days","1-2 days","Never or less than weekly"))]
    labelled::var_label(hts_data$person$telecommute_freq_bin4) <- "How many days typically working from home"
  }
  return(hts_data)
}

#' Add telecommute status variable
#' Requires `telecommute_freq` and `workplace` variables
#'
#' @param hts_data the hts_data list object
#' @return hts_data with telecommute status variable
#' @author Michael Jensen
#' @export
hts_bin_telecommute_trichotomy <- function(hts_data){
  telecommute_freq <- workplace <- telecommute_trichotomy <- NULL # Bind variables locally for CMD check
  if("telecommute_freq" %not_in% colnames(hts_data$person)){
    print("`telecommute_freq` variable missing from data")
  }else if("workplace" %not_in% colnames(hts_data$person)){
    print("`workplace` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, telecommute_trichotomy:=factor(
        fcase(is.na(workplace), NA_character_,
              workplace=="At home (telecommute or self-employed with home office)", "Fully At Home", 
              grepl("^\\d+", as.character(telecommute_freq)), "Hybrid",
              !is.na(telecommute_freq), "Fully In Person"),
        levels=c("Fully At Home","Hybrid","Fully In Person"))]
    #labelled::var_label(hts_data$person$telecommute_trichotomy) <- ""
  }
  return(hts_data)
}

#' Add land use modeling industry classification
#' Requires `industry` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a land use modeling industry classification variable
#' @author Michael Jensen
#' @export
hts_bin_lum_sector <- function(hts_data){
  industry <- lum_sector <- NULL # Bind variables locally for CMD check
  if("industry" %not_in% colnames(hts_data$person)){
    print("`industry` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, lum_sector:=factor(
        fcase(grepl("^(Gov|Mil)", as.character(industry)),                    "Gov",
              grepl("^(Fin|Real|Prof|Landsc|Tech)", as.character(industry)),  "Business Services",
              grepl("^(Pers|Sport|Soci|Art|Media)", as.character(industry)),  "Personal Services",
              grepl("^Hospitality", as.character(industry)),                  "Food Services",
              grepl("^Natural", as.character(industry)),                      "Natural Resources",
              grepl("care\\b", as.character(industry)),                       "Healthcare",
              as.character(industry)=="Public Education",                     "Educ",
              as.character(industry)=="Private Education",                    "Private Ed",
              as.character(industry) %in% c("Retail","Construction","Other"), as.character(industry),
              as.character(industry)=="Transportation and utilities",         "WTU",
              grepl("^Manufacturing", as.character(industry)),                "Manuf",
              !is.na(industry),                                               as.character(industry)),
        levels=c("Natural Resources","Construction","Manuf","Retail","WTU","Healthcare","Private Ed",
                 "Business Services","Personal Services","Food Services","Other","Educ","Gov"))]
    labelled::var_label(hts_data$person$lum_sector, .strict=FALSE) <- "Industry Sector (Land Use Modeling)"
  }
  return(hts_data)
}

#' Add generalized industry classification
#' Requires `industry` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a generalized industry classification variable
#' @author Michael Jensen
#' @export
hts_bin_industry_sector <- function(hts_data){
  industry <- industry_sector <- NULL # Bind variables locally for CMD check
  if("industry" %not_in% colnames(hts_data$person)){
    print("`industry` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, industry_sector:=factor(
        fcase(grepl("^(Natural|Constr|Manuf|Trans)", as.character(industry)),         "Construction & Manufacturing",
              grepl("^(Fin|Real|Prof|Landsc|Tech)", as.character(industry)),          "Professional & Business Services",
              grepl("^(Pers|Hospitality|Sport|Soci|Retail)", as.character(industry)), "Retail & Personal Services",              
              grepl("^(Art|Media)", as.character(industry)),                          "Arts & Media", 
              grepl("^(Pers|Hospitality|Sport|Soci|Retail)", as.character(industry)), "Retail & Personal Services",
              grepl("(care\\b|education$)", as.character(industry)),                  "Healthcare & Education",
              !is.na(industry),                                                       as.character(industry),
              grepl("^(Gov|Mil)", as.character(industry)),                            "Gov"),
        levels=c("Construction & Manufacturing","Professional & Business Services","Retail & Personal Services",
                 "Healthcare & Education","Arts & Media","Other","Gov"))]
    labelled::var_label(hts_data$person$industry_sector) <- "Industry Sector"
  }
  return(hts_data)
}

#' Add generalized transit frequency
#' Requires `transit_frequency` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a generalized transit frequency variable
#' @author Michael Jensen
#' @export
hts_bin_transit_frequency <- function(hts_data){
  transit_frequency <- transit_frequency_bin4 <- NULL # Bind variables locally for CMD check
  if("transit_frequency" %not_in% colnames(hts_data$person)){
    print("`transit_frequency` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, transit_frequency_bin4:=factor(
        fcase(safegsub("\\d?-?(\\d) days? a week$", as.character(transit_frequency)) %between% c(1,4), "1-4 days a week",
              safegsub("\\d?-?(\\d) days a week$", as.character(transit_frequency)) %between% c(5,7), "5-7 days a week",
              !is.na(transit_frequency), as.character(transit_frequency)),
        levels=c("1-4 days a week", "5-7 days a week", "1-3 days in the past month", "Never in the past 30 days"))]
    labelled::var_label(hts_data$person$transit_frequency_bin4) <- "Frequency of transit use"
  }
  return(hts_data)
}

#' Add generalized walk frequency
#' Requires `walk_frequency` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a generalized walk frequency variable
#' @author Michael Jensen
#' @export
hts_bin_walk_frequency <- function(hts_data){
  walk_frequency <- walk_frequency_bin4 <- NULL # Bind variables locally for CMD check
  if("walk_frequency" %not_in% colnames(hts_data$person)){
    print("`walk_frequency` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, walk_frequency_bin4:=factor(
        fcase(safegsub("\\d?-?(\\d) days? a week$", as.character(walk_frequency)) %between% c(1,4), "1-4 days a week",
              safegsub("\\d?-?(\\d) days a week$", as.character(walk_frequency)) %between% c(5,7), "5-7 days a week",
              !is.na(walk_frequency), as.character(walk_frequency)),
        levels=c("1-4 days a week", "5-7 days a week", "1-3 days in the past month", "Never in the past 30 days"))]
    labelled::var_label(hts_data$person$walk_frequency_bin4) <- "Frequency of walk use"
  }
  return(hts_data)
}

#' Add generalized bike frequency
#' Requires `bike_frequency` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a generalized bike frequency variable
#' @author Michael Jensen
#' @export
hts_bin_bike_frequency <- function(hts_data){
  bike_frequency <- bike_frequency_bin4 <- NULL # Bind variables locally for CMD check
  if("bike_frequency" %not_in% colnames(hts_data$person)){
    print("`bike_frequency` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, bike_frequency_bin4:=factor(
        fcase(safegsub("\\d?-?(\\d) days? a week$", as.character(bike_frequency)) %between% c(1,4), "1-4 days a week",
              safegsub("\\d?-?(\\d) days a week$", as.character(bike_frequency)) %between% c(5,7), "5-7 days a week",
              !is.na(bike_frequency), as.character(bike_frequency)),
        levels=c("1-4 days a week", "5-7 days a week", "1-3 days in the past month", "Never in the past 30 days"))]
    labelled::var_label(hts_data$person$bike_frequency_bin4) <- "Frequency of bike use"
  }
  return(hts_data)
}
