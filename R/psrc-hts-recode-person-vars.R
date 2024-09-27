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
  age <- age_bin3 <- age_bin5 <- NULL # Bind variables locally for CMD check
  rgx_yr <- "^.*\\b(\\d+) years.*$"
  if(!any(grepl("^age$", colnames(hts_data$person)))){
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
  if(!any(grepl("^employment$", colnames(hts_data$person)))){
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
  if(!any(grepl("^education$", colnames(hts_data$person)))){
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
    labelled::var_label(hts_data$hh$hhsize_bin4) <- "Household size"
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
  if(!any(grepl("^sexuality$", colnames(hts_data$person)))){
    print("`sexuality` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, sexuality_bin3:=factor(
        fcase(grepl("^(Heterosexual|Missing)", as.character(sexuality)), as.character(sexuality),
              !is.na(sexuality), "Bisexual, gay, lesbian, queer, don't know, something else"),
        levels=c("Heterosexual (straight)","Missing","Bisexual, gay, lesbian, queer, don't know, something else"))]
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
    labelled::var_label(hts_data$person$commute_mode_bin5) <- "Method of commuting to work location/office last week"
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
    labelled::var_label(hts_data$day$telework_time_bin3) <- "Telework time on travel day"
  }
  return(hts_data)
}

#' Add simplified telework time variable
#' Requires `telecommute_freq` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified telework time variable
#' @author Michael Jensen
#' @export
hts_bin_telecommute_freq <- function(hts_data){
  telecommute_freq <- telecommute_freq_bin4 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^telecommute_freq$", colnames(hts_data$day)))){
    print("`telecommute_freq` variable missing from data")
  }else{
    hts_data$day %<>% setDT() %>%
      .[, telecommute_freq_bin4:=factor(
        fcase(grepl("^(N|Less|A few)", telecommute_freq),             "Never or less than weekly",
              as.integer(safegsub("^(\\d) days?", as.character(telecommute_freq))) >=5, "5+ days",
              as.integer(safegsub("^(\\d) days?", 
                                  as.character(telecommute_freq))) %between% c(3,4),   "3-4 days",
              as.integer(safegsub("^(\\d) days?", 
                                  as.character(telecommute_freq))) %between% c(1,2),  "1-2 days"),
        levels=c("5+ days","3-4 days","1-2 days","Never or less than weekly"))]
    labelled::var_label(hts_data$day$telecommute_freq_bin4) <- "How many days typically working from home"
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
    labelled::var_label(hts_data$hh$vehicle_count_bin4) <- "Number of vehicles"
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
  if(!any(grepl("^industry$", colnames(hts_data$person)))){
    print("`industry` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, lum_sector:=factor(
        fcase(grepl("^(Gov|Mil)"), as.character(industry),                    "Gov",
              grepl("^(Fin|Real|Prof|Landsc|Tech)"), as.character(industry),  "Business Services",
              grepl("^(Pers|Sport|Soci|Art|Media)"), as.character(industry),  "Personal Services",
              grepl("^Hospitality"), as.character(industry),                  "Food Services",
              grepl("^Natural"), as.character(industry),                      "Natural Resources",
              grepl("care\\b"), as.character(industry),                       "Healthcare",
              as.character(industry)=="Public Education",                     "Educ",
              as.character(industry)=="Private Education",                    "Private Ed",
              as.character(industry) %in% c("Retail","Construction","Other"), as.character(industry),
              as.character(industry)=="Transportation and utilities",         "WTU",
              grepl("^Manufacturing"), as.character(industry),                "Manuf",
              !is.na(industry),                                               as.character(industry)),
        levels=c("Natural Resources","Construction","Manuf","Retail","WTU","Healthcare","Private Ed",
                 "Business Services","Personal Services","Food Services","Other","Educ","Gov"))]
    labelled::var_label(hts_data$person$lum_sector) <- "Industry Sector (Land Use Modeling)"
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
  if(!any(grepl("^industry$", colnames(hts_data$person)))){
    print("`industry` variable missing from data")
  }else{
    hts_data$person %<>% setDT() %>%
      .[, lum_sector:=factor(
        fcase(grepl("^(Natural|Constr|Manuf|Trans)"), as.character(industry),         "Construction & Manufacturing",
              grepl("^(Fin|Real|Prof|Landsc|Tech)"), as.character(industry),          "Professional & Business Services",
              grepl("^(Pers|Hospitality|Sport|Soci|Retail)"), as.character(industry), "Retail & Personal Services",              
              grepl("^(Art|Media)"), as.character(industry),                          "Arts & Media", 
              grepl("^(Pers|Hospitality|Sport|Soci|Retail)"), as.character(industry), "Retail & Personal Services",
              grepl("(care\\b|education$)"), as.character(industry),                  "Healthcare & Education",
              !is.na(industry),                                                       as.character(industry),
              grepl("^(Gov|Mil)"), as.character(industry),                            "Gov"),
        levels=c("Construction & Manufacturing","Professional & Business Services","Retail & Personal Services",
                 "Arts & Media","Retail & Personal Services","Healthcare & Education","Other","Gov"))]
    labelled::var_label(hts_data$person$industry_sector) <- "Industry Sector"
  }
  return(hts_data)
}

#' Add destination purposeclassification
#' Requires `dest_purpose` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified vehicle count variable
#' @author Michael Jensen
#' @export
hts_bin_dest_purpose <- function(hts_data){
  dest_purpose <- dest_purpose_bin9 <- dest_purpose_bin4 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^dest_purpose$", colnames(hts_data$trip)))){
    print("`dest_purpose` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>% 
      .[, dest_purpose_bin11:=factor(
        fcase(grepl("^Went (home|to another(residence|temporary))"), as.character(dest_purpose),       "Home",
              grepl("^(Attend|Went) .* (college|school|education|class)"), as.character(dest_purpose), "School",
              as.character(dest_purpose)=="Went to primary workplace",                                 "Primary work",
              grepl("Went to( other)? work-related"), as.character(dest_purpose),                      "Work-related",
              grepl("([pP]ick(ed)? up)|([dD]rop(ped)? off)"), as.character(dest_purpose),              "Pick up/Drop off",
              grepl("(shopping|^Got gas)"), as.character(dest_purpose),                                "Shopping",              
              grepl("\\beat\\b", as.character(dest_purpose)),                                          "Eat Meal",
              grepl("([sS]ocial|[rR]ecreation|exercise|[vV]olunteer|Vacation|family activity)", 
                    as.character(dest_purpose)),                                                       "Social/Recreation",
              !is.na(dest_purpose),                                                                    "Errands/Appointments/Other"),
        levels=c("Home","Primary work","Work-related","School","Pick up/Drop off",
                 "Shopping","Social/Recreation", "Errands/Appointments/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin11) <- "Destination Purpose"
    hts_data$trip %>%
      .[, dest_purpose_bin4:=factor(
        fcase(as.character(dest_purpose_bin11)=="Home",                               "Home",
              as.character(dest_purpose_bin11) %in% c("Primary work","Work-related"), "Work",
              grepl("^(Eat|Social)"), as.character(dest_purpose_bin11),               "Social, Recreation, Eat Meal",
              !is.na(dest_purpose_bin11),                                             "Errands/Appointments/Other"),
        levels=c("Home","Work","Social, Recreation, Eat Meal","Errands/Appointments/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin4) <- "Destination Purpose"
    }
  return(hts_data)
}
