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

#' Add destination purpose classifications
#' Requires `dest_purpose` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified destination purpose variable
#' @author Michael Jensen
#' @export
hts_bin_dest_purpose <- function(hts_data){
  dest_purpose<- dest_purpose_bin12 <- dest_purpose_bin9 <- dest_purpose_bin4 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^dest_purpose$", colnames(hts_data$trip)))){
    print("`dest_purpose` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>% 
      .[, dest_purpose_bin12:=factor(
        fcase(grepl("^Went home", as.character(dest_purpose)),                                         "Home",
              grepl("^Went to another(residence|temporary)", as.character(dest_purpose)),              "Overnight",
              grepl("^(Attend|Went) .* (college|school|education|class)", as.character(dest_purpose)), "School",
              as.character(dest_purpose)=="Went to primary workplace",                                 "Primary work",
              grepl("Went to( other)? work-related", as.character(dest_purpose)),                      "Work-related",
              grepl("^(Changed |Transferred to another) mode", as.character(dest_purpose)),            "Changed mode",
              grepl("([pP]ick(ed)? up)|([dD]rop(ped)? off)", as.character(dest_purpose)),              "Pick up/Drop off",
              grepl("(shopping|^Got gas)", as.character(dest_purpose)),                                "Shopping",              
              grepl("\\beat\\b", as.character(dest_purpose)),                                          "Eat Meal",
              grepl("([sS]ocial|[rR]ecreation|exercise|[vV]olunteer|Vacation|family activity)", 
                    as.character(dest_purpose)),                                                       "Social/Recreation",
              grepl("^([oO]ther( (purpose|reason))?$)", as.character(dest_purpose)),                   "Other",
              !is.na(dest_purpose),                                                                    "Errands"),
        levels=c("Home", "Overnight", "Primary work", "Work-related", "School", "Pick up/Drop off",
                 "Changed mode", "Shopping", "Eat Meal", "Social/Recreation", "Errands", "Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin12) <- "Destination Purpose - Modeling"
    hts_data$trip %<>% setDT() %>% 
      .[, dest_purpose_bin9:=factor(    
        fcase(as.character(dest_purpose_bin12) %in% c("Home","Overnight"),                             "Home",
              as.character(dest_purpose_bin12) %in% c("Errands","Changed mode","Other"),               "Errands/Other",
              !is.na(dest_purpose_bin12),                                              as.character(dest_purpose_bin12)),
        levels=c("Home", "Primary work", "Work-related", "School", "Pick up/Drop off",
                 "Shopping", "Eat Meal", "Social/Recreation", "Errands/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin9) <- "Destination Purpose"    
    hts_data$trip %>%
      .[, dest_purpose_bin4:=factor(
        fcase(as.character(dest_purpose_bin9)=="Home",                                                 "Home",
              as.character(dest_purpose_bin9) %in% c("Primary work","Work-related"),                   "Work",
              grepl("^(Eat|Social)", as.character(dest_purpose_bin9)),                                 "Social/Recreation/Meal",
              !is.na(dest_purpose_bin9),                                                               "Errands/Other"),
        levels=c("Home","Work","Social/Recreation/Meal","Errands/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin4) <- "Destination Purpose"
  }
  return(hts_data)
}

#' Add simplified mode classification
#' Requires `mode_characterization` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified mode variable
#' @author Michael Jensen
#' @export
hts_bin_mode <- function(hts_data){
  mode_characterization <- mode_basic <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^mode_characterization$", colnames(hts_data$trip)))){
    print("`mode_characterization` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>% 
      .[, mode_basic:=factor(
        fcase(mode_characterization=="Airplane",                          NA_character_,
              grepl("HOV", as.character(mode_characterization)),          "Carpool",
              as.character(mode_characterization)=="Drive SOV",           "Drive alone",
              grepl("^(Walk|Bike)", as.character(mode_characterization)), "Walk/Bike/Micromobility",
              !is.na(mode_characterization),                   as.character(mode_characterization)),
        levels=c("Drive alone", "Carpool", "Walk/Bike/Micromobility"))]
    labelled::var_label(hts_data$trip$mode_basic) <- "Travel mode"
  }
  return(hts_data)
}
