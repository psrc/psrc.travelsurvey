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
  if("dest_purpose" %not_in% colnames(hts_data$trip)){
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
#' Requires `mode_class` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified mode variable
#' @author Michael Jensen
#' @export
hts_bin_mode <- function(hts_data){
  mode_class <- mode_basic <- NULL # Bind variables locally for CMD check
  if("mode_class" %not_in% colnames(hts_data$trip)){
    print("`mode_class` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>% 
      .[, mode_basic:=factor(
        fcase(mode_class %in% c("Drive HOV2","Drive HOV3+"), "Carpool",
              mode_class=="Drive SOV", "Drive Alone",
              mode_class=="Transit", "Transit",
              !is.na(mode_class),  "Walk/Bike/Other"),
        levels=c("Walk/Bike/Other", "Transit", "Carpool", "Drive alone"))]
    labelled::var_label(hts_data$trip$mode_basic) <- "Travel mode"
  }
  return(hts_data)
}

#' Add simplified transit access mode classification
#' Requires `mode_acc` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified transit access mode variable
#' @author Michael Jensen
#' @export
hts_bin_transit_mode_acc <- function(hts_data){
  transit_mode_acc <- mode_acc <- mode_class <- NULL # Bind variables locally for CMD check
  if(!all(c("mode_acc","mode_class") %in% colnames(hts_data$trip))){
    print("`mode_acc` and/or `mode_class` variable missing from data")
  }else{
    hts_data$trip %<>% setDT() %>% 
      .[, transit_mode_acc:=factor(
        fcase((as.character(mode_class)!='Transit'|is.na(mode_class)), NA_character_,
              grepl("^Transferred", as.character(mode_acc)), NA_character_, # These aren't real access modes--indicate trip link or error
              grepl("([bB]i(ke|cycle)|[sS]cooter)", as.character(mode_acc)), "Bike/Micromobility",
              grepl("[wW]alk", as.character(mode_acc)), "Walked or jogged",
              !is.na(mode_acc), "Vehicular"),
        levels=c("Walked or jogged", "Bike/Micromobility", "Vehicular"))]
    labelled::var_label(hts_data$trip$transit_mode_acc) <- "Transit access mode"
  }
  return(hts_data)
}