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
        fcase(grepl("^Went (home|to another(residence|temporary))", as.character(dest_purpose)),       "Home",
              grepl("^(Attend|Went) .* (college|school|education|class)", as.character(dest_purpose)), "School",
              as.character(dest_purpose)=="Went to primary workplace",                                 "Primary work",
              grepl("Went to( other)? work-related", as.character(dest_purpose)),                      "Work-related",
              grepl("([pP]ick(ed)? up)|([dD]rop(ped)? off)", as.character(dest_purpose)),              "Pick up/Drop off",
              grepl("(shopping|^Got gas)", as.character(dest_purpose)),                                "Shopping",              
              grepl("\\beat\\b", as.character(dest_purpose)),                                          "Eat Meal",
              grepl("([sS]ocial|[rR]ecreation|exercise|[vV]olunteer|Vacation|family activity)", 
                    as.character(dest_purpose)),                                                       "Social/Recreation",
              !is.na(dest_purpose),                                                                    "Errands/Other"),
        levels=c("Home","Primary work","Work-related","School","Pick up/Drop off",
                 "Shopping","Social/Recreation", "Errands/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin11) <- "Destination Purpose"
    hts_data$trip %>%
      .[, dest_purpose_bin4:=factor(
        fcase(as.character(dest_purpose_bin11)=="Home",                               "Home",
              as.character(dest_purpose_bin11) %in% c("Primary work","Work-related"), "Work",
              grepl("^(Eat|Social)", as.character(dest_purpose_bin11)),               "Social/Recreation/Meal",
              !is.na(dest_purpose_bin11),                                             "Errands/Other"),
        levels=c("Home","Work","Social/Recreation/Meal","Errands/Other"))]
    labelled::var_label(hts_data$trip$dest_purpose_bin4) <- "Destination Purpose"
  }
  return(hts_data)
}