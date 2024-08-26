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