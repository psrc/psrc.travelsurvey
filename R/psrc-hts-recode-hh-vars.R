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

#' Add binned income variables
#' Requires `hhincome_broad` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with binned income variables
#' @importFrom dplyr coalesce
#' @author Michael Jensen
#' @export
hts_bin_income <- function(hts_data){
  hhincome_broad <- hhincome_bin5 <- hhincome_bin3 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^hhincome_broad$", colnames(hts_data$hh)))){
    print("`hhincome_broad` variable missing from data")
  }else{
    hts_data$hh %<>% setDT() %>%
      .[, hhincome_bin5:=factor(
        fcase(grepl("\\$(1|2)00,", as.character(hhincome_broad)), "$100,000 or more",
              !is.na(hhincome_broad),                   as.character(hhincome_broad)),
        levels=c("Under $25,000","$25,000-$49,999","$50,000-$74,999",
                 "$75,000-$99,999","$100,000 or more","Prefer not to answer"))]
    labelled::var_label(hts_data$hh$hhincome_bin5) <- "Nominal Household Income"
    hts_data$hh %<>%
      .[, hhincome_bin3:=factor(
      fcase(grepl("\\$25", as.character(hhincome_bin5)), "Less than $50,000",
            grepl("\\$7", as.character(hhincome_bin5)), "$50,000-$99,999",
            !is.na(hhincome_bin5), as.character(hhincome_bin5)),
      levels= c("Less than $50,000","$50,000-$99,999",
                "$100,000 or more","Prefer not to answer"))]
    labelled::var_label(hts_data$hh$hhincome_bin3) <- "Nominal Household Income"
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

#' Add simplified vehicle count variables
#' Requires `vehicle_count` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with simplified vehicle count variables
#' @author Michael Jensen
#' @export
hts_bin_vehicle_count <- function(hts_data){
  vehicle_count <- vehicle_count_bin4 <- veh_yn <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^vehicle_count$", colnames(hts_data$hh)))){
    print("`vehicle_count` variable missing from data")
  }else{
    rgx_veh <- "^(\\d+).*"
    hts_data$hh %<>% setDT() %>% .[, `:=`(
      vehicle_count_bin4=factor(
        fcase(as.integer(safegsub(rgx_veh, as.character(vehicle_count))) >3, "4+",
              !is.na(vehicle_count), safegsub(rgx_veh, as.character(vehicle_count))),
        levels=c("0","1","2","3","4+")),
      veh_yn=factor(
        fcase(vehicle_count=="0 (no vehicles)", "No vehicle",
              !is.na(vehicle_count),            "1+ vehicle"),
        levels= c("No vehicle","1+ vehicle")))]
    labelled::var_label(hts_data$hh$vehicle_count_bin4) <- "Number of vehicles"
    labelled::var_label(hts_data$hh$veh_yn) <- "Presence or absence of own vehicle(s)"   
  }
  return(hts_data)
}

#' Add simplified tenure variable
#' Requires `rent_own` variable
#'
#' @param hts_data the hts_data list object
#' @return hts_data with a simplified tenure variable
#' @author Michael Jensen
#' @export
hts_bin_rent_own <- function(hts_data){
  rent_own <- rent_own_bin2 <- NULL # Bind variables locally for CMD check
  if(!any(grepl("^rent_own$", colnames(hts_data$hh)))){
    print("`rent_own` variable missing from data")
  }else{
    hts_data$hh %<>% setDT() %>%
      .[, rent_own_bin2:=factor(
        fcase(grepl("^Provided", rent_own) ,"Rent",
              grepl("^(Other|Prefer)", as.character(rent_own)), NA_character_,
              !is.na(rent_own), as.character(rent_own)))]
    labelled::var_label(hts_data$hh$rent_own_bin2) <- "Housing tenure"
  }
  return(hts_data)
}
