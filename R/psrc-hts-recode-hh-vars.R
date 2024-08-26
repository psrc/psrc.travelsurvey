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
    labelled::var_label(hts_data$hh$hhincome_bin5) <- "Nominal Household Income"
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