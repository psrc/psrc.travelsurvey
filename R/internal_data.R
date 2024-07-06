#' steps for internal acs table lookup
#'
#' @name variable_value_lookup
#' @docType data
#' @keywords data
#' @importFrom readxl read_xlsx
#' @importFrom magrittr %>%
#' @importFrom data.table setDT
#' @importFrom dplyr rename
NULL

filepath <- "J:/Projects/Surveys/HHTravel/Survey2023/Data/data_published/PSRC_Codebook_2023_v1.xlsx"
init_variable_list <- read_xlsx(filepath, sheet="variable_list") %>% setDT() %>% setnames("hh", "household") 
init_value_labels <- read_xlsx(filepath, sheet="value_labels") %>% setDT() %>% setnames("label","value_label") 

#usethis::use_data(init_variable_list, init_value_labels, internal=TRUE, overwrite=TRUE)           # Makes part of the package; push this to repo
