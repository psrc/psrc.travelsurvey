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

# filepath <- "J:/Projects/Surveys/HHTravel/Survey2023/Data/data_published/PSRC_Codebook_2023_v1.xlsx"
# variable_list <- read_xlsx(filepath, sheet="variable_list") %>% setDT() %>% .[, logic:=NULL]
# variable_list[is.na(shared_name), shared_name:=variable]
# value_labels <- read_xlsx(filepath, sheet="value_labels") %>% setDT() %>% .[, 1:4]

# usethis::use_data(variable_list, value_labels, internal=TRUE, overwrite=TRUE)           # Makes part of the package; push this to repo
