#' steps for internal acs table lookup
#'
#' @name variable_value_lookup
#' @docType data
#' @keywords data
#' @importFrom magrittr %>%
#' @importFrom data.table setDT fread
NULL

# variable_list <- fread("final_variable_list_2025.csv") %>% setDT() %>% .[, logic:=NULL]
# variable_list[is.na(shared_name), shared_name:=variable]
# value_labels <- fread("final_value_labels_2025.csv") %>% setDT()

# usethis::use_data(variable_list, value_labels, internal=TRUE, overwrite=TRUE)           # Makes part of the package; push this to repo
