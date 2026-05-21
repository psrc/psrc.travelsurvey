library(data.table)
library(psrc.travelsurvey)
library(psrcelmer)

sql_type_to_variable_type <- function(sql_type) {
  fifelse(
    sql_type %chin% c("date", "datetime", "datetime2", "smalldatetime", "datetimeoffset", "time"),
    "date",
    fifelse(
      sql_type %chin% c("char", "nchar", "varchar", "nvarchar", "text", "ntext", "uniqueidentifier"),
      "character",
      "numeric"
    )
  )
}

get_household_person_view_columns <- function() {
  sql <- paste(
    "SELECT",
    "  v.name AS view_name,",
    "  c.name AS variable,",
    "  t.name AS sql_type",
    "FROM sys.columns AS c",
    "INNER JOIN sys.views AS v",
    "  ON c.object_id = v.object_id",
    "INNER JOIN sys.types AS t",
    "  ON c.user_type_id = t.user_type_id",
    "WHERE SCHEMA_NAME(v.schema_id) = 'HHSurvey'",
    "  AND v.name IN ('v_households', 'v_persons')",
    sep = "\n"
  )

  psrcelmer::get_query(sql) %>%
    setDT()
}

build_missing_variable_rows <- function(variable_list, source_columns = get_household_person_view_columns()) {
  variable <- view_name <- sql_type <- data_type <- NULL

  source_columns <- as.data.table(copy(source_columns))
  variable_list <- as.data.table(copy(variable_list))

  missing_columns <- setdiff(c("view_name", "variable", "sql_type"), names(source_columns))
  if (length(missing_columns) > 0) {
    stop("Missing required source metadata columns: ", paste(missing_columns, collapse = ", "))
  }

  source_columns[
    , data_type := sql_type_to_variable_type(tolower(sql_type))
  ][
    !variable %in% variable_list$variable,
    .(
      is_checkbox = 0L,
      hh = as.integer(any(view_name == "v_households")),
      person = as.integer(any(view_name == "v_persons")),
      day = 0L,
      trip = 0L,
      vehicle = 0L,
      location = 0L,
      data_type = data_type[[1]],
      description = "",
      shared_name = variable
    ),
    by = variable
  ]
}

refresh_variable_list <- function(variable_list = psrc.travelsurvey:::variable_list) {
  variable_list <- as.data.table(copy(variable_list))
  new_rows <- build_missing_variable_rows(variable_list)

  if (nrow(new_rows) == 0) {
    return(variable_list)
  }

  rbindlist(
    list(variable_list, new_rows),
    use.names = TRUE,
    fill = TRUE
  )
}

# Run the lines below in an interactive R session when you want to refresh the package data.
# variable_list <- refresh_variable_list()
# value_labels <- psrc.travelsurvey:::value_labels
# usethis::use_data(variable_list, value_labels, internal = TRUE, overwrite = TRUE)