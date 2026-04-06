# Summarize PSRC travel survey data

Summarize PSRC travel survey data

## Usage

``` r
psrc_hts_stat(
  hts_data,
  analysis_unit,
  group_vars = NULL,
  stat_var = NULL,
  incl_na = TRUE
)
```

## Arguments

- hts_data:

  the data object, a list with either data.table or NULL for hh, person,
  day, trip, vehicle

- analysis_unit:

  string, either "hh", "person", "day", "trip", or "vehicle"

- group_vars:

  vector with names of one or more grouping variables, in order

- stat_var:

  string, name of numeric variable for sum, median, mean; implicit for
  count

- incl_na:

  logical, whether NA should be included in results–including calculated
  shares

## Value

summary table

## Author

Michael Jensen
