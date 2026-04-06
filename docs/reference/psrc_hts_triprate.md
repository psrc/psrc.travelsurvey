# Summarize PSRC travel survey trip rates

Summarize PSRC travel survey trip rates

## Usage

``` r
psrc_hts_triprate(hts_data, group_vars = NULL, incl_na = TRUE)
```

## Arguments

- hts_data:

  the data object, a list with either data.table or NULL for hh, person,
  day, trip, vehicle

- group_vars:

  vector with names of one or more grouping variables, in order

- incl_na:

  logical, whether NA should be included in results–including calculated
  shares

## Value

summary table

## Author

Michael Jensen
