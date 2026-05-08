# Summarize PSRC transportation safety & security survey data

Summarize PSRC transportation safety & security survey data

## Usage

``` r
psrc_sss_stat(sss_data, group_vars = NULL, stat_var = NULL, incl_na = TRUE)
```

## Arguments

- sss_data:

  data.table returned by \`get_psrc_sss()\`

- group_vars:

  vector with names of one or more grouping variables, in order

- stat_var:

  string, name of numeric variable for min/max/median/mean; implicit for
  count/share

- incl_na:

  logical, whether NA should be included in results–including calculated
  shares

## Value

summary table

## Author

Michael Jensen
