# Retrieve PSRC travel survey data

Retrieve PSRC travel survey data

## Usage

``` r
get_psrc_hts(survey_years = c(2017, 2019, 2021, 2023, 2025), survey_vars)
```

## Arguments

- survey_years:

  vector of 4-digit survey years. Specify any combinations explicitly
  (e.g. 1719 for combined 2017/2019)

- survey_vars:

  desired variables (in addition to id fields, which are returned by
  default)

## Value

surveyDataTools data object, i.e. named list

## Author

Michael Jensen
