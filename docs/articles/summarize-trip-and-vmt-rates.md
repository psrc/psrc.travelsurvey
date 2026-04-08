# Summarize trip and VMT rates

## Why rates? A convenient way to examine and compare

In addition to [aggregate counts and
shares](https://psrc.github.io/psrc.travelsurvey/articles/retrieve-and-summarize-data.html#summarization),
**psrc.travelsurvey** includes functions to calculate average daily
trip- and VMT (vehicle miles traveled) rates. These metrics are often
used in travel analysis and modeling. As a ratio (mean amount per person
per day), these rates are normalized in respect to both population and
days reported, while maintaining a unit scale so they can be interpreted
or compared in both absolute and relative terms. (In contrast, shares
have no unit and can only be compared in relative terms; counts aren’t
normalized and by themselves can’t be meaningfully compared across
subgroups or surveys.)

## psrc.travelsurvey functions

Trip and VMT rates involve summarization at the day level prior to
aggregate summarization across respondents, and have their own
functions:

- **[`psrc_hts_triprate()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_triprate.md)**
- **[`psrc_hts_vmtrate()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_vmtrate.md)**

Each function is simplified from
[`psrc_hts_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_stat.md);
since `analysis_unit` and `stat_var` for each are fixed/inherent, there
are only two arguments:

- **hts_data** - the `hts_data` object
- **group_vars** - any grouping variables, in nesting order (share
  summaries are calculated per categories of the variable listed last).

The `incl_na=FALSE` option is also available in both functions.

### Requirements

Because VMT is calculated from distance, is limited to vehicular travel,
and accounts for co-travelers, the preceding
[`get_psrc_hts()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_hts.md)
call must include the following among the `survey_vars` argument:

- **`mode_class`**
- **`trip_distance`**
- **`travelers_total`**.

If the hts_data object is missing one or more of these,
**[`psrc_hts_vmtrate()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_vmtrate.md)**
will report that to the user and return NULL.

Because count is an implicit trip attribute,
**[`psrc_hts_triprate()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_triprate.md)**
requires no additional variables.

### Examples

``` r
library(psrc.travelsurvey)
library(data.table)

# Specify which variables to retrieve
vars <- c("telecommute_freq", "workplace",
          "mode_class", "distance_miles", "travelers_total")

# Retrieve the data
hts_data <- get_psrc_hts(survey_vars = vars, survey_year = 2025)
hts_data <- hts_bin_telecommute_trichotomy(hts_data)

# Calculate a trip rate
rs1 <- psrc_hts_triprate(hts_data, group_vars="telecommute_trichotomy")

head(dplyr::select(rs1, -c(survey_year, count, min, max, median)))
#>    telecommute_trichotomy     mean  mean_moe
#>                    <fctr>    <num>     <num>
#> 1:                 Hybrid 4.269087 0.3116074
#> 2:        Fully In Person 4.453841 0.2493623
#> 3:                   <NA> 3.705679 0.1628969

# Calculate a VMT rate
rs2 <- psrc_hts_vmtrate(hts_data, "telecommute_trichotomy", incl_na=FALSE)

head(dplyr::select(rs2, -c(survey_year, count, min, max, median)))
#>    telecommute_trichotomy  mean mean_moe
#>                    <fctr> <num>    <num>
#> 1:                 Hybrid   Inf      NaN
#> 2:        Fully In Person   Inf      NaN
#> 3:                   <NA>   Inf      NaN
```
