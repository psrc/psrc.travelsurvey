# Retrieve and summarize sss data

## For use with the 2025 PSRC Safety & Security Survey

The Safety & Security Survey is a more traditional attitudinal survey,
with responses at a person level, rather than multiple related tables.
(The **travelSurveyTools** package is not used in handling it.) Use
[`get_psrc_sss()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_sss.md)
to retrieve the variables you need, then use
[`psrc_sss_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_sss_stat.md)
to calculate weighted counts, shares, or numeric summaries.

Because the package queries Elmer, you’ll need to be connected in office
or through VPN to run the retrieval step.

## Data retrieval

Since psrc.travelsurvey uses Elmer, the agency’s central database,
you’ll need to be connected in office or through VPN. Then use
[`get_psrc_sss()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_sss.md),
which takes a single argument:

- **survey_vars** - a vector of desired survey variable names

The return object is a person-level `data.table`. `person_id`, `hh_id`,
`sample_segment`, and `person_weight` are included automatically, so
they do not need to be added to `survey_vars`.

``` r

library(psrc.travelsurvey)
library(magrittr)
library(dplyr)

vars <- c("employment", "crash_participant", "travel_anxiety", "safety_choices", "crash_number_people")

sss_data <- get_psrc_sss(survey_vars = vars)
```

## Summarization

[`psrc_sss_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_sss_stat.md)
assumes a person-level analysis unit. The main arguments are:

- **sss_data** - the table returned by
  [`get_psrc_sss()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_sss.md)
- **group_vars** - one or more grouping variables, in nesting order
- **stat_var** (optional) - a numeric variable for min, max, median, and
  mean summaries

Like
[`psrc_hts_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_stat.md),
it uses the last grouping variable as the share variable when `stat_var`
is omitted.

### Count and share example

This example estimates the weighted distribution of `travel_anxiety`
responses within each `crash_participant` category.

``` r

rs1 <- psrc_sss_stat(
  sss_data,
  group_vars = c("crash_participant", "travel_anxiety"),
  incl_na = FALSE
)

head(rs1)
```

The resulting table can be read as an association summary: within each
`crash_participant` group, compare the weighted shares across
`travel_anxiety` response levels.

### Numeric summary example

TSS variables are primarily dichotomous or ordinal, but
[`psrc_sss_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_sss_stat.md)
can also calculate handle weighted means for any numeric variables,
using the `stat_var` argument.

``` r

sss_data <- mutate(sss_data, crash_number_people_numeric = if_else(
    is.na(crash_number_people), NA_integer_, as.integer(stringr::str_extract(crash_number_people, "^\\d+"))))

rs2 <- psrc_sss_stat(
  sss_data,
  group_vars = "crash_participant",
  stat_var = "crash_number_people_numeric",
  incl_na = FALSE
)

head(rs2)
```

## Notes

Margin-of-error columns are returned where the underlying survey
calculation produces standard errors; if a statistic cannot support a
variance estimate, its MOE field will be `NA`.
