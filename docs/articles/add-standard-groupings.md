# Add Standard Groupings

## Agency standard category schemes

In light of sample sizes, breakdowns from PSRC’s Household Travel Survey
instrument often involve category groupings with less than the most
granular reported level of detail. **psrc.travelsurvey** functions
provide the most commonly utilized groupings, which can be considered
agency standard for the variables of interest. Although each new
variable is specific to one unit of analysis (i.e. hh, person, day,
trip, vehicle), the first and only argument to each grouping function is
the entire `hts_data` object, in order to facilitate piping. When more
than one standard grouping exists for a variable, the function returns
them all, for convenience; they can be distinguished by a suffix
denoting the number of categories.

``` r
library(psrc.travelsurvey)
library(magrittr)
library(data.table)

# Specify which variables to retrieve
vars <- c("age", "hhsize")

# Retrieve the data
hts_data <- get_psrc_hts(survey_year=2025, survey_vars = vars) 

# Add standard groupings for age and household size
# -- Notice age and hhsize use different tables (person, hh)
# -- but you don't need to worry about that; the package knows
hts_data <- hts_data %>% 
  hts_bin_age() %>%
  hts_bin_hhsize()

hts_data$person[, head(.SD), .SDcols=patterns("^age")]
#>            age          age_bin5          age_bin3
#>          <ord>            <fctr>            <fctr>
#> 1: 65-74 years 65 years or older 65 years or older
#> 2: 65-74 years 65 years or older 65 years or older
#> 3: 75-84 years 65 years or older 65 years or older
#> 4: 55-64 years       45-64 Years       18-64 Years
#> 5: 75-84 years 65 years or older 65 years or older
#> 6: 45-54 years       45-64 Years       18-64 Years
```

These functions each require a disaggregate input variable to be
requested in the earlier
[`get_psrc_hts()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_hts.md)
call. Both the input variable and the output variable are included in
the resulting `hts_data` object.

Currently, the following functions exist, along with the required input
variable.

[TABLE]
