# Retrieve and summarize data

## Works with **travelSurveyTools**

The **psrc.travelsurvey** package is meant to complement and enhance the
**travelSurveyTools** package for PSRC users. It delivers data in the
`hts_data` format appropriate as input to subsequent
**travelSurveyTools** functions. It also provides wrapper functions that
provide PSRC defaults to **travelSurveyTools** functions, resulting in
simpler workflows with fewer required arguments. **psrc.travelsurvey**
is constrained to an extent by fitting into the **travelSurveyTools**
system, but if you have suggestions for other features or functions
you’d find helpful, please contact the authors and we’ll see if we can
implement them.

## Data retrieval

Since psrc.travelsurvey uses Elmer, the agency’s central database, to
retrieve PSRC household travel survey data you’ll need to be connected
(in office or via VPN). Then use
[`get_psrc_hts()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_hts.md),
which takes two arguments:

- **survey_years** - a vector of one or more years; the default is all
  available i.e., `c(2017,2019,2021,2023)` as of this writing.
- **survey_vars** - a vector of variable names (regardless of the table
  in which they occur)

The return object is a list with separate data.table elements for each
potential unit of analysis, i.e. household (`hh`), `person`, `day`,
`trip`, and `vehicle`. The **travelSurveyTools** package workflow uses
this central object for related or independent variable summaries. It
can include the full set of variables for all your summaries, so it’s
unnecessary to request separate data objects to feed each separate
summary data table. It also supports multiyear summaries
(trends/comparisons) via the `survey_year` field.

For efficiency,
[`get_psrc_hts()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_hts.md)
only requests the variables you specify, so you must know their precise
names. A lookup function,
[`psrc_hts_varsearch()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_varsearch.md)
can assist; it takes a single string argument as the codebook search
term and matches either the variable name or description fields. Regular
expressions are supported for more complex searches; for example, the
word boundary operator `\\b` can be used to omit vehicle model from
results when searching for “mode”.

``` r
View(psrc_hts_varsearch("\\bmode\\b"))
```

To browse the entire variable list, open the internal data object
`variable_list`:

``` r
View(psrc.travelsurvey:::variable_list)
```

Note: It is unnecessary to include the survey year, table ids, weights,
or survey stratification variable (`sample_segment`) in your
`survey_vars` argument; since they are necessary for later steps,
[`get_psrc_hts()`](https://psrc.github.io/psrc.travelsurvey/reference/get_psrc_hts.md)
includes them implicitly. Factor variables and levels are applied from
the codebook, and missing or skip-value codes are recoded as `NA`.

## Summarization

To summarize data, use
[`psrc_hts_stat()`](https://psrc.github.io/psrc.travelsurvey/reference/psrc_hts_stat.md).
This has four primary arguments, and one option. The arguments are:

- **hts_data** - the `hts_data` object
- **analysis_unit** - string specifying the scale at which statistics
  are calculated, either “hh”, “person”, “day”,“trip”, or “vehicle”.
- **group_vars** - any grouping variables, in nesting order (share
  summaries are calculated per categories of the variable listed last).
- **stat_var** (optional) - Numeric variable if min/max/median/mean is
  desired; omit for count/share summaries.

As an example, we’ll retrieve the following survey variables, then do
one count/share summary, and one numeric summary. Notice,
travelSurveyTools handles the relational element of the data, so tables
lower in the organizational heirarchy (like `trips`) inherit attributes
from those above them (such as `hh`); this is a handy feature and avoids
unnecessary data duplication among tables.

``` r
library(psrc.travelsurvey)
library(data.table)

# Specify which variables to retrieve
vars <- c("hhsize", "employment", "commute_mode", "dest_purpose",
          "mode_class_5", "distance_miles")

# Retrieve the data
hts_data <- get_psrc_hts(survey_vars = vars, survey_year=2025)  # default includes all survey_years
  
# Calculate a numeric summary, i.e. min/max/median/mean
rs1 <- psrc_hts_stat(hts_data,
                     analysis_unit="trip", 
                     group_vars="hhsize", 
                     stat_var="distance_miles")

head(dplyr::select(rs1, -c(survey_year, min, max)))
#>      hhsize count      mean  mean_moe   median
#>       <ord> <int>     <num>     <num>    <num>
#> 1: 1 person  6330  7.533816 1.0839162 2.484248
#> 2: 2 people  9714 10.246266 2.2408366 4.009718
#> 3: 3 people  4143  8.100510 1.3662251 3.999776
#> 4: 4 people  3849  6.154936 0.5416001 2.487976
#> 5: 5 people  1443  7.118493 1.2528092 3.317509
#> 6: 6 people   440 10.042207 7.0121385 2.731555

# Calculate a categorical summary, i.e. count/share
rs2 <- psrc_hts_stat(hts_data, "trip", c("hhsize", "mode_class_5"))

head(dplyr::select(rs2[as.character(mode_class_5)=="Transit"],
                   -c(survey_year, mode_class_5)))
#>      hhsize count       prop    prop_moe       est   est_moe
#>       <ord> <int>      <num>       <num>     <num>     <num>
#> 1: 1 person   630 0.10681629 0.012461942 221470.54 26169.741
#> 2: 2 people   397 0.03407162 0.004651460 156516.10 21181.956
#> 3: 3 people   133 0.02563112 0.007600885  93227.21 27859.163
#> 4: 4 people   133 0.02354847 0.006762219  98766.21 28605.838
#> 5: 5 people    34 0.01427858 0.005657256  21545.34  8470.073
#> 6: 6 people     8 0.05188173 0.035224057  32626.57 23011.062
```

Average daily trip rates and average daily VMT rates are special cases
with their own summary functions, as described in [their own
vignette](https://psrc.github.io/psrc.travelsurvey/articles/summarize-trip-and-vmt-rates.md).

## Custom variables and filtering

Since each `hts_data` element is a data.table, they can be manipulated
with any combination of base r, tidyverse
(e.g. [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)),
or data.table (e.g `:=`) syntax. The example below illustrates adding
variables specific to the `hts_data$trip` element. Notice, the
inheritance capability mentioned earlier applies to custom variables as
well, so if you want to recode a household-level variable to feature in
a trip-level analysis, just add it to `hts_data$hh` (not to
`hts_data$trip`).

Standard category groupings not present in the base data can be added
via the functions described in [the accompanying
vignette](https://psrc.github.io/psrc.travelsurvey/articles/add-standard-groupings.md).
These take the entire `hts_data` object as argument, not a specific
table (the package knows which is involved). This lends itself to piping
multiple standard recodes.

Keep in mind that filtering any table removes records available for
subsequent analyses. Rather than manage multiple filtered copies of the
data object, you may want to create a new variable on the relevant table
with `NA` values for the records you intend to exclude from the summary.
You can then use the `incl_na=FALSE` option to exclude that line from
summary results (of particular value when you want shares calculated
from only reported categories). Here we calculate mode split percentages
for work trips only by adding a variable coded `NA` for all
non-work-related trips, then using the `incl_na=FALSE` option to limit
statistics to that category.

``` r
library(dplyr)
library(stringr)

# Use a standard variable recode to get simpler trip purpose
hts_data <- hts_bin_dest_purpose(hts_data)

# Use dplyr::mutate to add a simplified mode field & work purpose specifier
hts_data$trip <- mutate(
  hts_data$trip, 
  purpose_work = case_when(
    dest_purpose_bin4=="Work" ~ "Work",
    !is.na(dest_purpose_bin4) ~ NA_character_)
) 

# Calculate a categorical summary, i.e. count/share
rs3 <- psrc_hts_stat(hts_data, "trip", c("purpose_work", "mode_class_5"), incl_na=FALSE)

head(dplyr::select(rs3,
                   -c(survey_year, purpose_work)))
#>          mode_class_5 count       prop    prop_moe        est   est_moe
#>                 <ord> <int>      <num>       <num>      <num>     <num>
#> 1:              Drive  2865 0.83701361 0.018603624 1947461.90 118387.32
#> 2:            Transit   384 0.06310828 0.010826539  146832.71  24892.44
#> 3:               Walk   509 0.06122187 0.008932065  142443.63  19851.10
#> 4: Bike/Micromobility    87 0.01589812 0.008092092   36989.82  18977.08
#> 5:              Other    72 0.02275811 0.010591084   52950.82  24972.22
```

Note: **travelSurveyTools** summary functions require additional
preparatory steps that **psrc.travelsurvey** handles for you, for
simplicity. To use native **travelSurveyTools** summary functions,
you’ll need to handle these extra steps yourself, since
**travelSurveyTools** does not have a dependency on the
**psrc.travelsurvey** package.
