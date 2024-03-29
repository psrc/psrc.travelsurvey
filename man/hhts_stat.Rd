% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hhts.R
\name{hhts_stat}
\alias{hhts_stat}
\alias{hhts_count}
\alias{hhts_sum}
\alias{hhts_median}
\alias{hhts_mean}
\alias{hhts_summary}
\title{Generic call for HHTS summary statistics}
\usage{
hhts_stat(
  df,
  stat_type,
  stat_var,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)

hhts_count(
  df,
  stat_var = NULL,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)

hhts_sum(
  df,
  stat_var,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)

hhts_median(
  df,
  stat_var,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)

hhts_mean(
  df,
  stat_var,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)

hhts_summary(
  df,
  stat_var,
  group_vars = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE,
  rr = FALSE
)
}
\arguments{
\item{df}{the dataframe returned by \code{\link{get_hhts}}}

\item{stat_var}{The numeric variable to summarize}

\item{group_vars}{Categorical variable/s for grouping}

\item{geographic_unit}{Optional sub-regional geographic grouping variable, such as "final_cnty"}

\item{spec_wgt}{optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.}

\item{incl_na}{option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)}

\item{rr}{optional relative reliability column, i.e. coefficient of variation as category levels (breakpoints: .15/.3./.5 -> good/fair/weak/unreliable)}
}
\value{
A summary tibble, including variable names, summary statistic and margin of error

A table with the variable names and labels, summary statistic and margin of error
}
\description{
Given specific form by related \code{\link{hhts_stat}} functions.

Separate function for total, count, median, mean'
}
\author{
Michael Jensen
}
