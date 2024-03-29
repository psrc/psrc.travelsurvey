% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hhts.R
\name{hhts_bulk_stat}
\alias{hhts_bulk_stat}
\title{Household Survey summary statistics}
\usage{
hhts_bulk_stat(
  df,
  stat_type,
  stat_var = NULL,
  group_var_list = NULL,
  geographic_unit = NULL,
  spec_wgt = NULL,
  incl_na = TRUE
)
}
\arguments{
\item{df}{the dataframe returned by \code{\link{get_hhts}}}

\item{stat_type}{Desired survey statistic}

\item{stat_var}{The numeric variable to summarize}

\item{group_var_list}{Factor variable/s for grouping}

\item{geographic_unit}{Optional sub-regional geographic grouping variable, such as "final_cnty"}

\item{spec_wgt}{optional user-specified expansion weight, i.e. in place of the standard expansion weight determined by the variable hierarchy. Only possible if the variable name is included in the \code{\link{get_hhts}} call.}

\item{incl_na}{option to remove NA from group_vars (if FALSE, the total will not reflect the full dataset)}
}
\value{
A table with the variable names and labels, summary statistic and margin of error
}
\description{
Generate a statistic separately for a list of grouping variables
List input items can be multiple, i.e. character vector
}
\author{
Michael Jensen
}
