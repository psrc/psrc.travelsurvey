% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hhts.R
\name{hhts2srvyr}
\alias{hhts2srvyr}
\title{HHTS to srvyr}
\usage{
hhts2srvyr(df, survey, vars, spec_wgt = NULL)
}
\arguments{
\item{df}{the dataframe returned by \code{\link{get_hhts}}}

\item{survey}{the survey year or combination of survey years comprising the survey, e.g. c(2017_2019)}

\item{vars}{character vector with requested variables}

\item{spec_wgt}{optional user-specified expansion weight; only possible if the variable name is included in the \code{\link{get_hhts}} call.}
}
\value{
srvyr object with sampling weights
}
\description{
Creates srvyr object from HHTS
}
\author{
Michael Jensen
}
