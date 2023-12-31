% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_standardised_dss}
\alias{get_standardised_dss}
\title{Get standardised datasets}
\usage{
get_standardised_dss(
  X_Ready4useRepos,
  label_1L_chr = "Cities",
  type_chr = c("Input", "Output"),
  what_chr = make_ds_names(type_chr = character(0))
)
}
\arguments{
\item{X_Ready4useRepos}{PARAM_DESCRIPTION}

\item{label_1L_chr}{Label (a character vector of length one), Default: 'Cities'}

\item{type_chr}{Type (a character vector), Default: c("Input", "Output")}

\item{what_chr}{What (a character vector), Default: make_ds_names(type_chr = character(0))}
}
\value{
Standardised datasets (an output object of multiple potential types)
}
\description{
get_standardised_dss() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get standardised datasets. Function argument X_Ready4useRepos specifies the where to look for the required object. The function returns Standardised datasets (an output object of multiple potential types).
}
\keyword{internal}
