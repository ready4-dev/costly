% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_seed_currencies}
\alias{get_seed_currencies}
\title{Get seed currencies}
\usage{
get_seed_currencies(
  names_1L_lgl = F,
  indices_int = 1:4,
  default_pkg_ds_chr = character(0),
  tbl_index_1L_int = 2L,
  type_1L_chr = "Currencies",
  url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
  what_1L_chr = "Input_Seed",
  X_Ready4useRepos = ready4use::Ready4useRepos()
)
}
\arguments{
\item{names_1L_lgl}{Names (a logical vector of length one), Default: F}

\item{indices_int}{Indices (an integer vector), Default: 1:4}

\item{default_pkg_ds_chr}{Default package dataset (a character vector), Default: character(0)}

\item{tbl_index_1L_int}{Table index (an integer vector of length one), Default: 2}

\item{type_1L_chr}{Type (a character vector of length one), Default: 'Currencies'}

\item{url_1L_chr}{Url (a character vector of length one), Default: 'https://en.wikipedia.org/wiki/List_of_circulating_currencies'}

\item{what_1L_chr}{What (a character vector of length one), Default: 'Input_Seed'}

\item{X_Ready4useRepos}{PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()}
}
\value{
Seed currencies (an output object of multiple potential types)
}
\description{
get_seed_currencies() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get seed currencies. Function argument names_1L_lgl specifies the where to look for the required object. The function returns Seed currencies (an output object of multiple potential types).
}
\keyword{internal}
