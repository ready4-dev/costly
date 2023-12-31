% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_update.R
\name{update_correspondences}
\alias{update_correspondences}
\title{Update correspondences}
\usage{
update_correspondences(
  correspondences_x_r3 = ready4show::ready4show_correspondences(),
  standards_df,
  standards_var_nms_chr,
  seed_df,
  reference_var_nm_1L_chr,
  force_standard_1L_lgl = T,
  fuzzy_logic_1L_chr = "jw",
  max_distance_1L_dbl = Inf
)
}
\arguments{
\item{correspondences_x_r3}{Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()}

\item{standards_df}{Standards (a data.frame)}

\item{standards_var_nms_chr}{Standards variable names (a character vector)}

\item{seed_df}{Seed (a data.frame)}

\item{reference_var_nm_1L_chr}{Reference variable name (a character vector of length one)}

\item{force_standard_1L_lgl}{Force standard (a logical vector of length one), Default: T}

\item{fuzzy_logic_1L_chr}{Fuzzy logic (a character vector of length one), Default: 'jw'}

\item{max_distance_1L_dbl}{Maximum distance (a double vector of length one), Default: Inf}
}
\value{
Correspondences x (a ready4 S3)
}
\description{
update_correspondences() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update correspondences. Function argument correspondences_x_r3 specifies the object to be updated. Argument standards_df provides the object to be updated. The function returns Correspondences x (a ready4 S3).
}
\keyword{internal}
