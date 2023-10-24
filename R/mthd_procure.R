#' 
#' Procure items from a dataset
#' @name procure-CostlySource
#' @description procure method applied to CostlySource
#' @param x An object of class CostlySource
#' @param matches_chr Matches (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("names", "concepts")
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @aliases procure,CostlySource-method
#' @export 
#' @importFrom ready4 procure
methods::setMethod("procure", "CostlySource", function (x, matches_chr = character(0), what_1L_chr = c("names", 
    "concepts"), ...) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr %in% c("names", "concepts")) {
        if (identical(matches_chr, character(0))) {
            if (what_1L_chr == "names") {
                matches_chr <- x@include_chr
            }
            else {
                matches_chr <- get_corresponding_var(x@Ready4useDyad_r4, 
                  matches_chr = x@include_chr, what_1L_chr = "names")
            }
        }
        object_xx <- get_corresponding_var(x@Ready4useDyad_r4, 
            matches_chr = matches_chr, what_1L_chr = what_1L_chr)
    }
    return(object_xx)
})
