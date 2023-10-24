#' CostlySeed
#' 
#' Original (non-standardised) dataset (and metadata).
#' 
#' @include C4_CostlySource.R
#' @slot args_ls Arguments (a list)
#' @slot Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4)
#' @slot include_chr Include (a character vector)
#' @slot label_1L_chr Label (a character vector of length one)
#' @slot match_1L_chr Match (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name CostlySeed-class
#' @rdname CostlySeed-class
#' @export CostlySeed
#' @exportClass CostlySeed
CostlySeed <- methods::setClass("CostlySeed",
contains = "CostlySource",
slots = c(args_ls = "list",Ready4useDyad_r4 = "Ready4useDyad",include_chr = "character",label_1L_chr = "character",match_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(args_ls = list(list())))


methods::setValidity(methods::className("CostlySeed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
