#' CostlySource
#' 
#' Input dataset (and metadata) for generating standardised costing datasets.
#' 
#' @include 
#' @slot Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4)
#' @slot include_chr Include (a character vector)
#' @slot label_1L_chrs  (an instance of the character class)
#' @slot match_1L_chr Match (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name CostlySource-class
#' @rdname CostlySource-class
#' @export CostlySource
#' @exportClass CostlySource
CostlySource <- methods::setClass("CostlySource",
contains = "Ready4Module",
slots = c(Ready4useDyad_r4 = "Ready4useDyad",include_chr = "character",label_1L_chrs = "character",match_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(Ready4useDyad_r4 = ready4use::Ready4useDyad(),include_chr = NA_character_,label_1L_chrs = NA_character_,match_1L_chr = "A3"))


methods::setValidity(methods::className("CostlySource"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
