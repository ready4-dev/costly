#' CostlyCorrespondences
#' 
#' Collection of input, standards definition and results datasets for projects to generate standardised costing datasets.
#' 
#' @include C4_CostlySeed.R C4_CostlySource.R C4_CostlyStandards.R
#' @slot CostlySeed_r4 Original (non-standardised) dataset (and metadata). (a ready4 S4)
#' @slot CostlyStandards_r4 Dataset (and metadata) defining the allowable values of specified variables. (a ready4 S4)
#' @slot correspondences_r3 Correspondences (a ready4 S3)
#' @slot args_ls Arguments (a list)
#' @slot force_standard_1L_lgl Force standard (a logical vector of length one)
#' @slot fuzzy_logic_1L_chr Fuzzy logic (a character vector of length one)
#' @slot max_distance_1L_dbl Maximum distance (a double vector of length one)
#' @slot results_ls Results (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name CostlyCorrespondences-class
#' @rdname CostlyCorrespondences-class
#' @export CostlyCorrespondences
#' @exportClass CostlyCorrespondences
CostlyCorrespondences <- methods::setClass("CostlyCorrespondences",
contains = "Ready4Module",
slots = c(CostlySeed_r4 = "CostlySeed",CostlyStandards_r4 = "CostlyStandards",correspondences_r3 = "ready4show_correspondences",args_ls = "list",force_standard_1L_lgl = "logical",fuzzy_logic_1L_chr = "character",max_distance_1L_dbl = "numeric",results_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(CostlySeed_r4 = CostlySeed(),CostlyStandards_r4 = CostlyStandards(),correspondences_r3 = ready4show::ready4show_correspondences(),args_ls = list(),force_standard_1L_lgl = F,fuzzy_logic_1L_chr = character(0),max_distance_1L_dbl = Inf,results_ls = list()))


methods::setValidity(methods::className("CostlyCorrespondences"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
