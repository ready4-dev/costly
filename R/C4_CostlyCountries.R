#' CostlyCountries
#' 
#' Collection of input, standards definition and results datasets for projects to generate standardised country data for use in costing datasets.
#' 
#' @include C4_CostlyCorrespondences.R C4_CostlySeed.R C4_CostlyStandards.R fn_add.R
#' @slot CostlySeed_r4 Original (non-standardised) dataset (and metadata). (a ready4 S4)
#' @slot CostlyStandards_r4 Dataset (and metadata) defining the allowable values of specified variables. (a ready4 S4)
#' @slot fuzzy_logic_1L_chr Fuzzy logic (a character vector of length one)
#' @slot correspondences_r3 Correspondences (a ready4 S3)
#' @slot args_ls Arguments (a list)
#' @slot force_standard_1L_lgl Force standard (a logical vector of length one)
#' @slot max_distance_1L_dbl Maximum distance (a double vector of length one)
#' @slot results_ls Results (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name CostlyCountries-class
#' @rdname CostlyCountries-class
#' @export CostlyCountries
#' @exportClass CostlyCountries
CostlyCountries <- methods::setClass("CostlyCountries",
contains = "CostlyCorrespondences",
slots = c(CostlySeed_r4 = "CostlySeed",CostlyStandards_r4 = "CostlyStandards",fuzzy_logic_1L_chr = "character",correspondences_r3 = "ready4show_correspondences",args_ls = "list",force_standard_1L_lgl = "logical",max_distance_1L_dbl = "numeric",results_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(CostlySeed_r4 = add_source_label(CostlySeed(),"Country"),CostlyStandards_r4 = CostlyStandards(Ready4useDyad_r4 = add_country_standards(),
                                                                                                                         include_chr = c("Country", "Official","Common","A3","A2"),
                                                                                                                         label_1L_chr = "Country"),fuzzy_logic_1L_chr = "jw"))


methods::setValidity(methods::className("CostlyCountries"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
