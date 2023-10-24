#' Transform country
#' @description transform_country() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform country. Function argument country_1L_chr specifies the object to be updated. Argument from_1L_chr provides the object to be updated. The function returns Country (a character vector of length one).
#' @param country_1L_chr Country (a character vector of length one)
#' @param from_1L_chr From (a character vector of length one), Default: 'Name'
#' @param standards_df Standards (a data.frame), Default: data.frame()
#' @param what_1L_chr What (a character vector of length one), Default: 'Alpha_3'
#' @return Country (a character vector of length one)
#' @rdname transform_country
#' @export 
#' @importFrom utils data
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
transform_country <- function (country_1L_chr, from_1L_chr = "Name", standards_df = data.frame(), 
    what_1L_chr = "Alpha_3") 
{
    if (identical(standards_df, data.frame())) {
        utils::data("ISO_3166_1", package = "ISOcodes", envir = environment())
        standards_df <- ISO_3166_1
        rm(ISO_3166_1)
    }
    what_1L_chr <- match.arg(what_1L_chr, choices = setdiff(names(standards_df), 
        from_1L_chr))
    country_1L_chr <- ready4::get_from_lup_obj(standards_df, 
        match_value_xx = country_1L_chr, match_var_nm_1L_chr = from_1L_chr, 
        target_var_nm_1L_chr = what_1L_chr)
    return(country_1L_chr)
}
