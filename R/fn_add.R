#' Add country standards
#' @description add_country_standards() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add country standards. Function argument Ready4useDyad_r4 specifies the object to be updated. The function returns A dataset and data dictionary pair. (a ready4 S4).
#' @param Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4), Default: ready4use::Ready4useDyad()
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: c("ISO_3166_1", "ISOcodes")
#' @return A dataset and data dictionary pair. (a ready4 S4)
#' @rdname add_country_standards
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom utils data
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @keywords internal
add_country_standards <- function (Ready4useDyad_r4 = ready4use::Ready4useDyad(), default_pkg_ds_chr = c("ISO_3166_1", 
    "ISOcodes")) 
{
    utils::data(list = default_pkg_ds_chr[1], package = default_pkg_ds_chr[2], 
        envir = environment())
    Ready4useDyad_r4@ds_tb <- eval(parse(text = default_pkg_ds_chr[1])) %>% 
        tibble::as_tibble()
    Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(Ready4useDyad_r4@dictionary_r3, 
        var_nm_chr = names(Ready4useDyad_r4@ds_tb), var_ctg_chr = c("A2", 
            "A3", "N", "Country", "Official", "Common"), var_desc_chr = c("Alpabetical country code (two letters)", 
            "Alpabetical country code (three letters)", "Numeric country code", 
            "Country name", "Country name (official)", "Country name (common alternative)"), 
        var_type_chr = purrr::map_chr(Ready4useDyad_r4@ds_tb, 
            ~class(.x)[1]) %>% unname())
    return(Ready4useDyad_r4)
}
#' Add currency standards
#' @description add_currency_standards() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add currency standards. Function argument Ready4useDyad_r4 specifies the object to be updated. The function returns A dataset and data dictionary pair. (a ready4 S4).
#' @param Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4), Default: ready4use::Ready4useDyad()
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: c("ISO_4217", "ISOcodes")
#' @return A dataset and data dictionary pair. (a ready4 S4)
#' @rdname add_currency_standards
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom utils data
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @keywords internal
add_currency_standards <- function (Ready4useDyad_r4 = ready4use::Ready4useDyad(), default_pkg_ds_chr = c("ISO_4217", 
    "ISOcodes")) 
{
    utils::data(list = default_pkg_ds_chr[1], package = default_pkg_ds_chr[2], 
        envir = environment())
    Ready4useDyad_r4@ds_tb <- eval(parse(text = default_pkg_ds_chr[1])) %>% 
        tibble::as_tibble()
    Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(Ready4useDyad_r4@dictionary_r3, 
        var_nm_chr = names(Ready4useDyad_r4@ds_tb), var_ctg_chr = c("A3", 
            "N", "Currency"), var_desc_chr = c("Alpabetical currency code (three letters)", 
            "Numeric currency code", "Currency name"), var_type_chr = purrr::map_chr(Ready4useDyad_r4@ds_tb, 
            ~class(.x)[1]) %>% unname())
    return(Ready4useDyad_r4)
}
#' Add default country seed
#' @description add_default_country_seed() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add default country seed. Function argument CostlySeed_r4 specifies the object to be updated. The function returns Original (non-standardised) dataset (and metadata). (a ready4 S4).
#' @param CostlySeed_r4 Original (non-standardised) dataset (and metadata). (a ready4 S4), Default: CostlySeed()
#' @return Original (non-standardised) dataset (and metadata). (a ready4 S4)
#' @rdname add_default_country_seed
#' @export 
#' @importFrom tibble as_tibble
#' @importFrom ready4use renew.ready4use_dictionary
#' @importFrom purrr map_chr
#' @keywords internal
add_default_country_seed <- function (CostlySeed_r4 = CostlySeed()) 
{
    CostlySeed_r4@Ready4useDyad_r4@ds_tb <- get_seed_cities() %>% 
        tibble::as_tibble()
    CostlySeed_r4@Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(CostlySeed_r4@Ready4useDyad_r4@dictionary_r3, 
        var_nm_chr = names(CostlySeed_r4@Ready4useDyad_r4@ds_tb), 
        var_ctg_chr = c("City", "Country", "Population", "Latitude", 
            "Longitude", "Capital"), var_desc_chr = c("City name", 
            "Country name", "Population size", "Latitude coordinate", 
            "Longitude coordinate", "Is the nation's capital city"), 
        var_type_chr = purrr::map_chr(CostlySeed_r4@Ready4useDyad_r4@ds_tb, 
            ~class(.x)[1]) %>% unname())
    CostlySeed_r4@include_chr <- procure(CostlySeed_r4, matches_chr = get_seed_cities(T), 
        what_1L_chr = "concepts")
    return(CostlySeed_r4)
}
#' Add default currency seed
#' @description add_default_currency_seed() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add default currency seed. Function argument CostlySeed_r4 specifies the object to be updated. The function returns Original (non-standardised) dataset (and metadata). (a ready4 S4).
#' @param CostlySeed_r4 Original (non-standardised) dataset (and metadata). (a ready4 S4), Default: CostlySeed()
#' @param include_1L_chr Include (a character vector of length one), Default: 'Currency'
#' @param Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4), Default: ready4use::Ready4useDyad()
#' @return Original (non-standardised) dataset (and metadata). (a ready4 S4)
#' @rdname add_default_currency_seed
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @keywords internal
add_default_currency_seed <- function (CostlySeed_r4 = CostlySeed(), include_1L_chr = "Currency", 
    Ready4useDyad_r4 = ready4use::Ready4useDyad()) 
{
    if (identical(Ready4useDyad_r4, ready4use::Ready4useDyad())) {
        CostlySeed_r4@Ready4useDyad_r4@ds_tb <- get_seed_currencies() %>% 
            tibble::as_tibble()
        CostlySeed_r4@Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(CostlySeed_r4@Ready4useDyad_r4@dictionary_r3, 
            var_nm_chr = names(CostlySeed_r4@Ready4useDyad_r4@ds_tb), 
            var_ctg_chr = c("Country", "Currency", "Symbol", 
                "A3", "Fractional", "Number"), var_desc_chr = c("Country name", 
                "Currency name", "Currency symbol", "Currency alphabetical ISO code (three letter)", 
                "Currency's fractional unit", "Number of fractional units in basic unit"), 
            var_type_chr = purrr::map_chr(CostlySeed_r4@Ready4useDyad_r4@ds_tb, 
                ~class(.x)[1]) %>% unname())
    }
    else {
        CostlySeed_r4@Ready4useDyad_r4 <- Ready4useDyad_r4
    }
    if (include_1L_chr == "Currency") {
        CostlySeed_r4@include_chr <- procure(CostlySeed_r4, matches_chr = get_seed_currencies(T), 
            what_1L_chr = "concepts")
    }
    if (include_1L_chr == "Country") {
        CostlySeed_r4@include_chr <- "Country"
    }
    return(CostlySeed_r4)
}
#' Add source label
#' @description add_source_label() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add source label. Function argument CostlySource_r4 specifies the object to be updated. The function returns Input dataset (and metadata) for generating standardised costing datasets. (a ready4 S4).
#' @param CostlySource_r4 Input dataset (and metadata) for generating standardised costing datasets. (a ready4 S4), Default: CostlySource()
#' @param label_1L_chr Label (a character vector of length one), Default: 'Standardised'
#' @return Input dataset (and metadata) for generating standardised costing datasets. (a ready4 S4)
#' @rdname add_source_label
#' @export 
#' @keywords internal
add_source_label <- function (CostlySource_r4 = CostlySource(), label_1L_chr = "Standardised") 
{
    CostlySource_r4@label_1L_chr <- label_1L_chr
    return(CostlySource_r4)
}
