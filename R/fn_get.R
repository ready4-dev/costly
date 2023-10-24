#' Get corresponding variable
#' @description get_corresponding_var() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get corresponding variable. Function argument Ready4useDyad_r4 specifies the where to look for the required object. The function returns Corresponding (a character vector).
#' @param Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4)
#' @param matches_chr Matches (a character vector)
#' @param what_1L_chr What (a character vector of length one), Default: 'concepts'
#' @return Corresponding (a character vector)
#' @rdname get_corresponding_var
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
get_corresponding_var <- function (Ready4useDyad_r4, matches_chr, what_1L_chr = "concepts") 
{
    match_var_nm_1L_chr = ifelse(what_1L_chr == "names", "var_ctg_chr", 
        "var_nm_chr")
    target_var_nm_1L_chr = ifelse(what_1L_chr == "names", "var_nm_chr", 
        "var_ctg_chr")
    matches_chr <- match.arg(matches_chr, choices = Ready4useDyad_r4@dictionary_r3 %>% 
        dplyr::pull(!!rlang::sym(match_var_nm_1L_chr)), several.ok = T)
    corresponding_chr <- matches_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(Ready4useDyad_r4@dictionary_r3, 
        match_var_nm_1L_chr = match_var_nm_1L_chr, match_value_xx = .x, 
        target_var_nm_1L_chr = target_var_nm_1L_chr))
    return(corresponding_chr)
}
#' Get country standards
#' @description get_country_standards() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get country standards. Function argument names_1L_lgl specifies the where to look for the required object. The function returns Country standards (an output object of multiple potential types).
#' @param names_1L_lgl Names (a logical vector of length one), Default: F
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: c("ISO_3166_1", "ISOcodes")
#' @param indices_int Indices (an integer vector), Default: c(4:6, 2, 1)
#' @param tbl_index_1L_int Table index (an integer vector of length one), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'Countries'
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'Input_Seed'
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Country standards (an output object of multiple potential types)
#' @rdname get_country_standards
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_country_standards <- function (names_1L_lgl = F, default_pkg_ds_chr = c("ISO_3166_1", 
    "ISOcodes"), indices_int = c(4:6, 2, 1), tbl_index_1L_int = integer(0), 
    type_1L_chr = "Countries", url_1L_chr = character(0), what_1L_chr = "Input_Seed", 
    X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    if (!names_1L_lgl) {
        indices_int <- integer(0)
    }
    country_standards_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, 
        indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, 
        url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr, X_Ready4useRepos = X_Ready4useRepos)
    return(country_standards_xx)
}
#' Get currency
#' @description get_currency() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get currency. Function argument country_1L_chr specifies the where to look for the required object. The function returns Currency (a character vector of length one).
#' @param country_1L_chr Country (a character vector of length one)
#' @param case_when_false_1L_chr Case when false (a character vector of length one), Default: 'NA'
#' @param case_when_true_1L_chr Case when true (a character vector of length one), Default: 'NA'
#' @param case_when_true_ls Case when true (a list), Default: NULL
#' @param case_when_var_1L_chr Case when variable (a character vector of length one), Default: 'NA'
#' @param country_var_nms_chr Country variable names (a character vector), Default: c("State or territory[1]", "Countries/ territories")
#' @param currency_tb Currency (a tibble), Default: NULL
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: F
#' @param format_1L_chr Format (a character vector of length one), Default: 'Name'
#' @param fuzzy_logic_1L_chr Fuzzy logic (a character vector of length one), Default: 'jw'
#' @param indcs_int Indices (an integer vector), Default: c(2:3)
#' @param seed_df Seed (a data.frame), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: 'ISO code[2]'
#' @param max_distance_1L_dbl Maximum distance (a double vector of length one), Default: Inf
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one), Default: 'Alpha_3'
#' @param tf_false_val_1L_lgl Transform false value (a logical vector of length one), Default: T
#' @param type_1L_chr Type (a character vector of length one), Default: 'Country'
#' @param url_1L_chr Url (a character vector of length one), Default: 'https://en.wikipedia.org/wiki/List_of_circulating_currencies'
#' @param what_1L_chr What (a character vector of length one), Default: 'Symbol'
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param correspondences_y_r3 Correspondences y (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Currency (a character vector of length one)
#' @rdname get_currency
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom ready4use Ready4useRepos
#' @importFrom countrycode countrycode
#' @importFrom ready4 get_from_lup_obj
#' @importFrom Hmisc capitalize
#' @keywords internal
get_currency <- function (country_1L_chr, case_when_false_1L_chr = NA_character_, 
    case_when_true_1L_chr = NA_character_, case_when_true_ls = NULL, 
    case_when_var_1L_chr = NA_character_, country_var_nms_chr = c("State or territory[1]", 
        "Countries/ territories"), currency_tb = NULL, filter_cdn_1L_chr = NA_character_, 
    force_standard_1L_lgl = F, format_1L_chr = "Name", fuzzy_logic_1L_chr = "jw", 
    indcs_int = c(2:3), seed_df = NULL, match_var_nm_1L_chr = "ISO code[2]", 
    max_distance_1L_dbl = Inf, target_var_nm_1L_chr = "Alpha_3", 
    tf_false_val_1L_lgl = T, type_1L_chr = "Country", url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies", 
    what_1L_chr = "Symbol", correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    correspondences_y_r3 = ready4show::ready4show_correspondences(), 
    X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    if (format_1L_chr != target_var_nm_1L_chr) {
        country_1L_chr <- transform_country(country_1L_chr = country_1L_chr, 
            from_1L_chr = format_1L_chr, what_1L_chr = target_var_nm_1L_chr)
    }
    currency_1L_chr <- country_1L_chr %>% countrycode::countrycode("iso3c", 
        "iso4217c")
    if (!is.na(what_1L_chr)) {
        if (identical(correspondences_x_r3, ready4show::ready4show_correspondences())) {
            correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3, 
                old_nms_chr = c("Name", "Symbol", "Code", "Unit", 
                  "Number"), new_nms_chr = c("Currency[1][2]", 
                  "Symbol[D] orAbbrev.[3]", "ISO code[2]", "Fractionalunit", 
                  "Numberto basic"))
        }
        if (is.null(currency_tb)) {
            currency_tb <- make_currencies_dss(case_when_false_1L_chr = case_when_false_1L_chr, 
                case_when_true_1L_chr = case_when_true_1L_chr, 
                case_when_true_ls = case_when_true_ls, case_when_var_1L_chr = case_when_var_1L_chr, 
                country_var_nms_chr = country_var_nms_chr, filter_cdn_1L_chr = filter_cdn_1L_chr, 
                force_standard_1L_lgl = force_standard_1L_lgl, 
                fuzzy_logic_1L_chr = fuzzy_logic_1L_chr, indcs_int = indcs_int, 
                max_distance_1L_dbl = max_distance_1L_dbl, seed_df = seed_df, 
                tf_false_val_1L_lgl = tf_false_val_1L_lgl, type_1L_chr = type_1L_chr, 
                url_1L_chr = url_1L_chr, correspondences_x_r3 = correspondences_y_r3, 
                X_Ready4useRepos = X_Ready4useRepos)$Currencies_By_Country_Lookup
        }
        currency_1L_chr <- currency_tb[which(currency_1L_chr == 
            (currency_tb[, which(names(currency_tb) == match_var_nm_1L_chr)] %>% 
                unlist() %>% as.vector()))[1], which(names(currency_tb) == 
            ready4::get_from_lup_obj(correspondences_x_r3, match_var_nm_1L_chr = "old_nms_chr", 
                match_value_xx = Hmisc::capitalize(what_1L_chr), 
                target_var_nm_1L_chr = "new_nms_chr"))][[1, 1]]
    }
    return(currency_1L_chr)
}
#' Get currency standards
#' @description get_currency_standards() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get currency standards. Function argument names_1L_lgl specifies the where to look for the required object. The function returns Country standards (an output object of multiple potential types).
#' @param names_1L_lgl Names (a logical vector of length one), Default: F
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: c("ISO_4217", "ISOcodes")
#' @param indices_int Indices (an integer vector), Default: c(3, 1)
#' @param tbl_index_1L_int Table index (an integer vector of length one), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'Currency'
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'Input_Seed'
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Country standards (an output object of multiple potential types)
#' @rdname get_currency_standards
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_currency_standards <- function (names_1L_lgl = F, default_pkg_ds_chr = c("ISO_4217", 
    "ISOcodes"), indices_int = c(3, 1), tbl_index_1L_int = integer(0), 
    type_1L_chr = "Currency", url_1L_chr = character(0), what_1L_chr = "Input_Seed", 
    X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    if (!names_1L_lgl) {
        indices_int <- integer(0)
    }
    country_standards_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, 
        indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, 
        url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr, X_Ready4useRepos = X_Ready4useRepos)
    return(country_standards_xx)
}
#' Get currency tables
#' @description get_currency_tbls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get currency tables. Function argument type_1L_chr specifies the where to look for the required object. The function returns Currency table (an output object of multiple potential types).
#' @param type_1L_chr Type (a character vector of length one), Default: c("Both", "Country", "Currency")
#' @param country_var_nms_chr Country variable names (a character vector), Default: c("State or territory[1]", "Countries/ territories")
#' @param indcs_int Indices (an integer vector), Default: c(2:3)
#' @param url_1L_chr Url (a character vector of length one), Default: 'https://en.wikipedia.org/wiki/List_of_circulating_currencies'
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Currency table (an output object of multiple potential types)
#' @rdname get_currency_tbls
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom ready4use Ready4useRepos
#' @importFrom ready4 get_from_lup_obj
#' @importFrom rvest read_html html_table
#' @importFrom stats setNames
#' @importFrom purrr pluck
#' @keywords internal
get_currency_tbls <- function (type_1L_chr = c("Both", "Country", "Currency"), country_var_nms_chr = c("State or territory[1]", 
    "Countries/ territories"), indcs_int = c(2:3), url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies", 
    correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    correspondences_x_r3 <- update_currency_correspondences(correspondences_x_r3, 
        country_var_nms_chr = country_var_nms_chr, type_1L_chr = "Both")
    if (type_1L_chr != "Both") {
        element_1L_chr <- ready4::get_from_lup_obj(correspondences_x_r3, 
            match_var_nm_1L_chr = "old_nms_chr", match_value_xx = type_1L_chr, 
            target_var_nm_1L_chr = "new_nms_chr")
        indcs_int <- indcs_int[which(correspondences_x_r3$new_nms_chr == 
            element_1L_chr)]
        correspondences_x_r3 <- update_currency_correspondences(correspondences_x_r3, 
            country_var_nms_chr = country_var_nms_chr, type_1L_chr = type_1L_chr)
    }
    if (identical(X_Ready4useRepos, ready4use::Ready4useRepos())) {
        currency_tbls_ls <- url_1L_chr %>% rvest::read_html() %>% 
            rvest::html_table()
        currency_tbls_ls <- currency_tbls_ls[indcs_int] %>% stats::setNames(correspondences_x_r3$new_nms_chr)
    }
    else {
        currency_tbls_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = correspondences_x_r3$new_nms_chr, 
            metadata_1L_lgl = F)
    }
    if (length(currency_tbls_ls) == 1) {
        currency_tbl_xx <- currency_tbls_ls %>% purrr::pluck(1)
    }
    else {
        currency_tbl_xx <- currency_tbls_ls
    }
    return(currency_tbl_xx)
}
#' Get seed cities
#' @description get_seed_cities() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get seed cities. Function argument names_1L_lgl specifies the where to look for the required object. The function returns Seed cities (an output object of multiple potential types).
#' @param names_1L_lgl Names (a logical vector of length one), Default: F
#' @param indices_int Indices (an integer vector), Default: 2
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: c("world.cities", "maps")
#' @param tbl_index_1L_int Table index (an integer vector of length one), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'Cities'
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'Input_Seed'
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Seed cities (an output object of multiple potential types)
#' @rdname get_seed_cities
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_seed_cities <- function (names_1L_lgl = F, indices_int = 2L, default_pkg_ds_chr = c("world.cities", 
    "maps"), tbl_index_1L_int = integer(0), type_1L_chr = "Cities", 
    url_1L_chr = character(0), what_1L_chr = "Input_Seed", X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    if (!names_1L_lgl) {
        indices_int <- integer(0)
    }
    seed_cities_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, 
        indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, 
        url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr, X_Ready4useRepos = X_Ready4useRepos)
    return(seed_cities_xx)
}
#' Get seed currencies
#' @description get_seed_currencies() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get seed currencies. Function argument names_1L_lgl specifies the where to look for the required object. The function returns Seed currencies (an output object of multiple potential types).
#' @param names_1L_lgl Names (a logical vector of length one), Default: F
#' @param indices_int Indices (an integer vector), Default: 1:4
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: character(0)
#' @param tbl_index_1L_int Table index (an integer vector of length one), Default: 2
#' @param type_1L_chr Type (a character vector of length one), Default: 'Currencies'
#' @param url_1L_chr Url (a character vector of length one), Default: 'https://en.wikipedia.org/wiki/List_of_circulating_currencies'
#' @param what_1L_chr What (a character vector of length one), Default: 'Input_Seed'
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Seed currencies (an output object of multiple potential types)
#' @rdname get_seed_currencies
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_seed_currencies <- function (names_1L_lgl = F, indices_int = 1:4, default_pkg_ds_chr = character(0), 
    tbl_index_1L_int = 2L, type_1L_chr = "Currencies", url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies", 
    what_1L_chr = "Input_Seed", X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    if (!names_1L_lgl) {
        indices_int <- integer(0)
    }
    seed_currencies_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, 
        indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, 
        url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr, X_Ready4useRepos = X_Ready4useRepos)
    return(seed_currencies_xx)
}
#' Get seed dataset
#' @description get_seed_ds() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get seed dataset. Function argument label_1L_chr specifies the where to look for the required object. The function returns Seed dataset (an output object of multiple potential types).
#' @param label_1L_chr Label (a character vector of length one)
#' @param default_pkg_ds_chr Default package dataset (a character vector), Default: character(0)
#' @param indices_int Indices (an integer vector), Default: integer(0)
#' @param tbl_index_1L_int Table index (an integer vector of length one), Default: integer(0)
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'Input_Seed'
#' @param X_Ready4useRepos PARAM_DESCRIPTION, Default: ready4use::Ready4useRepos()
#' @return Seed dataset (an output object of multiple potential types)
#' @rdname get_seed_ds
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @importFrom rvest read_html html_table
#' @importFrom purrr pluck
#' @importFrom testit assert
#' @importFrom utils data
#' @keywords internal
get_seed_ds <- function (label_1L_chr, default_pkg_ds_chr = character(0), indices_int = integer(0), 
    tbl_index_1L_int = integer(0), url_1L_chr = character(0), 
    what_1L_chr = "Input_Seed", X_Ready4useRepos = ready4use::Ready4useRepos()) 
{
    what_1L_chr <- match.arg(what_1L_chr, choices = make_ds_names(file_nm_1L_lgl = F))
    if (identical(X_Ready4useRepos, ready4use::Ready4useRepos())) {
        if (!identical(url_1L_chr, character(0))) {
            seed_ds_xx <- url_1L_chr %>% rvest::read_html() %>% 
                rvest::html_table()
            seed_ds_xx <- seed_ds_xx[tbl_index_1L_int] %>% purrr::pluck(1)
        }
        else {
            testit::assert("If neither a non-empty Ready4useRepos module nor a URL are supplied, then default_pkg_ds_chr must be a length two character vector specifying the dataset name (position 1) and containing R package (position 2)", 
                identical(default_pkg_ds_chr, character(0)) | 
                  is.character(default_pkg_ds_chr) | length(default_pkg_ds_chr) == 
                  2)
            utils::data(list = default_pkg_ds_chr[1], package = default_pkg_ds_chr[2], 
                envir = environment())
            seed_ds_xx <- eval(parse(text = default_pkg_ds_chr[1]))
        }
    }
    else {
        seed_ds_xx <- get_standardised_dss(X_Ready4useRepos = X_Ready4useRepos, 
            label_1L_chr = label_1L_chr, what_chr = what_1L_chr)
    }
    if (!identical(indices_int, integer(0))) {
        seed_ds_xx <- names(seed_ds_xx)
        if (!is.na(indices_int[1])) {
            testit::assert("indices_int contains values outside of indices of seed dataset names", 
                identical(setdiff(indices_int, 1:length(seed_ds_xx)) %>% 
                  as.numeric(), numeric(0)))
            seed_ds_xx <- seed_ds_xx[indices_int]
        }
    }
    return(seed_ds_xx)
}
#' Get standardised datasets
#' @description get_standardised_dss() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get standardised datasets. Function argument X_Ready4useRepos specifies the where to look for the required object. The function returns Standardised datasets (an output object of multiple potential types).
#' @param X_Ready4useRepos PARAM_DESCRIPTION
#' @param label_1L_chr Label (a character vector of length one), Default: 'Cities'
#' @param type_chr Type (a character vector), Default: c("Input", "Output")
#' @param what_chr What (a character vector), Default: make_ds_names(type_chr = character(0))
#' @return Standardised datasets (an output object of multiple potential types)
#' @rdname get_standardised_dss
#' @export 
#' @importFrom purrr pluck
#' @keywords internal
get_standardised_dss <- function (X_Ready4useRepos, label_1L_chr = "Cities", type_chr = c("Input", 
    "Output"), what_chr = make_ds_names(type_chr = character(0))) 
{
    what_chr <- match.arg(what_chr, several.ok = T)
    standardised_dss_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = make_ds_names(label_1L_chr, 
        type_chr = type_chr, what_chr = what_chr), metadata_1L_lgl = F)
    if (length(standardised_dss_ls) == 1) {
        standardised_dss_xx <- standardised_dss_ls %>% purrr::pluck(1)
    }
    else {
        standardised_dss_xx <- standardised_dss_ls
    }
    return(standardised_dss_xx)
}
