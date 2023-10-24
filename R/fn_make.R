#' Make country correspondences
#' @description make_country_correspondences() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make country correspondences. The function returns Correspondences x (a ready4 S3).
#' @param option_1L_chr Option (a character vector of length one), Default: 'custom'
#' @param old_nms_chr Old names (a character vector), Default: character(0)
#' @param new_nms_chr New names (a character vector), Default: character(0)
#' @return Correspondences x (a ready4 S3)
#' @rdname make_country_correspondences
#' @export 
#' @importFrom ready4show renew.ready4show_correspondences ready4show_correspondences
#' @keywords internal
make_country_correspondences <- function (option_1L_chr = "custom", old_nms_chr = character(0), 
    new_nms_chr = character(0)) 
{
    if (option_1L_chr == "cities") {
        old_nms_chr = c("Azores", "Canary Islands", "Easter Island", 
            "East Timor", "Ivory Coast", "Kosovo", "Madeira", 
            "Netherlands Antilles", "Sicily", "Vatican City")
        new_nms_chr = c("Portugal", "Spain", "Chile", "Timor-Leste", 
            "Côte d'Ivoire", "Kosovo", "Portugal", "Bonaire, Sint Eustatius and Saba", 
            "Italy", "Holy See (Vatican City State)")
    }
    if (option_1L_chr == "currencies") {
        correspondences_chr <- c(Abkhazia = "Abkhazia", `Akrotiri and Dhekelia` = "Akrotiri and Dhekelia", 
            Artsakh = "Artsakh", `Saint Helena, Ascension and Tristan da Cunha` = "Ascension Island", 
            Congo = "Congo, Republic of the", Guernsey = "Bailiwick of Guernsey", 
            `Timor-Leste` = "East Timor", `Korea, Democratic People's Republic of` = "Korea, North", 
            Kosovo = "Kosovo", `Northern Cyprus` = "Northern Cyprus", 
            `Bonaire, Sint Eustatius and Saba` = "Saba", `Western Sahara` = "Sahrawi Republic[I]", 
            Somaliland = "Somaliland", `South Ossetia` = "South Ossetia", 
            Transnistria = "Transnistria", `Holy See (Vatican City State)` = "Vatican City")
        old_nms_chr = unname(correspondences_chr)
        new_nms_chr = names(correspondences_chr)
    }
    correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(), 
        old_nms_chr = old_nms_chr, new_nms_chr = new_nms_chr)
    return(correspondences_x_r3)
}
#' Make country standards
#' @description make_country_standards() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make country standards. The function returns Standards (an output object of multiple potential types).
#' @param as_list_1L_lgl As list (a logical vector of length one), Default: F
#' @param append_ls Append (a list), Default: NULL
#' @return Standards (an output object of multiple potential types)
#' @rdname make_country_standards
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @keywords internal
make_country_standards <- function (as_list_1L_lgl = F, append_ls = NULL) 
{
    X <- ready4use::Ready4useDyad()
    X@ds_tb <- get_country_standards() %>% tibble::as_tibble()
    X@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X@dictionary_r3, 
        var_nm_chr = names(X@ds_tb), var_ctg_chr = c("A2", "A3", 
            "N", "Country", "Official", "Common"), var_desc_chr = c("Alpabetical country code (two letters)", 
            "Alpabetical country code (three letters)", "Numeric country code", 
            "Country name", "Country name (official)", "Country name (common alternative)"), 
        var_type_chr = purrr::map_chr(X@ds_tb, ~class(.x)[1]) %>% 
            unname())
    standards_xx <- make_standards_xx(as_list_1L_lgl, append_ls = append_ls, 
        var_nms_chr = get_country_standards(T), label_1L_chr = "Country", 
        Ready4useDyad_r4 = X)
    return(standards_xx)
}
#' Make currency list
#' @description make_currency_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make currency list. The function is called for its side effects and does not return a value.
#' @param country_1L_chr Country (a character vector of length one)
#' @param currency_tb Currency (a tibble), Default: NULL
#' @param format_1L_chr Format (a character vector of length one), Default: 'Name'
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: 'ISO code[2]'
#' @param what_1L_chr What (a character vector of length one), Default: 'Symbol[D] orAbbrev.[3]'
#' @return NULL
#' @rdname make_currency_ls
#' @export 
#' @keywords internal
make_currency_ls <- function (country_1L_chr, currency_tb = NULL, format_1L_chr = "Name", 
    match_var_nm_1L_chr = "ISO code[2]", what_1L_chr = "Symbol[D] orAbbrev.[3]") 
{
    currency_code_1L_chr <- get_currency(country_1L_chr, currency_tb = currency_tb, 
        format_1L_chr = format_1L_chr, match_var_nm_1L_chr = match_var_nm_1L_chr, 
        what_1L_chr = NA_character_)
    currency_symbol_1L_chr <- get_currency(country_1L_chr, currency_tb = currency_tb, 
        format_1L_chr = format_1L_chr, match_var_nm_1L_chr = match_var_nm_1L_chr, 
        what_1L_chr = what_1L_chr)
}
#' Make currency standards
#' @description make_currency_standards() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make currency standards. The function returns Standards (an output object of multiple potential types).
#' @param as_list_1L_lgl As list (a logical vector of length one), Default: F
#' @param append_ls Append (a list), Default: NULL
#' @return Standards (an output object of multiple potential types)
#' @rdname make_currency_standards
#' @export 
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @keywords internal
make_currency_standards <- function (as_list_1L_lgl = F, append_ls = NULL) 
{
    X <- ready4use::Ready4useDyad()
    X@ds_tb <- get_currency_standards() %>% tibble::as_tibble()
    X@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X@dictionary_r3, 
        var_nm_chr = names(X@ds_tb), var_ctg_chr = c("A3", "N", 
            "Currency"), var_desc_chr = c("Alpabetical currency code (three letters)", 
            "Numeric currency code", "Currency name"), var_type_chr = purrr::map_chr(X@ds_tb, 
            ~class(.x)[1]) %>% unname())
    standards_xx <- make_standards_xx(as_list_1L_lgl, append_ls = append_ls, 
        var_nms_chr = get_currency_standards(T), label_1L_chr = "Currency", 
        Ready4useDyad_r4 = X)
    return(standards_xx)
}
#' Make defaults
#' @description make_defaults() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make defaults. The function returns Default (an output object of multiple potential types).
#' @param label_1L_chr Label (a character vector of length one), Default: 'Standardised'
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "arguments", "correspondences", "logic", "seed", "reference")
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: F
#' @return Default (an output object of multiple potential types)
#' @rdname make_defaults
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @keywords internal
make_defaults <- function (label_1L_chr = "Standardised", what_1L_chr = c("all", 
    "arguments", "correspondences", "logic", "seed", "reference"), 
    force_standard_1L_lgl = F) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    default_ls <- list(CostlySeed_r4 = CostlySeed(label_1L_chr = label_1L_chr), 
        args_ls = list(case_when_false_1L_chr = NA_character_, 
            case_when_true_1L_chr = NA_character_, case_when_true_ls = NULL, 
            case_when_var_1L_chr = NA_character_, filter_cdn_1L_chr = NA_character_, 
            tf_false_val_1L_lgl = T), fuzzy_logic_1L_chr = "jw", 
        correspondences_x_r3 = ready4show::ready4show_correspondences())
    default_xx <- default_ls
    if (what_1L_chr == c("arguments")) {
        default_xx <- default_xx$args_ls
    }
    if (what_1L_chr == c("correspondences")) {
        default_xx <- default_xx$correspondences_x_r3
    }
    if (what_1L_chr == c("logic")) {
        default_xx <- default_xx$fuzzy_logic_1L_chr
    }
    if (what_1L_chr == c("seed")) {
        default_xx <- default_xx$CostlySeed_r4
    }
    if (what_1L_chr == c("reference")) {
        default_xx <- default_xx$seed_var_nms_chr
    }
    return(default_xx)
}
#' Make dataset names
#' @description make_ds_names() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dataset names. The function returns Names (a character vector).
#' @param label_1L_chr Label (a character vector of length one), Default: 'Standardised'
#' @param file_nm_1L_lgl File name (a logical vector of length one), Default: T
#' @param type_chr Type (a character vector), Default: c("Input", "Output")
#' @param what_chr What (a character vector), Default: c("Correspondences", "Lookup", "Seed", "Standards", "Validation")
#' @return Names (a character vector)
#' @rdname make_ds_names
#' @export 
#' @importFrom testit assert
#' @importFrom purrr map flatten_chr map_chr discard
#' @importFrom ready4 get_from_lup_obj
#' @importFrom ready4show renew.ready4show_correspondences ready4show_correspondences
#' @keywords internal
make_ds_names <- function (label_1L_chr = "Standardised", file_nm_1L_lgl = T, 
    type_chr = c("Input", "Output"), what_chr = c("Correspondences", 
        "Lookup", "Seed", "Standards", "Validation")) 
{
    testit::assert("label_1L_chr needs to be a length one character vector", 
        length(label_1L_chr) == 1 && is.character(label_1L_chr))
    if (identical(type_chr, character(0))) {
        names_chr <- what_chr
    }
    else {
        type_chr <- match.arg(type_chr, several.ok = T) %>% sort()
        what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
        names_chr <- type_chr %>% purrr::map(~{
            if (.x == "Input") {
                suffices_chr <- intersect(what_chr, c("Correspondences", 
                  "Seed", "Standards"))
            }
            else {
                suffices_chr <- intersect(what_chr, c("Correspondences", 
                  "Lookup", "Validation"))
            }
            paste0(.x, "_", suffices_chr)
        }) %>% purrr::flatten_chr()
        if (file_nm_1L_lgl) {
            names_chr <- names_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(), 
                old_nms_chr = names_chr, new_nms_chr = paste0(label_1L_chr, 
                  "_", names_chr)), match_var_nm_1L_chr = "old_nms_chr", 
                match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr")) %>% 
                purrr::map_chr(~ifelse(endsWith(.x, "_"), NA_character_, 
                  .x)) %>% purrr::discard(is.na)
        }
        else {
            names_chr <- names_chr
        }
    }
    return(names_chr)
}
#' Make standardised datasets
#' @description make_standardised_dss() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make standardised datasets. The function returns Standardised datasets (a list).
#' @param label_1L_chr Label (a character vector of length one), Default: 'Country'
#' @param lookup_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param seed_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param standards_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_chr Type (a character vector), Default: c("Input", "Output")
#' @param validation_ls Validation (a list), Default: list()
#' @param what_chr What (a character vector), Default: c("Correspondences", "Lookup", "Seed", "Standards", "Validation")
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param correspondences_y_r3 Correspondences y (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @return Standardised datasets (a list)
#' @rdname make_standardised_dss
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom ready4show ready4show_correspondences
#' @importFrom testit assert
#' @importFrom stats setNames
#' @keywords internal
make_standardised_dss <- function (label_1L_chr = "Country", lookup_Ready4useDyad = ready4use::Ready4useDyad(), 
    seed_Ready4useDyad = ready4use::Ready4useDyad(), standards_Ready4useDyad = ready4use::Ready4useDyad(), 
    type_chr = c("Input", "Output"), validation_ls = list(), 
    what_chr = c("Correspondences", "Lookup", "Seed", "Standards", 
        "Validation"), correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    correspondences_y_r3 = ready4show::ready4show_correspondences()) 
{
    testit::assert("label_1L_chr needs to be a length one character vector", 
        length(label_1L_chr) == 1 && is.character(label_1L_chr))
    type_chr <- match.arg(type_chr, several.ok = T) %>% sort()
    what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
    standardised_dss_ls <- list(correspondences_x_r3, seed_Ready4useDyad, 
        standards_Ready4useDyad, correspondences_y_r3, lookup_Ready4useDyad, 
        validation_ls) %>% stats::setNames(make_ds_names(label_1L_chr))
    standardised_dss_ls <- standardised_dss_ls[make_ds_names(label_1L_chr, 
        type_chr = type_chr, what_chr = what_chr)]
    return(standardised_dss_ls)
}
#' Make standards output object of multiple potential types
#' @description make_standards_xx() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make standards output object of multiple potential types. The function returns Standards (an output object of multiple potential types).
#' @param as_list_1L_lgl As list (a logical vector of length one), Default: T
#' @param append_ls Append (a list), Default: NULL
#' @param var_nms_chr Variable names (a character vector), Default: 'NA'
#' @param label_1L_chr Label (a character vector of length one), Default: 'Standardised'
#' @param Ready4useDyad_r4 A dataset and data dictionary pair. (a ready4 S4), Default: ready4use::Ready4useDyad()
#' @return Standards (an output object of multiple potential types)
#' @rdname make_standards_xx
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom stats setNames
#' @keywords internal
make_standards_xx <- function (as_list_1L_lgl = T, append_ls = NULL, var_nms_chr = NA_character_, 
    label_1L_chr = "Standardised", Ready4useDyad_r4 = ready4use::Ready4useDyad()) 
{
    X <- CostlyStandards()
    X@Ready4useDyad_r4 <- Ready4useDyad_r4
    X@label_1L_chr <- label_1L_chr
    if (!is.na(var_nms_chr[1])) {
        X@include_chr <- get_corresponding_var(X@Ready4useDyad_r4, 
            matches_chr = var_nms_chr, what_1L_chr = "concepts")
    }
    if (as_list_1L_lgl) {
        standards_xx <- append(append_ls, list(X) %>% stats::setNames(X@label_1L_chr))
    }
    else {
        standards_xx <- X
    }
    return(standards_xx)
}
#' Make validation list
#' @description make_validation_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make validation list. The function returns Validation (a list).
#' @param allowed_xx Allowed (an output object of multiple potential types)
#' @param ds_df Dataset (a data.frame)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: F
#' @return Validation (a list)
#' @rdname make_validation_ls
#' @export 
#' @importFrom purrr map2 map2_chr
#' @importFrom stats setNames
#' @keywords internal
make_validation_ls <- function (allowed_xx, ds_df, var_nm_1L_chr, sort_1L_lgl = F) 
{
    type_chr <- c("Absent", "Valid", "Invalid", "Valid", "Invalid")
    what_chr <- c(rep("Values", 3), rep("Cases", 2))
    validation_ls <- purrr::map2(type_chr, what_chr, ~make_validation_output(allowed_xx %>% 
        sort(), ds_df = ds_df, var_nm_1L_chr = var_nm_1L_chr, 
        sort_1L_lgl = sort_1L_lgl, type_1L_chr = .x, what_1L_chr = .y)) %>% 
        stats::setNames(purrr::map2_chr(type_chr, what_chr, ~paste0(.x, 
            "_", .y)))
    return(validation_ls)
}
#' Make validation output
#' @description make_validation_output() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make validation output. The function returns Output (an output object of multiple potential types).
#' @param allowed_xx Allowed (an output object of multiple potential types)
#' @param ds_df Dataset (a data.frame)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: F
#' @param type_1L_chr Type (a character vector of length one), Default: 'Valid'
#' @param what_1L_chr What (a character vector of length one), Default: 'Cases'
#' @return Output (an output object of multiple potential types)
#' @rdname make_validation_output
#' @export 
#' @importFrom tibble is_tibble as_tibble
#' @importFrom dplyr filter pull arrange
#' @importFrom rlang sym
#' @keywords internal
make_validation_output <- function (allowed_xx, ds_df, var_nm_1L_chr, sort_1L_lgl = F, 
    type_1L_chr = "Valid", what_1L_chr = "Cases") 
{
    if (is.data.frame(ds_df) & !tibble::is_tibble(ds_df)) {
        ds_tb <- tibble::as_tibble(ds_df)
    }
    else {
        ds_tb <- ds_df
    }
    output_xx <- NULL
    if (what_1L_chr %in% c("cases", "Cases")) {
        if (type_1L_chr %in% c("valid", "Valid")) {
            output_xx <- dplyr::filter(ds_tb, !!rlang::sym(var_nm_1L_chr) %in% 
                allowed_xx)
        }
        if (type_1L_chr %in% c("invalid", "Invalid")) {
            output_xx <- dplyr::filter(ds_tb, !(!!rlang::sym(var_nm_1L_chr)) %in% 
                allowed_xx)
        }
    }
    if (what_1L_chr %in% c("values", "Values")) {
        test_xx <- dplyr::pull(ds_tb, !!rlang::sym(var_nm_1L_chr))
        if (type_1L_chr %in% c("valid", "Valid")) {
            output_xx <- intersect(test_xx, allowed_xx)
        }
        if (type_1L_chr %in% c("invalid", "Invalid")) {
            output_xx <- setdiff(test_xx, allowed_xx)
        }
        if (type_1L_chr %in% c("absent", "Absent")) {
            output_xx <- setdiff(allowed_xx, test_xx)
        }
    }
    if (sort_1L_lgl & !is.null(output_xx)) {
        if (tibble::is_tibble(output_xx)) {
            output_xx <- output_xx %>% dplyr::arrange(!!rlang::sym(var_nm_1L_chr))
        }
        if (is.atomic(output_xx)) 
            output_xx <- sort(output_xx)
    }
    return(output_xx)
}
