#' Update correspondences
#' @description update_correspondences() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update correspondences. Function argument correspondences_x_r3 specifies the object to be updated. Argument standards_df provides the object to be updated. The function returns Correspondences x (a ready4 S3).
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param standards_df Standards (a data.frame)
#' @param standards_var_nms_chr Standards variable names (a character vector)
#' @param seed_df Seed (a data.frame)
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one)
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: T
#' @param fuzzy_logic_1L_chr Fuzzy logic (a character vector of length one), Default: 'jw'
#' @param max_distance_1L_dbl Maximum distance (a double vector of length one), Default: Inf
#' @return Correspondences x (a ready4 S3)
#' @rdname update_correspondences
#' @export 
#' @importFrom ready4show ready4show_correspondences make_pt_ready4show_correspondences
#' @importFrom testit assert
#' @importFrom dplyr pull bind_rows arrange filter_at vars all_vars filter
#' @importFrom purrr map map_lgl map2_dfr
#' @importFrom stats setNames
#' @importFrom ready4 get_from_lup_obj
#' @importFrom tidyselect all_of
#' @importFrom stringdist amatch
#' @keywords internal
update_correspondences <- function (correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    standards_df, standards_var_nms_chr, seed_df, reference_var_nm_1L_chr, 
    force_standard_1L_lgl = T, fuzzy_logic_1L_chr = "jw", max_distance_1L_dbl = Inf) 
{
    testit::assert("'seed_df' must have both columns and rows", 
        nrow(seed_df) > 0 && ncol(seed_df) > 0)
    testit::assert("'reference_var_nm_1L_chr' must be a length one character vector", 
        length(reference_var_nm_1L_chr) == 1 && is.character((reference_var_nm_1L_chr)))
    testit::assert("'reference_var_nm_1L_chr' must refer to a column name of 'seed_df'", 
        reference_var_nm_1L_chr %in% names(seed_df))
    if (!identical(fuzzy_logic_1L_chr, character(0))) {
        items_chr <- seed_df %>% dplyr::pull(reference_var_nm_1L_chr) %>% 
            unique() %>% sort()
        matched_ls <- items_chr %>% purrr::map(~{
            item_1L_chr <- .x
            standards_var_nms_chr %>% purrr::map_lgl(~item_1L_chr %in% 
                (standards_df %>% dplyr::pull(.x)))
        }) %>% stats::setNames(items_chr)
        correspondences_y_r3 <- purrr::map2_dfr(matched_ls, names(matched_ls), 
            ~{
                if (length(standards_var_nms_chr) > 1) {
                  options_chr <- c(NA_character_, standards_var_nms_chr[2:length(standards_var_nms_chr)])
                }
                else {
                  options_chr <- c(NA_character_)
                }
                replace_1L_chr <- options_chr[which(.x)[1]]
                if (identical(replace_1L_chr, character(0))) 
                  replace_1L_chr <- NA_character_
                with_1L_chr <- ifelse(any(.x[2:5]), standards_var_nms_chr[1], 
                  NA_character_)
                if (is.na(replace_1L_chr) | is.na(with_1L_chr)) {
                  ready4show::make_pt_ready4show_correspondences()
                }
                else {
                  ready4show::make_pt_ready4show_correspondences(old_nms_chr = .y, 
                    new_nms_chr = ready4::get_from_lup_obj(standards_df, 
                      match_value_xx = .y, match_var_nm_1L_chr = replace_1L_chr, 
                      target_var_nm_1L_chr = with_1L_chr))
                }
            }) %>% ready4show::ready4show_correspondences()
        correspondences_x_r3 <- dplyr::bind_rows(correspondences_x_r3, 
            correspondences_y_r3) %>% dplyr::arrange(old_nms_chr)
        matched_lgl <- purrr::map_lgl(matched_ls, ~any(.x))
        matches_chr <- names(matched_lgl)[matched_lgl]
        unmatched_tb <- standards_df %>% dplyr::filter_at(dplyr::vars(tidyselect::all_of(standards_var_nms_chr)), 
            dplyr::all_vars(!. %in% matches_chr))
        unmatched_chr <- setdiff(names(matched_lgl), matches_chr)
        z_ready4show_correspondences <- ready4show::make_pt_ready4show_correspondences(old_nms_chr = unmatched_chr, 
            new_nms_chr = (unmatched_tb %>% dplyr::pull(Name))[stringdist::amatch(unmatched_chr, 
                unmatched_tb %>% dplyr::pull(Name), maxDist = max_distance_1L_dbl, 
                method = fuzzy_logic_1L_chr)]) %>% ready4show::ready4show_correspondences()
        z_ready4show_correspondences <- z_ready4show_correspondences %>% 
            dplyr::filter(!old_nms_chr %in% correspondences_x_r3$old_nms_chr)
        correspondences_x_r3 <- dplyr::bind_rows(correspondences_x_r3, 
            z_ready4show_correspondences) %>% dplyr::arrange(old_nms_chr)
    }
    if (force_standard_1L_lgl) {
        correspondences_x_r3 <- correspondences_x_r3 %>% dplyr::filter(new_nms_chr %in% 
            standards_df$Name)
    }
    return(correspondences_x_r3)
}
#' Update country default list
#' @description update_country_default_ls() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update country default list. Function argument default_ls specifies the object to be updated. Argument force_standard_1L_lgl provides the object to be updated. The function returns Default (a list).
#' @param default_ls Default (a list)
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: F
#' @return Default (a list)
#' @rdname update_country_default_ls
#' @export 
#' @keywords internal
update_country_default_ls <- function (default_ls, force_standard_1L_lgl = F) 
{
    default_ls$args_ls$case_when_true_ls <- list(capital = "name == 'Pristina' ~ 1")
    default_ls$args_ls$case_when_var_1L_chr <- default_ls$args_ls$case_when_false_1L_chr <- "capital"
    default_ls$CostlySeed_r4 <- add_default_country_seed(default_ls$CostlySeed_r4)
    default_ls$correspondences_x_r3 <- make_country_correspondences("cities")
    default_ls$correspondences_x_r3 <- update_correspondences(correspondences_x_r3 = default_ls$correspondences_x_r3, 
        seed_df = default_ls$CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% 
            as.data.frame(), reference_var_nm_1L_chr = procure(default_ls$CostlySeed_r4, 
            matches_chr = default_ls$CostlySeed_r4@label_1L_chr, 
            what_1L_chr = "names"), force_standard_1L_lgl = force_standard_1L_lgl, 
        fuzzy_logic_1L_chr = "jw", max_distance_1L_dbl = Inf, 
        standards_df = get_country_standards(), standards_var_nms_chr = get_country_standards(T))
    default_ls$fuzzy_logic_1L_chr <- character(0)
    return(default_ls)
}
#' Update currency correspondences
#' @description update_currency_correspondences() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update currency correspondences. Function argument correspondences_x_r3 specifies the object to be updated. Argument country_var_nms_chr provides the object to be updated. The function returns Correspondences x (a ready4 S3).
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param country_var_nms_chr Country variable names (a character vector), Default: c("State or territory[1]", "Countries/ territories")
#' @param type_1L_chr Type (a character vector of length one), Default: c("Both", "Country", "Currency")
#' @return Correspondences x (a ready4 S3)
#' @rdname update_currency_correspondences
#' @export 
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
update_currency_correspondences <- function (correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    country_var_nms_chr = c("State or territory[1]", "Countries/ territories"), 
    type_1L_chr = c("Both", "Country", "Currency")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(correspondences_x_r3, ready4show::ready4show_correspondences())) {
        correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3, 
            old_nms_chr = c("Country", "Currency"), new_nms_chr = c("by_country_tb", 
                "by_currency_tb"))
    }
    if (type_1L_chr %in% correspondences_x_r3$old_nms_chr) {
        element_1L_chr <- ready4::get_from_lup_obj(correspondences_x_r3, 
            match_var_nm_1L_chr = "old_nms_chr", match_value_xx = type_1L_chr, 
            target_var_nm_1L_chr = "new_nms_chr")
        country_var_nms_chr <- country_var_nms_chr[which(correspondences_x_r3$new_nms_chr == 
            element_1L_chr)]
        correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3, 
            filter_cdn_1L_chr = paste0("new_nms_chr == ", deparse(element_1L_chr)))
    }
    return(correspondences_x_r3)
}
#' Update currency default list
#' @description update_currency_default_ls() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update currency default list. Function argument default_ls specifies the object to be updated. Argument force_standard_1L_lgl provides the object to be updated. The function returns Default (a list).
#' @param default_ls Default (a list)
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: F
#' @return Default (a list)
#' @rdname update_currency_default_ls
#' @export 
#' @keywords internal
update_currency_default_ls <- function (default_ls, force_standard_1L_lgl = F) 
{
    default_ls$CostlySeed_r4 <- add_default_currency_seed(default_ls$CostlySeed_r4)
    return(default_ls)
}
#' Update module slot
#' @description update_module_slot() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update module slot. Function argument x_r4 specifies the object to be updated. Argument y_r3 provides the object to be updated. The function returns X (a ready4 S4).
#' @param x_r4 X (a ready4 S4)
#' @param y_r3 Y (a ready4 S3)
#' @param what_1L_chr What (a character vector of length one)
#' @param new_val_xx New value (an output object of multiple potential types), Default: NULL
#' @return X (a ready4 S4)
#' @rdname update_module_slot
#' @export 
#' @importFrom testit assert
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
update_module_slot <- function (x_r4, y_r3, what_1L_chr, new_val_xx = NULL) 
{
    testit::assert("x_r4 must be a Ready4Module", inherits(x_r4, 
        "Ready4Module"))
    testit::assert("y_r3 must be a correspondences lookup that includes `old_nms_chr` and `new_nms_chr` columns", 
        identical(setdiff(c("old_nms_chr", "new_nms_chr"), names(y_r3)), 
            character(0)))
    match.arg(what_1L_chr, choices = y_r3$old_nms_chr)
    x_r4 <- x_r4 <- renewSlot(x_r4, ready4::get_from_lup_obj(y_r3, 
        match_value_xx = what_1L_chr, match_var_nm_1L_chr = "old_nms_chr", 
        target_var_nm_1L_chr = "new_nms_chr"), new_val_xx)
    return(x_r4)
}
#' Update with standards
#' @description update_with_standards() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update with standards. Function argument seed_df specifies the object to be updated. Argument standards_df provides the object to be updated. The function returns Standardised dataset (a data.frame).
#' @param seed_df Seed (a data.frame)
#' @param standards_df Standards (a data.frame)
#' @param standards_var_nms_chr Standards variable names (a character vector)
#' @param correspondences_x_r3 Correspondences x (a ready4 S3), Default: ready4show::ready4show_correspondences()
#' @param case_when_false_1L_chr Case when false (a character vector of length one), Default: 'NA'
#' @param case_when_true_1L_chr Case when true (a character vector of length one), Default: 'NA'
#' @param case_when_true_ls Case when true (a list), Default: NULL
#' @param case_when_var_1L_chr Case when variable (a character vector of length one), Default: 'NA'
#' @param order_1L_lgl Order (a logical vector of length one), Default: F
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'country.etc'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param force_standard_1L_lgl Force standard (a logical vector of length one), Default: F
#' @param fuzzy_logic_1L_chr Fuzzy logic (a character vector of length one), Default: character(0)
#' @param max_distance_1L_dbl Maximum distance (a double vector of length one), Default: Inf
#' @param tf_false_val_1L_lgl Transform false value (a logical vector of length one), Default: T
#' @return Standardised dataset (a data.frame)
#' @rdname update_with_standards
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom dplyr mutate filter pull arrange
#' @importFrom rlang sym
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj update_tb_r3
#' @keywords internal
update_with_standards <- function (seed_df, standards_df, standards_var_nms_chr, correspondences_x_r3 = ready4show::ready4show_correspondences(), 
    case_when_false_1L_chr = NA_character_, case_when_true_1L_chr = NA_character_, 
    case_when_true_ls = NULL, case_when_var_1L_chr = NA_character_, 
    order_1L_lgl = F, reference_var_nm_1L_chr = "country.etc", 
    filter_cdn_1L_chr = NA_character_, force_standard_1L_lgl = F, 
    fuzzy_logic_1L_chr = character(0), max_distance_1L_dbl = Inf, 
    tf_false_val_1L_lgl = T) 
{
    if (!identical(fuzzy_logic_1L_chr, character(0))) {
        correspondences_x_r3 <- update_correspondences(correspondences_x_r3 = correspondences_x_r3, 
            seed_df = seed_df, reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
            force_standard_1L_lgl = force_standard_1L_lgl, fuzzy_logic_1L_chr = fuzzy_logic_1L_chr, 
            max_distance_1L_dbl = max_distance_1L_dbl, standards_df = standards_df, 
            standards_var_nms_chr = standards_var_nms_chr)
    }
    standardised_ds_df <- seed_df %>% dplyr::mutate(`:=`(!!rlang::sym(reference_var_nm_1L_chr), 
        !!rlang::sym(reference_var_nm_1L_chr) %>% purrr::map_chr(~ifelse(.x %in% 
            correspondences_x_r3$old_nms_chr, ready4::get_from_lup_obj(correspondences_x_r3, 
            match_value_xx = .x, match_var_nm_1L_chr = "old_nms_chr", 
            target_var_nm_1L_chr = "new_nms_chr"), .x))))
    if (force_standard_1L_lgl) {
        standardised_ds_df <- standardised_ds_df %>% dplyr::filter(!!rlang::sym(reference_var_nm_1L_chr) %in% 
            (standards_df %>% dplyr::pull(standards_var_nms_chr[1])))
    }
    if (!is.null(case_when_true_ls)) {
        standardised_ds_df <- standardised_ds_df %>% ready4::update_tb_r3(case_when_true_ls = case_when_true_ls, 
            case_when_var_1L_chr = case_when_var_1L_chr, case_when_false_1L_chr = case_when_var_1L_chr, 
            filter_cdn_1L_chr = filter_cdn_1L_chr, tf_false_val_1L_lgl = tf_false_val_1L_lgl)
    }
    if (order_1L_lgl) {
        standardised_ds_df <- standardised_ds_df %>% dplyr::arrange(!!rlang::sym(reference_var_nm_1L_chr))
    }
    return(standardised_ds_df)
}
