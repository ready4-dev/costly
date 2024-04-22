update_correspondences <- function(correspondences_x_r3 = ready4show::ready4show_correspondences(),
                                   standards_df,
                                   standards_var_nms_chr,
                                   seed_df,
                                   reference_var_nm_1L_chr,
                                   force_standard_1L_lgl = T,
                                   fuzzy_logic_1L_chr = "jw",
                                   max_distance_1L_dbl = Inf
){
  testit::assert("'seed_df' must have both columns and rows",
                 nrow(seed_df)>0 && ncol(seed_df)>0)
  testit::assert("'reference_var_nm_1L_chr' must be a length one character vector",
                 length(reference_var_nm_1L_chr) == 1 && is.character((reference_var_nm_1L_chr)))
  testit::assert("'reference_var_nm_1L_chr' must refer to a column name of 'seed_df'",
                 reference_var_nm_1L_chr %in% names(seed_df))
  if(!identical(fuzzy_logic_1L_chr, character(0))){
    items_chr <- seed_df %>%
      dplyr::pull(reference_var_nm_1L_chr) %>%
      unique() %>% sort()
    matched_ls <- items_chr %>%
      purrr::map(~ {
        item_1L_chr <- .x
        standards_var_nms_chr %>%
          purrr::map_lgl(~ item_1L_chr %in% (standards_df %>% dplyr::pull(.x)))
      }
      ) %>%
      stats::setNames(items_chr)
    correspondences_y_r3 <- purrr::map2_dfr(matched_ls, names(matched_ls),
                                                    ~ {
                                                      if(length(standards_var_nms_chr)>1){
                                                        options_chr <- c(NA_character_, standards_var_nms_chr[2:length(standards_var_nms_chr)])
                                                      }else{
                                                        options_chr <- c(NA_character_)
                                                      }
                                                      replace_1L_chr <- options_chr[which(.x)[1]]
                                                      if(identical(replace_1L_chr, character(0)))
                                                        replace_1L_chr <- NA_character_
                                                      with_1L_chr <- ifelse(any(.x[2:5]),standards_var_nms_chr[1],NA_character_)

                                                      if(is.na(replace_1L_chr) | is.na(with_1L_chr)){
                                                        ready4show::make_pt_ready4show_correspondences()
                                                      }else{
                                                        ready4show::make_pt_ready4show_correspondences(old_nms_chr = .y,
                                                                                                       new_nms_chr = ready4::get_from_lup_obj(standards_df,
                                                                                                                                              match_value_xx = .y,
                                                                                                                                              match_var_nm_1L_chr = replace_1L_chr,
                                                                                                                                              target_var_nm_1L_chr = with_1L_chr))
                                                      }
                                                    }) %>% ready4show::ready4show_correspondences()
    correspondences_x_r3 <- dplyr::bind_rows(correspondences_x_r3,
                                                     correspondences_y_r3) %>%
      dplyr::arrange(old_nms_chr)
    matched_lgl <- purrr::map_lgl(matched_ls, ~ any(.x))
    matches_chr <- names(matched_lgl)[matched_lgl]
    unmatched_tb <- standards_df %>%
      dplyr::filter_at(dplyr::vars(tidyselect::all_of(standards_var_nms_chr)), dplyr::all_vars(!. %in% matches_chr))
    unmatched_chr <- setdiff(names(matched_lgl),matches_chr)
    z_ready4show_correspondences <- ready4show::make_pt_ready4show_correspondences(old_nms_chr = unmatched_chr,
                                                                                   new_nms_chr = (unmatched_tb %>% dplyr::pull(Name))[stringdist::amatch(unmatched_chr,
                                                                                                                                                         unmatched_tb %>%
                                                                                                                                                           dplyr::pull(Name),
                                                                                                                                                         maxDist = max_distance_1L_dbl,
                                                                                                                                                         method = fuzzy_logic_1L_chr)]) %>%
      ready4show::ready4show_correspondences()
    z_ready4show_correspondences <- z_ready4show_correspondences %>%
      dplyr::filter(!old_nms_chr %in% correspondences_x_r3$old_nms_chr)
    correspondences_x_r3 <- dplyr::bind_rows(correspondences_x_r3,
                                                     z_ready4show_correspondences) %>%
      dplyr::arrange(old_nms_chr)
  }
  if(force_standard_1L_lgl){
    correspondences_x_r3 <- correspondences_x_r3 %>%
      dplyr::filter(new_nms_chr %in% standards_df$Name)
  }
  return(correspondences_x_r3)
}
update_country_default_ls <- function(default_ls,
                                      force_standard_1L_lgl = F){

  default_ls$args_ls$case_when_true_ls <- list(capital = "name == 'Pristina' ~ 1")
  default_ls$args_ls$case_when_var_1L_chr <- default_ls$args_ls$case_when_false_1L_chr <- "capital"
  default_ls$CostlySeed_r4 <- add_default_country_seed(default_ls$CostlySeed_r4)
  default_ls$correspondences_x_r3 <- make_country_correspondences("cities")
  default_ls$correspondences_x_r3 <- update_correspondences(correspondences_x_r3 = default_ls$correspondences_x_r3,
                                                                    seed_df = default_ls$CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
                                                                    reference_var_nm_1L_chr = procure(default_ls$CostlySeed_r4,
                                                                                                      matches_chr = default_ls$CostlySeed_r4@label_1L_chr,
                                                                                                      what_1L_chr = "names"),
                                                                    force_standard_1L_lgl = force_standard_1L_lgl,
                                                                    fuzzy_logic_1L_chr = "jw",
                                                                    max_distance_1L_dbl = Inf,
                                                                    standards_df = get_country_standards(),
                                                                    standards_var_nms_chr = get_country_standards(T))
  default_ls$fuzzy_logic_1L_chr <- character(0)
  return(default_ls)
}
update_currency_correspondences <- function(correspondences_x_r3 = ready4show::ready4show_correspondences(),
                                            country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
                                            type_1L_chr = c("Both","Country","Currency")){
  type_1L_chr <- match.arg(type_1L_chr)
  if(identical(correspondences_x_r3, ready4show::ready4show_correspondences())){
    correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3, old_nms_chr = c("Country","Currency"),
                                                                                 new_nms_chr = c("by_country_tb","by_currency_tb"))
  }
  if(type_1L_chr %in% correspondences_x_r3$old_nms_chr){
    element_1L_chr <- ready4::get_from_lup_obj(correspondences_x_r3, match_var_nm_1L_chr = "old_nms_chr",
                                               match_value_xx = type_1L_chr, target_var_nm_1L_chr = "new_nms_chr")
    country_var_nms_chr <- country_var_nms_chr[which(correspondences_x_r3$new_nms_chr == element_1L_chr)]
    correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3 , filter_cdn_1L_chr = paste0("new_nms_chr == ", deparse(element_1L_chr)))
  }
  return(correspondences_x_r3)
}
update_currency_default_ls <- function(default_ls,
                                       force_standard_1L_lgl = F){
  default_ls$CostlySeed_r4 <- add_default_currency_seed(default_ls$CostlySeed_r4)
  return(default_ls)
}

update_medications_lup <- function(medications_lup,
                                   average_chr = character(0),
                                   integers_1L_lgl = TRUE, ##??
                                   arrange_by_1L_chr = "Response",
                                   arrange_tfmn_fn = identity,
                                   keep_chr = character(0),
                                   medication_var_nm_1L_chr = "Legal Instrument Drug",
                                   milligrams_indx_1L_int = 1L,
                                   milligrams_var_nm_1L_chr = "AMT Trade Product Pack",
                                   new_cases_tb = NULL,
                                   plural_chr = "",
                                   prices_tb = NULL,
                                   range_int = NA_integer_,
                                   reference_1L_int = integer(0),
                                   rename_meds_lup = ready4show::ready4show_correspondences(),
                                   rename_prices_lup = ready4show::ready4show_correspondences(),
                                   replace_blanks_1L_lgl = F,
                                   spaced_1L_lgl = TRUE,
                                   type_1L_chr = c("dosage", "validation", "weighted"),
                                   units_chr = "mg"){
  type_1L_chr <- match.arg(type_1L_chr)
  if(is.null(new_cases_tb)){
    new_cases_tb <- medications_lup %>% dplyr::filter(F)
  }else{
    new_cases_tb <- dplyr::bind_rows(medications_lup %>% dplyr::filter(F), new_cases_tb)
  }
  if(!identical(rename_meds_lup, ready4show::ready4show_correspondences()) && !is.null(prices_tb)){
    filtered_tb <- prices_tb %>%
      dplyr::filter(!!rlang::sym(medication_var_nm_1L_chr) %in% rename_prices_lup$old_nms_chr) %>%
      dplyr::mutate(!!rlang::sym(medication_var_nm_1L_chr) := !!rlang::sym(medication_var_nm_1L_chr) %>% purrr::map_chr(~ready4::get_from_lup_obj(rename_prices_lup, match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr")))
    new_cases_tb <- dplyr::bind_rows(new_cases_tb,
                                     medications_lup %>% dplyr::select(c("Response", "Medication")) %>% dplyr::distinct() %>% make_medications_lup(prices_tb = filtered_tb, add_dosage_1L_lgl = F) %>%
                                       dplyr::filter(!is.na(!!rlang::sym(milligrams_var_nm_1L_chr)))) %>%
      dplyr::select(names(medications_lup))
    #medications_lup <- medications_lup %>% dplyr::filter(!Medication %in% rename_prices_lup$new_nms_chr)
  }
  if(!identical(rename_meds_lup, ready4show::ready4show_correspondences()) && !is.null(prices_tb)){
    medications_tb <- medications_lup %>% dplyr::filter(Medication %in% rename_meds_lup$old_nms_chr) %>% dplyr::select(c("Response", "Medication")) %>%
      dplyr::mutate(Medication = Medication %>% purrr::map_chr(~ready4::get_from_lup_obj(rename_meds_lup, match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr")))
    medications_tb <- medications_tb %>% # make update fn
      dplyr::left_join(prices_tb %>% dplyr::mutate(Medication = !!rlang::sym(medication_var_nm_1L_chr)))
    medications_tb <- medications_tb %>%
      dplyr::select(intersect(names(medications_tb), names(medications_lup)))
    medications_lup <- medications_lup %>% dplyr::filter(!Medication %in% rename_meds_lup$old_nms_chr)
    medications_lup <- dplyr::bind_rows(medications_lup,
                                        medications_tb ) %>%
      dplyr::arrange(as.numeric(Response)) # make arg
  }
  ###
  ## unmatched_chr <- medications_lup %>% dplyr::filter(is.na(`Item Code`)) %>% dplyr::pull(Medication) # make fn
  ###
  ###
  if(nrow(new_cases_tb)>0){
    medications_lup <- dplyr::bind_rows(medications_lup %>% dplyr::filter(!Medication %in% c(rename_prices_lup$new_nms_chr, intersect(new_cases_tb$Medication, get_missing_medications(medications_lup)))),
                                        new_cases_tb %>%
                                          dplyr::mutate(Response = Medication %>% purrr::map_chr(~ready4::get_from_lup_obj(medications_lup,
                                                                                                                           target_var_nm_1L_chr = "Response",
                                                                                                                           match_var_nm_1L_chr = "Medication",
                                                                                                                           match_value_xx = .x)))) %>% dplyr::arrange(!!rlang::sym(arrange_by_1L_chr) %>% arrange_tfmn_fn())
  }
  if(type_1L_chr %in% c("dosage", "weighted")){
    if(!"Milligrams" %in% names(medications_lup)){
      medications_lup <- dplyr::mutate(medications_lup, Milligrams = NA_character_)
    }
    medications_lup <- dplyr::mutate(medications_lup,
                                     Milligrams = dplyr::case_when(is.na(!!rlang::sym(milligrams_var_nm_1L_chr)) ~ Milligrams,
                                                                   T ~ !!rlang::sym(milligrams_var_nm_1L_chr) %>% #`AMT Trade Product Pack` %>%
                                                                     get_patterns(integers_1L_lgl = integers_1L_lgl, plural_chr = plural_chr, range_int = range_int, replace_blanks_1L_lgl = replace_blanks_1L_lgl,
                                                                                  spaced_1L_lgl = spaced_1L_lgl, units_chr = units_chr) %>% ## ??
                                                                     purrr::map_chr(~paste0(.x,collapse = "_"))))
    #if(!"Per Tablet" %in% names(medications_lup)){
    medications_lup <- dplyr::mutate(medications_lup, `Per Tablet` = DPMQ /  `Maximum Quantity`) %>%
      dplyr::mutate(`Per mg` = `Per Tablet` / (Milligrams %>% get_patterns(flatten_1L_lgl = T,
                                                                           #integers_1L_lgl = F,
                                                                           range_int = range_int,
                                                                           spaced_1L_lgl = spaced_1L_lgl,
                                                                           type_1L_chr = "quantity",
                                                                           units_chr = units_chr, what_1L_chr = "double", plural_chr = plural_chr,
                                                                           reference_1L_int = reference_1L_int)))
    #}
  }
  if(!identical(average_chr, character(0))){
    medications_lup <- #dplyr::left_join(medications_lup,
      update_medications_lup(medications_lup, arrange_by_1L_chr = arrange_by_1L_chr, arrange_tfmn_fn = arrange_tfmn_fn, integers_1L_lgl = integers_1L_lgl, milligrams_var_nm_1L_chr = milligrams_var_nm_1L_chr,
                             new_cases_tb = dplyr::bind_rows(medications_lup %>% dplyr::filter(F),
                                                             suppressWarnings(dplyr::summarise(medications_lup, dplyr::across(c(`Maximum Quantity`,  DPMQ, Milligrams, `Per Tablet`, `Per mg`),
                                                                                                                              ~ {
                                                                                                                                if(is.numeric(.x)){
                                                                                                                                  mean(.x, na.rm = TRUE)
                                                                                                                                }else{
                                                                                                                                  .x %>% get_patterns(flatten_1L_lgl = T, range_int = range_int, type_1L_chr = "quantity",
                                                                                                                                                      units_chr = units_chr, what_1L_chr = "integer", plural_chr = plural_chr,
                                                                                                                                                      reference_1L_int = reference_1L_int,
                                                                                                                                                      spaced_1L_lgl = spaced_1L_lgl) %>% # New
                                                                                                                                    mean(na.rm = TRUE)
                                                                                                                                }}))) %>%
                                                               dplyr::mutate(Medication = average_chr,
                                                                             Milligrams = paste0(Milligrams, " ", units_chr[1]))),
                             plural_chr = plural_chr, range_int = range_int, replace_blanks_1L_lgl = replace_blanks_1L_lgl,
                             spaced_1L_lgl = spaced_1L_lgl, units_chr = units_chr)
  }
  ###
  if(!identical(keep_chr, character(0))){
    medications_lup <- medications_lup %>%
      dplyr::select(tidyselect::all_of(keep_chr))
  }
  if(type_1L_chr == "weighted"){
    medications_lup <- medications_lup$Response %>% unique() %>% purrr::map_dfr(~dplyr::filter(medications_lup, Response == .x) %>% dplyr::summarise(Response = .x,
                                                                                                                                                     Medication = paste0(unique(Medication), collapse = " / "),
                                                                                                                                                     `Per Tablet` = mean(`Per Tablet`, na.rm = TRUE),
                                                                                                                                                     `Per mg` = mean(`Per mg`, na.rm = TRUE))) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x),NA_real_,.x)))
  }
  return(medications_lup)
}






update_module_slot <- function(x_r4, # Be careful about potentially making this a Method of Ready4Module - need to think through renew syntax
                               y_r3,
                               what_1L_chr,
                               new_val_xx = NULL){
  testit::assert("x_r4 must be a Ready4Module",
                 inherits(x_r4, "Ready4Module"))
  testit::assert("y_r3 must be a correspondences lookup that includes `old_nms_chr` and `new_nms_chr` columns",
                 identical(setdiff(c("old_nms_chr", "new_nms_chr"), names(y_r3)), character(0)))
  match.arg(what_1L_chr, choices = y_r3$old_nms_chr)
  x_r4 <-  x_r4 <- renewSlot(x_r4,
                             ready4::get_from_lup_obj(y_r3,  # remove ns reference when exporting to ready4
                                                      match_value_xx = what_1L_chr, match_var_nm_1L_chr = "old_nms_chr", target_var_nm_1L_chr = "new_nms_chr"),
                             new_val_xx)
  return(x_r4)
}
update_with_standards <- function(seed_df,
                                  standards_df,
                                  standards_var_nms_chr,
                                  correspondences_x_r3 = ready4show::ready4show_correspondences(),
                                  case_when_false_1L_chr = NA_character_,
                                  case_when_true_1L_chr = NA_character_,
                                  case_when_true_ls = NULL,
                                  case_when_var_1L_chr = NA_character_,
                                  order_1L_lgl = F,
                                  reference_var_nm_1L_chr = "country.etc", # remove default
                                  filter_cdn_1L_chr = NA_character_,
                                  force_standard_1L_lgl = F,
                                  fuzzy_logic_1L_chr = character(0),
                                  max_distance_1L_dbl = Inf,
                                  tf_false_val_1L_lgl = T){
  if(!identical(fuzzy_logic_1L_chr, character(0))){ # Strictly not necessary, but reminder how to avoid modifying correspondences
    correspondences_x_r3 <- update_correspondences(correspondences_x_r3 = correspondences_x_r3,
                                                           seed_df = seed_df,
                                                           reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                                                           force_standard_1L_lgl = force_standard_1L_lgl,
                                                           fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                                           max_distance_1L_dbl = max_distance_1L_dbl,
                                                           standards_df = standards_df,
                                                           standards_var_nms_chr = standards_var_nms_chr)
  }
  standardised_ds_df <- seed_df %>% dplyr::mutate(!!rlang::sym(reference_var_nm_1L_chr) := !!rlang::sym(reference_var_nm_1L_chr) %>%
                                                    purrr::map_chr(~ifelse(.x %in% correspondences_x_r3$old_nms_chr, # Replace when manufacture method is updated??
                                                                           ready4::get_from_lup_obj(correspondences_x_r3,
                                                                                                    match_value_xx = .x,
                                                                                                    match_var_nm_1L_chr = "old_nms_chr",
                                                                                                    target_var_nm_1L_chr = "new_nms_chr"),
                                                                           .x)))
  if(force_standard_1L_lgl){
    standardised_ds_df <- standardised_ds_df %>%
      dplyr::filter(!!rlang::sym(reference_var_nm_1L_chr) %in% (standards_df %>% dplyr::pull(standards_var_nms_chr[1])))
  }
  if(!is.null(case_when_true_ls)){
    standardised_ds_df <- standardised_ds_df %>% ready4::update_tb_r3(case_when_true_ls = case_when_true_ls, case_when_var_1L_chr = case_when_var_1L_chr, case_when_false_1L_chr = case_when_var_1L_chr, filter_cdn_1L_chr = filter_cdn_1L_chr, tf_false_val_1L_lgl = tf_false_val_1L_lgl)
  }
  if(order_1L_lgl){
    standardised_ds_df <- standardised_ds_df %>% dplyr::arrange(!!rlang::sym(reference_var_nm_1L_chr))
  }
  return(standardised_ds_df)
}
