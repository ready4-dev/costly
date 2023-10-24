manufacture_CostlyCorrespondences <- function(x,
                                              sort_1L_lgl = T,
                                              type_chr = make_ds_names(type_chr = character(0)),
                                              what_1L_chr = c("dataset", "defaults", "shorthand"),
                                              ...){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr %in% c("dataset",
                        "defaults", "shorthand")){
    if(what_1L_chr == "dataset"){
      if(type_chr[1] == "Correspondences"){
        object_xx <- update_with_standards(x@CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
                                           correspondences_x_r3 = x@correspondences_r3,
                                           case_when_false_1L_chr = x@args_ls$case_when_false_1L_chr,
                                           case_when_true_1L_chr = x@args_ls$case_when_true_1L_chr,
                                           case_when_true_ls = x@args_ls$case_when_true_ls,
                                           case_when_var_1L_chr = x@args_ls$case_when_var_1L_chr,
                                           reference_var_nm_1L_chr = procure(x@CostlySeed_r4,
                                                                             matches_chr = x@CostlySeed_r4@label_1L_chr,
                                                                             what_1L_chr = "names"),
                                           filter_cdn_1L_chr = x@args_ls$filter_cdn_1L_chr,
                                           force_standard_1L_lgl = x@force_standard_1L_lgl,
                                           fuzzy_logic_1L_chr = x@fuzzy_logic_1L_chr,
                                           max_distance_1L_dbl = x@max_distance_1L_dbl,
                                           standards_df = x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb,
                                           standards_var_nms_chr = procure(x@CostlyStandards_r4, matches_chr = x@CostlyStandards_r4@include_chr, what_1L_chr = "names"),
                                           tf_false_val_1L_lgl = x@args_ls$tf_false_val_1L_lgl)
      }
      if(type_chr[1] == "Lookup"){
        seed_matches_chr <- c(x@CostlySeed_r4@label_1L_chr,x@CostlySeed_r4@match_1L_chr)
        standards_matches_chr <- c(x@CostlyStandards_r4@label_1L_chr,x@CostlyStandards_r4@match_1L_chr)
        testit::assert("Seed matches need to be concepts included in the 'var_ctg_chr' column of the seed dictionary.",
                       identical(setdiff(seed_matches_chr, x@CostlySeed_r4@Ready4useDyad_r4@dictionary_r3$var_ctg_chr), character(0)))
        testit::assert("Standards matches values need to be concepts included in the 'var_ctg_chr' column of the standards dictionary.",
                       identical(setdiff(standards_matches_chr, x@CostlyStandards_r4@Ready4useDyad_r4@dictionary_r3$var_ctg_chr), character(0)))
        standard_concepts_chr <- x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>%
          dplyr::pull(!!rlang::sym(procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[1])
          ))
        standard_codes_chr <- x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>%
          dplyr::pull(!!rlang::sym(procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[2])
          ))
        ds_df <- as.data.frame(x@CostlySeed_r4@Ready4useDyad_r4@ds_tb)
        ds_df <- ds_df %>%
          dplyr::mutate(!!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])) := !!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])
          ) %>% purrr::map2_chr(!!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[2])),
                                ~ {
                                  ifelse(.x %in% (standard_concepts_chr),
                                         .x,
                                         ifelse(.y %in% (standard_codes_chr),
                                                ready4::get_from_lup_obj(x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb,
                                                                         match_var_nm_1L_chr = procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[2]),
                                                                         match_value_xx = .y,
                                                                         target_var_nm_1L_chr = procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[1])),
                                                ifelse(x@force_standard_1L_lgl, NA_character_, .x))

                                  )}))
        if(x@force_standard_1L_lgl){
          ds_df <- dplyr::filter(ds_df, !!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])) %>% purrr::map_lgl(~!is.na(.x)))
        }
        object_xx <- ds_df
      }
    }
    if(what_1L_chr == "defaults"){
      object_xx <- make_defaults(label_1L_chr = x@CostlySeed_r4@label_1L_chr,force_standard_1L_lgl = x@force_standard_1L_lgl, what_1L_chr = "all")
    }
    if(what_1L_chr == "shorthand"){
      object_xx <- ready4show::ready4show_correspondences() %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("seed", "standards",
                                                                                                                             "correspondences", "arguments", "force", "logic", "distance",
                                                                                                                             "results", "sharing"),
                                                                                                             new_nms_chr = methods::slotNames(x))
    }
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
manufacture_CostlyCountries <- function(x,
                                        sort_1L_lgl = T,
                                        type_chr = make_ds_names(type_chr = character(0)),
                                        what_1L_chr = c("dataset", "defaults", "shorthand"),
                                        ...){
  if(what_1L_chr %in% c("dataset", "defaults")){
    if(what_1L_chr == "defaults"){
      object_xx <- methods::callNextMethod()
      object_xx <- update_country_default_ls(object_xx, force_standard_1L_lgl = x@force_standard_1L_lgl)
    }
    if(what_1L_chr == "dataset"){
      object_xx <- methods::callNextMethod(x = x, type_chr = type_chr, sort_1L_lgl = sort_1L_lgl, what_1L_chr = what_1L_chr, ... = ...)
    }
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
manufacture_CostlyCurrencies <- function(x,
                                         sort_1L_lgl = T,
                                         type_chr = make_ds_names(type_chr = character(0)),
                                         what_1L_chr = c("dataset", "defaults", "shorthand"),
                                         ...){
  if(what_1L_chr %in% c("dataset", "defaults")){
    if(what_1L_chr == "defaults"){
      object_xx <- methods::callNextMethod()
      object_xx <- update_currency_default_ls(object_xx, force_standard_1L_lgl = x@force_standard_1L_lgl)
    }
    if(what_1L_chr == "dataset"){
      object_xx <- methods::callNextMethod(x = x, type_chr = type_chr, sort_1L_lgl = sort_1L_lgl, what_1L_chr = what_1L_chr, ... = ...)
    }
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
