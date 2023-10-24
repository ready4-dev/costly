## Retrieve from repo
## Apply to grouped ds
## Create repos
## Validate

get_timezone <- function(country_1L_chr, ## Depends on maps package
                         method_1L_chr = "accurate"){
  ### WIP
  latitude_1L_dbl <- -37.8
  longitude_1L_dbl <- 144.9
  lutz::tz_lookup_coords(lat = latitude_1L_dbl, lon = longitude_1L_dbl, method = method_1L_chr)
}
## Classes
# CostlySource <- methods::setClass("CostlySource",
#                                   contains = "Ready4Module",
#                                   slots = c(Ready4useDyad_r4 = "Ready4useDyad",
#                                             include_chr = "character",
#                                             label_1L_chr = "character",
#                                             match_1L_chr = "character"),
#                                   prototype =  list(Ready4useDyad_r4 = ready4use::Ready4useDyad(),
#                                                     include_chr = NA_character_,
#                                                     label_1L_chr = "Standardised",
#                                                     match_1L_chr = "A3"))
# CostlySeed <- methods::setClass("CostlySeed",
#                                 contains = "CostlySource",
#                                 slots = c(args_ls = "list"),
#                                 prototype =  list(args_ls = list()))
# CostlyStandards <- methods::setClass("CostlyStandards",
#                                      contains = "CostlySource",
#                                      slots = c(args_ls = "list"),
#                                      prototype =  list(args_ls = list()))
# CostlyCorrespondences <- methods::setClass("CostlyCorrespondences",
#                                            contains = "Ready4Module",
#                                            slots = c(CostlySeed_r4 = "CostlySeed",
#                                                      CostlyStandards_r4 = "CostlyStandards",
#                                                      correspondences_r3 = "ready4show_correspondences",
#                                                      args_ls = "list",
#                                                      force_standard_1L_lgl = "logical",
#                                                      fuzzy_logic_1L_chr = "character",
#                                                      max_distance_1L_dbl = "numeric",
#                                                      results_ls = "list"),
#                                            prototype =  list(CostlySeed_r4 = CostlySeed(),
#                                                              CostlyStandards_r4 = CostlyStandards(),
#                                                              correspondences_r3 = ready4show::ready4show_correspondences(),
#                                                              args_ls = list(),
#                                                              force_standard_1L_lgl = F,
#                                                              fuzzy_logic_1L_chr = character(0),
#                                                              max_distance_1L_dbl = Inf,
#                                                              results_ls = list()))
# CostlyCountries <- methods::setClass("CostlyCountries",
#                                      contains = "CostlyCorrespondences",
#                                      prototype = list(CostlySeed_r4 = add_source_label(CostlySeed(),"Country"),
#                                                       CostlyStandards_r4 = make_country_standards(),
#                                                       fuzzy_logic_1L_chr = "jw"))
# CostlyCurrencies <- methods::setClass("CostlyCurrencies",
#                                       contains = "CostlyCorrespondences",
#                                       prototype = list(CostlySeed_r4 = add_source_label(CostlySeed(),"Currency"),
#                                                        CostlyStandards_r4 = make_currency_standards()))

## Methods
# methods::setMethod("manufacture", "CostlyCorrespondences", function(x,
#                                                                     sort_1L_lgl = T,
#                                                                     type_chr = make_ds_names(type_chr = character(0)),
#                                                                     what_1L_chr = c("dataset",
#                                                                                     "defaults", "shorthand"),
#                                                                     ...){
#   what_1L_chr <- match.arg(what_1L_chr)
#   if(what_1L_chr %in% c("dataset",
#                         "defaults", "shorthand")){
#     if(what_1L_chr == "dataset"){
#       if(type_chr[1] == "Correspondences"){
#         object_xx <- update_with_standards(x@CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
#                                            correspondences_x_r3 = x@correspondences_r3,
#                                            case_when_false_1L_chr = x@args_ls$case_when_false_1L_chr,
#                                            case_when_true_1L_chr = x@args_ls$case_when_true_1L_chr,
#                                            case_when_true_ls = x@args_ls$case_when_true_ls,
#                                            case_when_var_1L_chr = x@args_ls$case_when_var_1L_chr,
#                                            reference_var_nm_1L_chr = procure(x@CostlySeed_r4,
#                                                                              matches_chr = x@CostlySeed_r4@label_1L_chr,
#                                                                              what_1L_chr = "names"),
#                                            filter_cdn_1L_chr = x@args_ls$filter_cdn_1L_chr,
#                                            force_standard_1L_lgl = x@force_standard_1L_lgl,
#                                            fuzzy_logic_1L_chr = x@fuzzy_logic_1L_chr,
#                                            max_distance_1L_dbl = x@max_distance_1L_dbl,
#                                            standards_df = x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb,
#                                            standards_var_nms_chr = procure(x@CostlyStandards_r4, matches_chr = x@CostlyStandards_r4@include_chr, what_1L_chr = "names"),
#                                            tf_false_val_1L_lgl = x@args_ls$tf_false_val_1L_lgl)
#       }
#       if(type_chr[1] == "Lookup"){
#         seed_matches_chr <- c(x@CostlySeed_r4@label_1L_chr,x@CostlySeed_r4@match_1L_chr)
#         standard_matches_chr <- c(x@CostlyStandards_r4@label_1L_chr,x@CostlyStandards_r4@match_1L_chr)
#         testit::assert("Seed matches need to be concepts included in the 'var_ctg_chr' column of the seed dictionary.",
#                        identical(setdiff(seed_matches_chr, x@CostlySeed_r4@Ready4useDyad_r4@dictionary_r3$var_ctg_chr), character(0)))
#         testit::assert("Standards matches values need to be concepts included in the 'var_ctg_chr' column of the standards dictionary.",
#                        identical(setdiff(standards_matches_chr, x@CostlyStandards_r4@Ready4useDyad_r4@dictionary_r3$var_ctg_chr), character(0)))
#         standard_concepts_chr <- x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>%
#           dplyr::pull(!!rlang::sym(procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[1])
#           ))
#         standard_codes_chr <- x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>%
#           dplyr::pull(!!rlang::sym(procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[2])
#           ))
#         ds_df <- as.data.frame(x@CostlySeed_r4@Ready4useDyad_r4@ds_tb)
#         ds_df <- ds_df %>%
#           dplyr::mutate(!!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])) := !!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])
#           ) %>% purrr::map2_chr(!!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[2])),
#                                 ~ {
#                                   ifelse(.x %in% (standard_concepts_chr),
#                                          .x,
#                                          ifelse(.y %in% (standard_codes_chr),
#                                                 ready4::get_from_lup_obj(x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb,
#                                                                          match_var_nm_1L_chr = procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[2]),
#                                                                          match_value_xx = .y,
#                                                                          target_var_nm_1L_chr = procure(x@CostlyStandards_r4, matches_chr = standards_matches_chr[1])),
#                                                 ifelse(x@force_standard_1L_lgl, NA_character_, .x))
#
#                                   )}))
#         if(x@force_standard_1L_lgl){
#           ds_df <- dplyr::filter(ds_df, !!rlang::sym(procure(x@CostlySeed_r4, seed_matches_chr[1])) %>% purrr::map_lgl(~!is.na(.x)))
#         }
#         object_xx <- ds_df
#       }
#     }
#     if(what_1L_chr == "defaults"){
#       object_xx <- make_defaults(label_1L_chr = x@CostlySeed_r4@label_1L_chr,force_standard_1L_lgl = x@force_standard_1L_lgl, what_1L_chr = "all")
#     }
#     if(what_1L_chr == "shorthand"){
#       object_xx <- ready4show::ready4show_correspondences() %>% ready4show::renew.ready4show_correspondences(old_nms_chr = c("seed", "standards",
#                                                                                                                              "correspondences", "arguments", "force", "logic", "distance",
#                                                                                                                              "results", "sharing"),
#                                                                                                              new_nms_chr = methods::slotNames(x))
#     }
#   }else{
#     object_xx <- methods::callNextMethod()
#   }
#   return(object_xx)
# })
# methods::setMethod("manufacture", "CostlyCountries", function(x,
#                                                               sort_1L_lgl = T,
#                                                               type_chr = make_ds_names(type_chr = character(0)),
#                                                               what_1L_chr = c("dataset", "defaults", "shorthand"),
#                                                               ...){
#   if(what_1L_chr %in% c("dataset", "defaults")){
#     if(what_1L_chr == "defaults"){
#       object_xx <- methods::callNextMethod()
#       object_xx <- update_country_default_ls(object_xx, force_standard_1L_lgl = x@force_standard_1L_lgl)
#     }
#     if(what_1L_chr == "dataset"){
#       object_xx <- methods::callNextMethod(x = x, type_chr = type_chr, sort_1L_lgl = sort_1L_lgl, what_1L_chr = what_1L_chr, ... = ...)
#     }
#   }else{
#     object_xx <- methods::callNextMethod()
#   }
#   return(object_xx)
# })
# methods::setMethod("manufacture", "CostlyCurrencies", function(x,
#                                                                sort_1L_lgl = T,
#                                                                type_chr = make_ds_names(type_chr = character(0)),
#                                                                what_1L_chr = c("dataset", "defaults", "shorthand"),
#                                                                ...){
#   if(what_1L_chr %in% c("dataset", "defaults")){
#     if(what_1L_chr == "defaults"){
#       object_xx <- methods::callNextMethod()
#       object_xx <- update_currency_default_ls(object_xx, force_standard_1L_lgl = x@force_standard_1L_lgl)
#     }
#     if(what_1L_chr == "dataset"){
#       object_xx <- methods::callNextMethod(x = x, type_chr = type_chr, sort_1L_lgl = sort_1L_lgl, what_1L_chr = what_1L_chr, ... = ...)
#     }
#   }else{
#     object_xx <- methods::callNextMethod()
#   }
#   return(object_xx)
# })
# methods::setMethod("procure", "CostlySource", function(x,
#                                                        matches_chr = character(0),
#                                                        what_1L_chr = c("names","concepts")){
#   what_1L_chr <- match.arg(what_1L_chr)
#   if(what_1L_chr %in% c("names","concepts")){
#     if(identical(matches_chr, character(0))){
#       if(what_1L_chr == "names"){
#         matches_chr <- x@include_chr
#       }else{
#         matches_chr <- get_corresponding_var(x@Ready4useDyad_r4,
#                                              matches_chr = x@include_chr,
#                                              what_1L_chr = "names")
#       }
#     }
#     object_xx <- get_corresponding_var(x@Ready4useDyad_r4,
#                                        matches_chr = matches_chr,
#                                        what_1L_chr = what_1L_chr)
#   }
#   return(object_xx)
# })
# methods::setMethod("ratify", "CostlyCorrespondences", function(x,
#                                                                new_val_xx = "identity",
#                                                                sort_1L_lgl = T,
#                                                                type_1L_chr = make_ds_names(type_chr = character(0))[1:2],
#                                                                what_chr = make_ds_names(type_chr = character(0))){
#   type_1L_chr <- match.arg(type_1L_chr)
#   if(type_1L_chr == "Correspondences"){
#     if(!identical(new_val_xx, "identity") && !is.null(new_val_xx)){
#       correspondences_x_r3 <- new_val_xx
#     }else{
#       correspondences_x_r3 <- x@correspondences_r3
#     }
#     x <- renew(x, new_val_xx = new_val_xx, type_1L_chr = "final", what_1L_chr = "correspondences")
#
#   }else{
#     correspondences_x_r3 <- x@correspondences_r3
#   }
#   # match_1L_chr <- x@CostlySeed_r4@label_1L_chr ## @include_chr[1]
#   ds_df <- manufacture(x, type_chr = type_1L_chr, what_1L_chr = "dataset")
#   validation_ls <- make_validation_ls(x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>% dplyr::pull(procure(x@CostlyStandards_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, what_1L_chr = "names")),
#                                       ds_df = ds_df,
#                                       var_nm_1L_chr = procure(x@CostlySeed_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, #match_1L_chr
#                                                               what_1L_chr = "names"),
#                                       sort_1L_lgl = sort_1L_lgl)
#   x@results_ls <- make_standardised_dss(x@CostlySeed_r4@label_1L_chr,
#                                         lookup_Ready4useDyad = ready4use::Ready4useDyad(ds_tb = tibble::as_data_frame(ds_df), dictionary_r3 = x@CostlySeed_r4@Ready4useDyad_r4@dictionary_r3),
#                                         seed_Ready4useDyad = x@CostlySeed_r4@Ready4useDyad_r4,
#                                         standards_Ready4useDyad = x@CostlyStandards_r4@Ready4useDyad_r4,
#                                         validation_ls = validation_ls,
#                                         what_chr = what_chr,
#                                         correspondences_x_r3 = correspondences_x_r3,
#                                         correspondences_y_r3 = x@correspondences_r3)
#   return(x)
# })
# methods::setMethod("renew", "CostlyCorrespondences",
#                    function (x, new_val_xx = NULL, type_1L_chr = "slot", what_1L_chr = "correspondences", ...) {
#                      if(type_1L_chr == "default"){
#                        default_ls <- manufacture(x, what_1L_chr = "defaults")
#                        x@args_ls <- default_ls$args_ls
#                        x@correspondences_r3 <- default_ls$correspondences_x_r3
#                        x@fuzzy_logic_1L_chr <- default_ls$fuzzy_logic_1L_chr
#                        x@CostlySeed_r4 <- default_ls$CostlySeed_r4
#                      }
#                      if(type_1L_chr %in% c("exploratory", "final","slot")){
#                        y <- manufacture(x, what_1L_chr = "shorthand")
#                        what_1L_chr <- match.arg(what_1L_chr, choices = y$old_nms_chr)
#                        if(what_1L_chr == "correspondences"){
#                          if(!is.null(new_val_xx) && !identical(new_val_xx, "identity")){
#                            x@correspondences_r3 <- new_val_xx
#                          }
#                          if(is.null(new_val_xx) | !identical(new_val_xx, "identity")){
#                            x@correspondences_r3 <- update_correspondences(correspondences_x_r3 = x@correspondences_r3,
#                                                                           standards_df = x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
#                                                                           standards_var_nms_chr = procure(x@CostlyStandards_r4, matches_chr = x@CostlyStandards_r4@include_chr, what_1L_chr = "names"),
#                                                                           seed_df = x@CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
#                                                                           reference_var_nm_1L_chr = procure(x@CostlySeed_r4, matches_chr = x@CostlySeed_r4@label_1L_chr,
#                                                                                                             what_1L_chr = "names"),
#                                                                           force_standard_1L_lgl = x@force_standard_1L_lgl,
#                                                                           fuzzy_logic_1L_chr = x@fuzzy_logic_1L_chr,
#                                                                           max_distance_1L_dbl = x@max_distance_1L_dbl)
#                          }
#                          if(type_1L_chr == "final"){
#                            x@fuzzy_logic_1L_chr <- character(0)
#                          }
#                        }else{
#                          if(type_1L_chr == "slot"){
#                            x <- update_module_slot(x,
#                                                    y_r3 = y,
#                                                    what_1L_chr = what_1L_chr,
#                                                    new_val_xx = new_val_xx)
#                          }
#                        }
#                      }
#                      return(x)
#                    })
#
