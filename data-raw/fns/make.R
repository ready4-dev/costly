make_assignment_lup <-  function(ds_tb, # export to programs pkg
                                 group_idx_1L_int = 2L,
                                 group_var_nm_1L_chr = "participation",
                                 response_id_indcs_int = integer(0),#3L:6L,
                                 response_id_var_nms_chr = character(0),#c("response_ids_bl_chr","response_ids_8_wk_chr","response_ids_6_mnth_chr","response_ids_ruq_6_mnth_chr")
                                 uid_idx_1L_int = 1L,
                                 uid_var_nm_1L_chr = "uid_chr"){
  if(!tibble::is_tibble(ds_tb))
    ds_tb <- tibble::as_tibble(ds_tb)
  assignment_lup <- ds_tb %>% dplyr::select(tidyselect::all_of(c(uid_idx_1L_int, group_idx_1L_int, response_id_indcs_int)))
  names(assignment_lup) <- c(uid_var_nm_1L_chr, group_var_nm_1L_chr, response_id_var_nms_chr)
  assignment_lup <- dplyr::filter(assignment_lup, !is.na(!!rlang::sym(uid_var_nm_1L_chr))) %>% dplyr::filter(!!rlang::sym(uid_var_nm_1L_chr) != "")
  return(assignment_lup)
}
make_country_correspondences <- function(option_1L_chr = "custom",
                                         old_nms_chr = character(0),
                                         new_nms_chr = character(0)){
  if(option_1L_chr == "cities"){
    old_nms_chr = c("Azores", "Canary Islands", "Easter Island",
                    "East Timor", "Ivory Coast", "Kosovo", "Madeira",
                    "Netherlands Antilles", "Sicily", "Vatican City")
    new_nms_chr = c("Portugal","Spain","Chile",
                    "Timor-Leste", "Côte d'Ivoire", "Kosovo", "Portugal",
                    "Bonaire, Sint Eustatius and Saba", "Italy", "Holy See (Vatican City State)")
  }
  if(option_1L_chr == "currencies"){
    correspondences_chr <- c(Abkhazia = "Abkhazia", `Akrotiri and Dhekelia` = "Akrotiri and Dhekelia",
                             Artsakh = "Artsakh", `Saint Helena, Ascension and Tristan da Cunha` = "Ascension Island", Congo = "Congo, Republic of the",
                             Guernsey = "Bailiwick of Guernsey", `Timor-Leste` = "East Timor", `Korea, Democratic People's Republic of` = "Korea, North", Kosovo = "Kosovo", `Northern Cyprus`= "Northern Cyprus", `Bonaire, Sint Eustatius and Saba` = "Saba",
                             `Western Sahara` = "Sahrawi Republic[I]", `Somaliland` = "Somaliland", `South Ossetia` = "South Ossetia", Transnistria = "Transnistria", `Holy See (Vatican City State)`= "Vatican City")
    old_nms_chr = unname(correspondences_chr)
    new_nms_chr = names(correspondences_chr)
  }
  correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(),
                                                                               old_nms_chr = old_nms_chr,
                                                                               new_nms_chr = new_nms_chr)
  return(correspondences_x_r3)
}
make_country_standards <- function(as_list_1L_lgl = F,
                                   append_ls = NULL){
  X <- add_country_standards()
  standards_xx <- make_standards_xx(as_list_1L_lgl,
                                    append_ls = append_ls,
                                    var_nms_chr = get_country_standards(T),
                                    label_1L_chr = "Country",
                                    Ready4useDyad_r4 = X)
  return(standards_xx)
}
make_currency_ls <- function(country_1L_chr, # Needs upating?
                             currency_tb = NULL,
                             format_1L_chr = "Name",
                             match_var_nm_1L_chr = "ISO code[2]", # update with fn
                             what_1L_chr = "Symbol[D] orAbbrev.[3]"){ # update with fn
  currency_code_1L_chr <- get_currency(country_1L_chr,
                                       currency_tb = currency_tb,
                                       format_1L_chr = format_1L_chr,
                                       match_var_nm_1L_chr = match_var_nm_1L_chr,
                                       what_1L_chr = NA_character_)
  currency_symbol_1L_chr <- get_currency(country_1L_chr,
                                         currency_tb = currency_tb,
                                         format_1L_chr = format_1L_chr,
                                         match_var_nm_1L_chr = match_var_nm_1L_chr,
                                         what_1L_chr = what_1L_chr)
}
make_currency_standards <- function(as_list_1L_lgl = F,
                                    append_ls = NULL){
  X <- add_curency_standards()
  standards_xx <- make_standards_xx(as_list_1L_lgl,
                                    append_ls = append_ls,
                                    var_nms_chr = get_currency_standards(T),
                                    label_1L_chr = "Currency",
                                    Ready4useDyad_r4 = X)
  return(standards_xx)
}
make_defaults <- function(label_1L_chr = "Standardised",
                          what_1L_chr = c("all", "arguments", "correspondences", "logic", "seed", "reference"),
                          force_standard_1L_lgl = F){
  what_1L_chr <- match.arg(what_1L_chr)
  default_ls <- list(CostlySeed_r4 = CostlySeed(label_1L_chr = label_1L_chr),
                     args_ls = list(case_when_false_1L_chr = NA_character_,
                                    case_when_true_1L_chr = NA_character_,
                                    case_when_true_ls = NULL,
                                    case_when_var_1L_chr = NA_character_,
                                    filter_cdn_1L_chr = NA_character_,
                                    tf_false_val_1L_lgl = T),
                     fuzzy_logic_1L_chr = "jw",
                     correspondences_x_r3 = ready4show::ready4show_correspondences())

  default_xx <- default_ls
  if(what_1L_chr == c("arguments")){
    default_xx <- default_xx$args_ls
  }
  if(what_1L_chr == c("correspondences")){
    default_xx <- default_xx$correspondences_x_r3
  }
  if(what_1L_chr == c("logic")){
    default_xx <- default_xx$fuzzy_logic_1L_chr
  }
  if(what_1L_chr == c("seed")){
    default_xx <- default_xx$CostlySeed_r4
  }
  if(what_1L_chr == c("reference")){
    default_xx <- default_xx$seed_var_nms_chr
  }
  return(default_xx)
}
make_ds_names <- function(label_1L_chr = "Standardised",
                          file_nm_1L_lgl = T,
                          type_chr = c("Input", "Output"),
                          what_chr = c("Correspondences","Lookup", "Seed", "Standards", "Validation")){
  testit::assert("label_1L_chr needs to be a length one character vector", length(label_1L_chr)==1 && is.character(label_1L_chr))
  if(identical(type_chr, character(0))){
    names_chr <- what_chr
  }else{
    type_chr <- match.arg(type_chr, several.ok = T) %>% sort()
    what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
    names_chr <- type_chr %>% purrr::map(~{
      if(.x == "Input"){
        suffices_chr <- intersect(what_chr, c("Correspondences", "Seed", "Standards"))
      }else{
        suffices_chr <- intersect(what_chr, c("Correspondences", "Lookup", "Validation"))
      }
      paste0(.x,"_",suffices_chr)

    }) %>% purrr::flatten_chr()
    if(file_nm_1L_lgl){
      names_chr <- names_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(),
                                                                                                                       old_nms_chr = names_chr,
                                                                                                                       new_nms_chr = paste0(label_1L_chr,"_",names_chr)),
                                                                          match_var_nm_1L_chr = "old_nms_chr",
                                                                          match_value_xx = .x,
                                                                          target_var_nm_1L_chr = "new_nms_chr")) %>%
        purrr::map_chr(~ifelse(endsWith(.x,"_"),NA_character_,.x)) %>% purrr::discard(is.na)
    }else{
      names_chr <- names_chr
    }
  }
  return(names_chr)
}
make_medications_lup <- function(key_tb,
                                 prices_tb,
                                 add_dosage_1L_lgl = T,
                                 brand_var_nm_1L_chr = "Brand Name",
                                 # drop_chr = c("Formulary", "Program", "Manufacturer Code", "Responsible Person", "Maximum Repeats", "Claimed Price for Pack",
                                 #              "Claimed Price to Pharmacist", "Claimed DPMQ", "Premium","C'wlth Pays Premium"),
                                 integers_1L_lgl = TRUE, ##??
                                 keep_chr = c("Response", "Medication", "Legal Instrument MoA", "Brand Name", "Maximum Quantity", "DPMQ", "Maximum Patient Charge", "Milligrams", "AMT Trade Product Pack", "Per Tablet", "Per mg"),
                                 #medications_lup = NULL,
                                 medication_var_nm_1L_chr = "`Legal Instrument Drug`",
                                 milligrams_var_nm_1L_chr = "AMT Trade Product Pack",
                                 plural_chr = "",
                                 range_int = NA_integer_,
                                 reference_1L_int = integer(0),
                                 rename_meds_lup = ready4show::ready4show_correspondences(),
                                 replace_blanks_1L_lgl = F,
                                 spaced_1L_lgl = TRUE,

                                 units_chr = "mg"){
  medications_lup <- key_tb %>%
    dplyr::left_join(prices_tb %>% dplyr::mutate(Medication = `Legal Instrument Drug`))
  ###
  medications_tb <- medications_lup %>% dplyr::filter(is.na(`Item Code`)) %>% dplyr::select(c("Response", "Medication"))
  ###
  medications_lup <- medications_lup %>% dplyr::filter(!is.na(`Item Code`))
  ###
  medications_lup <- dplyr::bind_rows(medications_lup, medications_tb %>% # make update fn
                                        dplyr::left_join(prices_tb %>% dplyr::mutate(Medication = !!rlang::sym(brand_var_nm_1L_chr)))) %>% # make arg
    dplyr::arrange(as.numeric(Response)) # make arg
  ###
  ## unmatched_chr <- medications_lup %>% dplyr::filter(is.na(`Item Code`)) %>% dplyr::pull(Medication) # make fn
  ###
  # rename_lup <- ready4show::ready4show_correspondences() %>% # make arg
  #   ready4show::renew.ready4show_correspondences(old_nms_chr = setdiff(unmatched_chr, c("None","Agomelatine", "Pain relief", "St John’s Wort","Valerian","Vitamins","Other","blank response")),
  #                                                new_nms_chr = c("Dosulepin", "Fluvoxamine", "Lithium", "Valproic acid"))
  ###
  medications_lup <- update_medications_lup(medications_lup,
                                            integers_1L_lgl = integers_1L_lgl, ##??
                                            keep_chr = keep_chr,
                                            medication_var_nm_1L_chr = medication_var_nm_1L_chr,
                                            milligrams_var_nm_1L_chr = milligrams_var_nm_1L_chr,
                                            plural_chr = plural_chr,
                                            prices_tb = prices_tb,
                                            range_int = range_int,
                                            reference_1L_int =  reference_1L_int,
                                            rename_meds_lup = rename_meds_lup,
                                            replace_blanks_1L_lgl = replace_blanks_1L_lgl,
                                            spaced_1L_lgl = spaced_1L_lgl,
                                            type_1L_chr = "dosage",
                                            units_chr = units_chr)


  return(medications_lup)

}
make_period_patterns <- function(range_int = 1L:12L,
                                 integers_1L_lgl = TRUE,
                                 plural_chr = "s",
                                 spaced_1L_lgl = TRUE,
                                 type_1L_chr = c("english","numeric","blank"),
                                 units_chr = c("minute","hour","week","month","year")){
  type_1L_chr <- match.arg(type_1L_chr)
  period_patterns_chr <- purrr::map(plural_chr,
                                    ~ {
                                      plural_1L_chr <- .x
                                      spacing_chr <- c("","\\s+")
                                      if(!is.na(spaced_1L_lgl)){
                                        spacing_chr <- spacing_chr[spaced_1L_lgl +1]
                                      }
                                      if(type_1L_chr == "english"){
                                        patterns_chr <- units_chr %>% purrr::map(~{
                                          unit_1L_chr <- .x
                                          range_int %>% purrr::map(~paste0(english::words(.x),paste0(spacing_chr, unit_1L_chr),ifelse(.x>1,plural_1L_chr,""))) %>% purrr::flatten_chr()}) %>% purrr::flatten_chr()
                                      }
                                      if(type_1L_chr %in% c("blank","numeric")){
                                        patterns_chr <- c(paste0(ifelse(type_1L_chr == "blank","",1),spacing_chr,units_chr),
                                                          spacing_chr %>% purrr::map(~paste0(ifelse(type_1L_chr == "blank","",
                                                                                                    ifelse(integers_1L_lgl,"[[:digit:]]+","\\d*\\.?\\d+")),.x,units_chr,plural_1L_chr)) %>% purrr::flatten_chr())
                                      }
                                      patterns_chr
                                    }) %>% purrr::flatten_chr() %>% unique()


  return(period_patterns_chr)
}
make_standardised_dss <- function(label_1L_chr = "Country",
                                  lookup_Ready4useDyad = ready4use::Ready4useDyad(),
                                  seed_Ready4useDyad = ready4use::Ready4useDyad(),
                                  standards_Ready4useDyad = ready4use::Ready4useDyad(),
                                  type_chr = c("Input", "Output"),
                                  validation_ls = list(),
                                  what_chr = c("Correspondences","Lookup", "Seed", "Standards", "Validation"),
                                  correspondences_x_r3 = ready4show::ready4show_correspondences(),
                                  correspondences_y_r3 = ready4show::ready4show_correspondences()){
  testit::assert("label_1L_chr needs to be a length one character vector", length(label_1L_chr)==1 && is.character(label_1L_chr))
  type_chr <- match.arg(type_chr, several.ok = T) %>% sort()
  what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
  standardised_dss_ls <- list(correspondences_x_r3, seed_Ready4useDyad, standards_Ready4useDyad, correspondences_y_r3, lookup_Ready4useDyad, validation_ls) %>% stats::setNames(make_ds_names(label_1L_chr))
  standardised_dss_ls <- standardised_dss_ls[make_ds_names(label_1L_chr, type_chr = type_chr, what_chr = what_chr)]
  return(standardised_dss_ls)
}
make_standards_xx <- function(as_list_1L_lgl = T,
                              append_ls = NULL,
                              var_nms_chr = NA_character_,
                              label_1L_chr = "Standardised",
                              Ready4useDyad_r4 = ready4use::Ready4useDyad()){
  X <- CostlyStandards()
  X@Ready4useDyad_r4 <- Ready4useDyad_r4

  X@label_1L_chr <- label_1L_chr
  if(!is.na(var_nms_chr[1])){
    X@include_chr <- get_corresponding_var(X@Ready4useDyad_r4,
                                           matches_chr = var_nms_chr,
                                           what_1L_chr = "concepts")
  }
  if(as_list_1L_lgl){
    standards_xx <- append(append_ls, list(X) %>% stats::setNames(X@label_1L_chr))
  }else{
    standards_xx <- X
  }
  return(standards_xx)
}
make_validation_ls <- function(allowed_xx,
                               ds_df,
                               var_nm_1L_chr,
                               sort_1L_lgl = F){
  type_chr <- c("Absent", "Valid", "Invalid", "Valid", "Invalid")
  what_chr <- c(rep("Values", 3), rep("Cases", 2))
  validation_ls <- purrr::map2(type_chr,
                               what_chr,
                               ~ make_validation_output(allowed_xx %>% sort(), ds_df = ds_df, var_nm_1L_chr = var_nm_1L_chr, sort_1L_lgl = sort_1L_lgl, type_1L_chr = .x, what_1L_chr = .y)) %>%
    stats::setNames(purrr::map2_chr(type_chr, what_chr, ~paste0(.x,"_",.y)))
  return(validation_ls)
}
make_validation_output <- function(allowed_xx,
                                   ds_df,
                                   var_nm_1L_chr,
                                   sort_1L_lgl = F,
                                   type_1L_chr = "Valid",
                                   what_1L_chr = "Cases"){
  if(is.data.frame(ds_df) & !tibble::is_tibble(ds_df)){
    ds_tb <- tibble::as_tibble(ds_df)
    #message("ds_df has been converted to a tibble")
  }else{
    ds_tb <- ds_df
  }
  output_xx <- NULL
  if(what_1L_chr %in% c("cases","Cases")){
    if(type_1L_chr %in% c("valid", "Valid")){
      output_xx <- dplyr::filter(ds_tb, !!rlang::sym(var_nm_1L_chr) %in% allowed_xx)
    }
    if(type_1L_chr %in% c("invalid", "Invalid")){
      output_xx <- dplyr::filter(ds_tb, !(!!rlang::sym(var_nm_1L_chr)) %in% allowed_xx)
    }
  }
  if(what_1L_chr %in% c("values", "Values")){
    test_xx <- dplyr::pull(ds_tb, !!rlang::sym(var_nm_1L_chr))
    if(type_1L_chr %in% c("valid", "Valid")){
      output_xx <- intersect(test_xx, allowed_xx)
    }
    if(type_1L_chr %in% c("invalid", "Invalid")){
      output_xx <- setdiff(test_xx, allowed_xx)
    }
    if(type_1L_chr %in% c("absent", "Absent")){
      output_xx <- setdiff(allowed_xx, test_xx)
    }
  }
  if(sort_1L_lgl & !is.null(output_xx)){
    if(tibble::is_tibble(output_xx)){
      output_xx <- output_xx %>% dplyr::arrange(!!rlang::sym(var_nm_1L_chr))
    }
    if(is.atomic(output_xx))
      output_xx <- sort(output_xx)
  }
  return(output_xx)
}
