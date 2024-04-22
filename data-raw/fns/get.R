get_corresponding_var <- function(Ready4useDyad_r4,
                                  matches_chr,
                                  what_1L_chr = "concepts"){
  match_var_nm_1L_chr = ifelse(what_1L_chr == "names","var_ctg_chr","var_nm_chr")
  target_var_nm_1L_chr = ifelse(what_1L_chr == "names","var_nm_chr","var_ctg_chr")
  matches_chr <- match.arg(matches_chr, choices = Ready4useDyad_r4@dictionary_r3 %>% dplyr::pull(!!rlang::sym(match_var_nm_1L_chr)),several.ok = T)
  corresponding_chr <- matches_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(Ready4useDyad_r4@dictionary_r3,
                                                                                match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                                                match_value_xx = .x,
                                                                                target_var_nm_1L_chr = target_var_nm_1L_chr))
  return(corresponding_chr)
}
get_country_standards <- function(names_1L_lgl = F,
                                  default_pkg_ds_chr = c("ISO_3166_1", "ISOcodes"),
                                  indices_int = c(4:6,2,1),
                                  tbl_index_1L_int = integer(0),
                                  type_1L_chr = "Countries",
                                  url_1L_chr = character(0),
                                  what_1L_chr = "Input_Seed",
                                  X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(!names_1L_lgl){
    indices_int <- integer(0)
  }
  country_standards_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr,  X_Ready4useRepos = X_Ready4useRepos)
  return(country_standards_xx)
}
get_currency_standards <- function(names_1L_lgl = F,
                                   default_pkg_ds_chr = c("ISO_4217", "ISOcodes"),
                                   indices_int = c(3,1),
                                   tbl_index_1L_int = integer(0),
                                   type_1L_chr = "Currency",
                                   url_1L_chr = character(0),
                                   what_1L_chr = "Input_Seed",
                                   X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(!names_1L_lgl){
    indices_int <- integer(0)
  }
  country_standards_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr,  X_Ready4useRepos = X_Ready4useRepos)
  return(country_standards_xx)
}
get_currency <- function(country_1L_chr,
                         case_when_false_1L_chr = NA_character_,
                         case_when_true_1L_chr = NA_character_,
                         case_when_true_ls = NULL,
                         case_when_var_1L_chr = NA_character_,
                         country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
                         currency_tb = NULL,
                         filter_cdn_1L_chr = NA_character_,
                         force_standard_1L_lgl = F,
                         format_1L_chr = "Name",
                         fuzzy_logic_1L_chr = "jw",
                         indcs_int = c(2:3),
                         seed_df = NULL,
                         match_var_nm_1L_chr = "ISO code[2]",
                         max_distance_1L_dbl = Inf,
                         target_var_nm_1L_chr = "Alpha_3",
                         tf_false_val_1L_lgl = T,
                         type_1L_chr = "Country",
                         url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                         #preferred_1L_chr = character(0),
                         what_1L_chr = "Symbol",
                         correspondences_x_r3 = ready4show::ready4show_correspondences(),
                         correspondences_y_r3 = ready4show::ready4show_correspondences(),
                         X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(format_1L_chr != target_var_nm_1L_chr){
    country_1L_chr <- transform_country(country_1L_chr = country_1L_chr, from_1L_chr = format_1L_chr, what_1L_chr = target_var_nm_1L_chr)
  }
  currency_1L_chr <- country_1L_chr %>%
    countrycode::countrycode("iso3c","iso4217c")
  if(!is.na(what_1L_chr)){
    if(identical(correspondences_x_r3, ready4show::ready4show_correspondences())){
      correspondences_x_r3 <- ready4show::renew.ready4show_correspondences(correspondences_x_r3, old_nms_chr = c("Name","Symbol","Code","Unit", "Number"),
                                                                                   new_nms_chr = c("Currency[1][2]","Symbol[D] orAbbrev.[3]","ISO code[2]","Fractionalunit","Numberto basic"))
    }
    if(is.null(currency_tb)){
      currency_tb <- make_currencies_dss(case_when_false_1L_chr = case_when_false_1L_chr, # Replace with get_standardised_dss
                                         case_when_true_1L_chr = case_when_true_1L_chr,
                                         case_when_true_ls = case_when_true_ls,
                                         case_when_var_1L_chr = case_when_var_1L_chr,
                                         country_var_nms_chr = country_var_nms_chr,
                                         filter_cdn_1L_chr = filter_cdn_1L_chr,
                                         force_standard_1L_lgl = force_standard_1L_lgl,
                                         fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                         indcs_int = indcs_int,
                                         max_distance_1L_dbl = max_distance_1L_dbl,
                                         seed_df = seed_df,
                                         tf_false_val_1L_lgl = tf_false_val_1L_lgl,
                                         type_1L_chr = type_1L_chr,
                                         url_1L_chr = url_1L_chr,
                                         correspondences_x_r3 = correspondences_y_r3,
                                         X_Ready4useRepos = X_Ready4useRepos)$Currencies_By_Country_Lookup
    }
    currency_1L_chr <- currency_tb[which(currency_1L_chr == (currency_tb[,which(names(currency_tb) == match_var_nm_1L_chr)] %>%
                                                               unlist() %>% as.vector()))[1],
                                   which(names(currency_tb) == ready4::get_from_lup_obj(correspondences_x_r3,
                                                                                        match_var_nm_1L_chr = "old_nms_chr",
                                                                                        match_value_xx = Hmisc::capitalize(what_1L_chr),
                                                                                        target_var_nm_1L_chr = "new_nms_chr"))][[1,1]]
  }
  return(currency_1L_chr)
}
# From: https://stackoverflow.com/questions/60719592/r-built-in-list-of-currency-symbols
get_currency_tbls <- function(type_1L_chr = c("Both","Country","Currency"),
                              country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
                              indcs_int = c(2:3),
                              url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                              correspondences_x_r3 = ready4show::ready4show_correspondences(),
                              X_Ready4useRepos = ready4use::Ready4useRepos()){
  type_1L_chr <- match.arg(type_1L_chr)
  correspondences_x_r3 <- update_currency_correspondences(correspondences_x_r3, country_var_nms_chr = country_var_nms_chr, type_1L_chr = "Both")
  if(type_1L_chr != "Both"){
    element_1L_chr <- ready4::get_from_lup_obj(correspondences_x_r3, match_var_nm_1L_chr = "old_nms_chr",
                                               match_value_xx = type_1L_chr, target_var_nm_1L_chr = "new_nms_chr")
    indcs_int <- indcs_int[which(correspondences_x_r3$new_nms_chr == element_1L_chr)]
    correspondences_x_r3 <- update_currency_correspondences(correspondences_x_r3, country_var_nms_chr = country_var_nms_chr, type_1L_chr = type_1L_chr)
  }
  if(identical(X_Ready4useRepos, ready4use::Ready4useRepos())){
    currency_tbls_ls <- url_1L_chr %>% rvest::read_html() %>% rvest::html_table()
    currency_tbls_ls <- currency_tbls_ls[indcs_int] %>%
      stats::setNames(correspondences_x_r3$new_nms_chr)
  }else{
    currency_tbls_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = correspondences_x_r3$new_nms_chr, metadata_1L_lgl = F)
  }
  if(length(currency_tbls_ls) == 1){
    currency_tbl_xx <- currency_tbls_ls %>% purrr::pluck(1)
  }else{
    currency_tbl_xx <- currency_tbls_ls
  }
  return(currency_tbl_xx)
}
get_missing_medications <- function(medications_lup,
                                    reference_var_chr = c("DPMQ", "Per Tablet")){
  reference_var_1L_chr <- intersect(reference_var_chr, names(medications_lup))[1]
  missing_chr <- medications_lup %>% dplyr::filter(is.na(!!rlang::sym(reference_var_1L_chr))) %>% dplyr::pull(Medication)
  return(missing_chr)
}
get_patterns <- function(descriptions_chr,
                         add_numeric_1L_lgl = TRUE,
                         flatten_1L_lgl = FALSE,
                         integers_1L_lgl = TRUE,
                         plural_chr = "s",
                         range_int = 1L:12L,
                         reference_1L_int = integer(0),
                         replace_blanks_1L_lgl = FALSE,
                         spaced_1L_lgl = TRUE,
                         strict_1L_lgl = TRUE,
                         type_1L_chr = c("pattern", "end", "start", "match", "mismatch", "quantity"),
                         unique_1L_lgl = TRUE,
                         units_chr = c("minute","hour","week","month","year"),
                         what_1L_chr = c("character", "double", "integer", "logical")){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  if(!is.na(range_int[1]) && ! identical(range_int, integer(0))){
    english_chr <- make_period_patterns(integers_1L_lgl = integers_1L_lgl, plural_chr = plural_chr, range_int = range_int, spaced_1L_lgl = spaced_1L_lgl, type_1L_chr = "english", units_chr = units_chr)
  }else{
    english_chr <- character(0)
  }
  numeric_chr <- make_period_patterns(integers_1L_lgl = ifelse(what_1L_chr == "double", FALSE, integers_1L_lgl),
                                      plural_chr = plural_chr,
                                      spaced_1L_lgl = spaced_1L_lgl,
                                      type_1L_chr = "numeric",
                                      units_chr = units_chr)
  patterns_xx <- purrr::map(descriptions_chr,
                            ~ {
                              description_1L_chr <- .x
                              purrr::map(c(english_chr,numeric_chr),
                                         ~ {
                                           periods_chr <- stringr::str_extract_all(description_1L_chr,.x) %>% purrr::flatten_chr()
                                           if(identical(periods_chr, character(0))){
                                             NA_character_
                                           }else{periods_chr}
                                         }
                              ) %>% purrr::flatten_chr() %>% purrr::discard(is.na)
                            })
  if(unique_1L_lgl){
    patterns_xx <- patterns_xx %>% purrr::map(~unique(.x))
  }
  remove_chr <- make_period_patterns(integers_1L_lgl = integers_1L_lgl, plural_chr = plural_chr, spaced_1L_lgl = spaced_1L_lgl, type_1L_chr = "blank", units_chr = units_chr)

  extension_ls <- patterns_xx %>% purrr::map(~{
    all_chr <- .x
    extension_ls <- all_chr %>% purrr::map(~{
      extend_1L_chr <- .x
      extension_chr <- remove_chr %>% purrr::map_chr(~stringi::stri_replace_last_regex(extend_1L_chr,.x,""))
      extension_chr[nchar(extension_chr)==max(nchar(extension_chr))] %>% unique()})
    #extension_ls
  })

  if(!identical(reference_1L_int, integer(0))){ ## Still to test
    patterns_xx <- patterns_xx %>% purrr::map2(extension_ls,
                                               ~{
                                                 all_chr <- .x
                                                 if(length(all_chr)>1){
                                                   .y %>%
                                                     purrr::map_chr(~
                                                                      {
                                                                        extension_chr <- .x
                                                                        # extension_chr <- extension_chr[nchar(extension_chr)==max(nchar(extension_chr))] %>% unique()
                                                                        if(plural_chr[reference_1L_int] == ""){
                                                                          extension_chr <- extension_chr[nchar(extension_chr)==min(nchar(extension_chr))] %>% unique()
                                                                        }else{
                                                                          if(any(endsWith(extension_chr, plural_chr[reference_1L_int]))){
                                                                            extension_chr <- extension_chr[which(endsWith(extension_chr, plural_chr[reference_1L_int]))]
                                                                            extension_chr <- extension_chr[nchar(extension_chr)==max(nchar(extension_chr))]
                                                                          }
                                                                          extension_chr <- unique(extension_chr)
                                                                        }
                                                                        #}
                                                                        extension_chr
                                                                      } )

                                                 }else{
                                                   all_chr
                                                 }
                                               })
  }
  patterns_xx <- patterns_xx %>% purrr::map2(extension_ls,~{
    all_chr <- .x
    if(length(all_chr)>1){
      only_numbers_chr <- .y %>% purrr::flatten_chr() %>% purrr::map_chr(~{
        all_1L_chr <- .x
        candidates_chr <- remove_chr %>%
          purrr::map_chr(~stringr::str_replace_all(all_1L_chr,.x,""))
        candidates_chr <- suppressWarnings(candidates_chr[!is.na(as.numeric(candidates_chr))]) %>% stringr::str_squish() %>% unique()
        if(identical(candidates_chr, character(0))){
          candidates_chr <- NA_character_
        }
        candidates_chr
      }) %>% purrr::discard(is.na)

      if(identical(only_numbers_chr, character(0))){
        all_chr
      }else{
        all_chr[nchar(only_numbers_chr)==max(nchar(only_numbers_chr))] %>% unique()
      }
    }else{
      all_chr
    }
  })
  if(type_1L_chr %in% c("start", "end", "match",
                        "quantity")){ # , "logical"
    replace_blanks_1L_lgl <- T
  }
  if(replace_blanks_1L_lgl){
    patterns_xx <- patterns_xx %>% purrr::map(~{
      if(identical(.x, character(0))){NA_character_}else{.x %>% purrr::map_chr(~if(identical(.x, character(0))){NA_character_}else{.x})}
    })
  }
  if(type_1L_chr %in% c("start", "end", "match", "mismatch", "quantity")){ # , "logical"
    start_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(is.na(.y[1]), NA_character_, suppressWarnings(sub(paste0(.y,".*"), "", .x)))) ## suppressWarnings?
    end_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(is.na(.y[1]), NA_character_, suppressWarnings(sub(paste0(".*", .y), "", .x)))) ## suppressWarnings?
    if(strict_1L_lgl){
      match_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(is.na(.x) && is.na(.y[1]), NA_character_,
                                                                         {
                                                                           all_chr <- ifelse(stringr::str_squish(.x) == .y,.x, NA_character_) %>%
                                                                             purrr::discard(is.na)
                                                                           ifelse(length(all_chr)>0,.x,NA_character_)
                                                                         }
      ))
      mismatch_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(is.na(.x) && is.na(.y[1]), NA_character_,
                                                                            {
                                                                              all_chr <- ifelse(stringr::str_squish(.x) == .y,.x, NA_character_) %>%
                                                                                purrr::discard(is.na)
                                                                              ifelse(length(all_chr)==0,.x,NA_character_)
                                                                            }
      ))
    }else{
      match_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(is.na(.y[1]), NA_character_, .x))
      mismatch_ls <- descriptions_chr %>% purrr::map2(patterns_xx, ~ ifelse(!is.na(.y[1]), NA_character_, .x))
    }
  }
  if(type_1L_chr == "start"){
    patterns_xx <- start_ls
  }
  if(type_1L_chr == "end"){
    patterns_xx <- end_ls
  }
  if(type_1L_chr %in% c("match", "mismatch", "quantity")){ #, "logical"
    remove_chr <- make_period_patterns(integers_1L_lgl = integers_1L_lgl, plural_chr = plural_chr, spaced_1L_lgl = spaced_1L_lgl, type_1L_chr = "blank", units_chr = units_chr)
    quantity_ls <- patterns_xx %>% purrr::map2(match_ls,~{
      if(!identical(.x,stringr::str_squish(.y))){
        NA_character_#.x
      }else{
        phrases_chr <- .x
        phrases_chr %>% purrr::map_chr(~{
          phrase_1L_chr <- .x
          go_1L_lgl <- remove_chr %>% purrr::map_lgl(~stringr::str_detect(phrase_1L_chr,.x)) %>% any()
          ifelse(go_1L_lgl,
                 {
                   all_chr <- remove_chr %>% purrr::map_chr(~stringr::str_remove_all(phrase_1L_chr,.x))
                   all_chr[nchar(all_chr)==min(nchar(all_chr))]
                 },
                 phrase_1L_chr)
        })
      }
    })
    if(add_numeric_1L_lgl){
      quantity_ls <- quantity_ls %>% purrr::map2(descriptions_chr, ~ifelse(is.na(.x),
                                                                           ifelse(as.numeric(.y) %>% suppressWarnings() %>% is.na(),NA_character_,.y),
                                                                           .x))
      if(type_1L_chr %in% c("match", "mismatch")){
        allowable_chr <- descriptions_chr[!(quantity_ls %>% purrr::flatten_chr() %>% is.na())] %>% unique()

        #if(type_1L_chr == "match"){
        match_ls <- descriptions_chr %>% purrr::map(~ifelse(.x %in% allowable_chr, .x, NA_character_))
        #}
        #if(type_1L_chr == "mismatch"){
        mismatch_ls <- descriptions_chr %>% purrr::map(~ifelse(!.x %in% allowable_chr, .x, NA_character_))
        #}
      }
    }
    if(type_1L_chr == "match"){
      patterns_xx <- match_ls
    }
    if(type_1L_chr == "mismatch"){
      patterns_xx <- mismatch_ls
    }
    if(type_1L_chr == "quantity"){
      patterns_xx <- quantity_ls
    }
    if(type_1L_chr %in% c("match", "quantity")){
      if(what_1L_chr == "double"){
        patterns_xx <- quantity_ls %>% purrr::map(~ as.double(.x))
      }
      if(what_1L_chr == "integer"){
        patterns_xx <- quantity_ls %>% purrr::map(~ as.integer(.x))
      }
    }
  }
  if(what_1L_chr == "logical"){
    patterns_xx <- patterns_xx %>% purrr::map(~ !is.na(.x[1])) ####
  }
  if(flatten_1L_lgl){
    if(what_1L_chr == "character"){
      fn <- purrr::flatten_chr
    }
    if(what_1L_chr == "double"){
      fn <- purrr::flatten_dbl
    }
    if(what_1L_chr == "integer"){
      fn <- purrr::flatten_int
    }
    if(what_1L_chr == "logical"){
      fn <- purrr::flatten_lgl
    }
    patterns_xx <- patterns_xx %>% fn
  }
  return(patterns_xx)
}
get_pattern_mismatches <- function(ds_tb, # unused?
                                   var_nms_chr,
                                   units_chr,
                                   add_numeric_1L_lgl = TRUE,
                                   integers_1L_lgl = FALSE,
                                   invert_1L_lgl = FALSE,
                                   plural_chr = c("s", ""),
                                   range_int = NA_integer_,
                                   reference_1L_int = 1L,
                                   sort_1L_lgl = FALSE,
                                   spaced_1L_lgl = NA,
                                   strict_1L_lgl = TRUE,
                                   trim_1L_lgl = TRUE,
                                   what_1L_chr = c("unique", "all", "logical")
){
  what_1L_chr <- match.arg(what_1L_chr)
  mismatches_xx <- var_nms_chr %>% purrr::map(~ds_tb %>% dplyr::pull(!!rlang::sym(.x)) %>%
                                                get_patterns(integers_1L_lgl = integers_1L_lgl,
                                                             add_numeric_1L_lgl = add_numeric_1L_lgl,
                                                             plural_chr = plural_chr,
                                                             range_int = range_int,
                                                             reference_1L_int = reference_1L_int,
                                                             replace_blanks_1L_lgl = TRUE,
                                                             spaced_1L_lgl = spaced_1L_lgl,
                                                             strict_1L_lgl = strict_1L_lgl,
                                                             unique_1L_lgl = TRUE,
                                                             units_chr = units_chr,
                                                             type_1L_chr = ifelse(invert_1L_lgl,"match","mismatch"),
                                                             flatten_1L_lgl = TRUE,
                                                             #what_1L_chr = "double"
                                                ))
  if(trim_1L_lgl){
    mismatches_xx <- mismatches_xx %>% purrr::map(~ifelse(trimws(.x)=="",NA_character_,.x))
  }
  if(what_1L_chr == "logical"){
    mismatches_xx <- mismatches_xx %>% purrr::map(~!is.na(.x))
  }
  if(what_1L_chr == "unique")
    mismatches_xx <- mismatches_xx %>% purrr::flatten_chr() %>% unique() %>% purrr::discard(is.na)
  if(sort_1L_lgl){
    mismatches_xx <- mismatches_xx %>% sort()
  }
  return(mismatches_xx)
}
get_seed_cities <- function(names_1L_lgl = F,
                            indices_int = 2L,
                            default_pkg_ds_chr = c("world.cities", "maps"),
                            tbl_index_1L_int = integer(0),
                            type_1L_chr = "Cities",
                            url_1L_chr = character(0),
                            what_1L_chr = "Input_Seed",
                            X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(!names_1L_lgl){
    indices_int <- integer(0)
  }
  seed_cities_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr,  X_Ready4useRepos = X_Ready4useRepos)
  return(seed_cities_xx)
}
get_seed_currencies <- function(names_1L_lgl = F,
                               indices_int = 1:4,
                               default_pkg_ds_chr = character(0),
                               tbl_index_1L_int = 2L,
                               type_1L_chr = "Currencies",
                               url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                               what_1L_chr = "Input_Seed",
                               X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(!names_1L_lgl){
    indices_int <- integer(0)
  }
  seed_currencies_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr,  X_Ready4useRepos = X_Ready4useRepos)
  return(seed_currencies_xx)
}
get_seed_ds <- function(label_1L_chr,
                        default_pkg_ds_chr = character(0),
                        indices_int = integer(0),
                        tbl_index_1L_int = integer(0),
                        url_1L_chr = character(0),
                        what_1L_chr = "Input_Seed",
                        X_Ready4useRepos = ready4use::Ready4useRepos()){
  what_1L_chr <- match.arg(what_1L_chr, choices = make_ds_names(file_nm_1L_lgl = F))
  if(identical(X_Ready4useRepos, ready4use::Ready4useRepos())){
    if(!identical(url_1L_chr, character(0))){
      seed_ds_xx <- url_1L_chr %>% rvest::read_html() %>% rvest::html_table()
      seed_ds_xx <-  seed_ds_xx[tbl_index_1L_int] %>%
        #stats::setNames(correspondences_x_r3$new_nms_chr) %>%
        purrr::pluck(1)

    }else{
      testit::assert("If neither a non-empty Ready4useRepos module nor a URL are supplied, then default_pkg_ds_chr must be a length two character vector specifying the dataset name (position 1) and containing R package (position 2)",
                     identical(default_pkg_ds_chr, character(0)) | is.character(default_pkg_ds_chr) | length(default_pkg_ds_chr)==2)
      utils::data(list = default_pkg_ds_chr[1], package = default_pkg_ds_chr[2], envir = environment())
      seed_ds_xx <- eval(parse(text=default_pkg_ds_chr[1]))
    }

  }else{
    seed_ds_xx <- get_standardised_dss(X_Ready4useRepos = X_Ready4useRepos,
                                       label_1L_chr = label_1L_chr,
                                       what_chr = what_1L_chr)
  }
  if(!identical(indices_int, integer(0))){
    seed_ds_xx <- names(seed_ds_xx)
    if(!is.na(indices_int[1])){
      testit::assert("indices_int contains values outside of indices of seed dataset names",
                     identical(setdiff(indices_int, 1:length(seed_ds_xx)) %>% as.numeric(), numeric(0)))
      seed_ds_xx <- seed_ds_xx[indices_int]
    }
  }
  return(seed_ds_xx)
}
get_standardised_dss <- function(X_Ready4useRepos,
                                 label_1L_chr = "Cities",
                                 type_chr = c("Input", "Output"),
                                 what_chr = make_ds_names(type_chr = character(0))){
  what_chr <- match.arg(what_chr, several.ok = T)
  standardised_dss_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = make_ds_names(label_1L_chr, type_chr = type_chr, what_chr = what_chr), metadata_1L_lgl = F)
  if(length(standardised_dss_ls) == 1){
    standardised_dss_xx <- standardised_dss_ls %>% purrr::pluck(1)
  }else{
    standardised_dss_xx <- standardised_dss_ls
  }
  return(standardised_dss_xx)
}
