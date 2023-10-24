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
