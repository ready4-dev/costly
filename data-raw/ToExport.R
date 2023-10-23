## Retrieve from repo
## Apply to grouped ds
## Create repos
## Validate
add_default_country_seed <- function(CostlySeed_r4 = CostlySeed()){
  CostlySeed_r4@Ready4useDyad_r4@ds_tb <- get_seed_cities() %>% tibble::as_tibble()
  CostlySeed_r4@Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(CostlySeed_r4@Ready4useDyad_r4@dictionary_r3,
                                                                                        var_nm_chr = names(CostlySeed_r4@Ready4useDyad_r4@ds_tb),
                                                                                        var_ctg_chr = c("City" ,"Country", "Population","Latitude","Longitude","Capital"),
                                                                                        var_desc_chr = c("City name",
                                                                                                         "Country name", "Population size","Latitude coordinate","Longitude coordinate","Is the nation's capital city" ),
                                                                                        var_type_chr = purrr::map_chr(CostlySeed_r4@Ready4useDyad_r4@ds_tb, ~ class(.x)[1]) %>% unname())
  CostlySeed_r4@include_chr <- procure(CostlySeed_r4, matches_chr = get_seed_cities(T), what_1L_chr = "concepts")
  return(CostlySeed_r4)
}
add_default_currency_seed <- function(CostlySeed_r4 = CostlySeed(),
                                      include_1L_chr = "Currency",
                                      Ready4useDyad_r4 = ready4use::Ready4useDyad()){
  if(identical(Ready4useDyad_r4,ready4use::Ready4useDyad() )){
    CostlySeed_r4@Ready4useDyad_r4@ds_tb <- get_seed_curencies() %>% tibble::as_tibble()
    CostlySeed_r4@Ready4useDyad_r4@dictionary_r3 <- ready4use::renew.ready4use_dictionary(CostlySeed_r4@Ready4useDyad_r4@dictionary_r3,
                                                                                          var_nm_chr = names(CostlySeed_r4@Ready4useDyad_r4@ds_tb),
                                                                                          var_ctg_chr = c("Country", "Currency","Symbol","A3","Fractional","Number"),
                                                                                          var_desc_chr = c("Country name", "Currency name","Currency symbol","Currency alphabetical ISO code (three letter)","Currency's fractional unit", "Number of fractional units in basic unit" ),
                                                                                          var_type_chr = purrr::map_chr(CostlySeed_r4@Ready4useDyad_r4@ds_tb, ~ class(.x)[1]) %>% unname())
  }else{
    CostlySeed_r4@Ready4useDyad_r4 <- Ready4useDyad_r4
  }
  if(include_1L_chr == "Currency"){
    CostlySeed_r4@include_chr <- procure(CostlySeed_r4, matches_chr = get_seed_curencies(T), what_1L_chr = "concepts")
  }
  if(include_1L_chr == "Country"){
    CostlySeed_r4@include_chr <- "Country"
  }
  return(CostlySeed_r4)
}
add_source_label <- function(CostlySource_r4 = CostlySource(),
                             label_1L_chr = "Standardised"){
  CostlySource_r4@label_1L_chr <- label_1L_chr
  return(CostlySource_r4)
}
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
                         x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                         y_ready4show_correspondences = ready4show::ready4show_correspondences(),
                         X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(format_1L_chr != target_var_nm_1L_chr){
    country_1L_chr <- transform_country(country_1L_chr = country_1L_chr, from_1L_chr = format_1L_chr, what_1L_chr = target_var_nm_1L_chr)
  }
  currency_1L_chr <- country_1L_chr %>%
    countrycode::countrycode("iso3c","iso4217c")
  if(!is.na(what_1L_chr)){
    if(identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())){
      x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences, old_nms_chr = c("Name","Symbol","Code","Unit", "Number"),
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
                                         x_ready4show_correspondences = y_ready4show_correspondences,
                                         X_Ready4useRepos = X_Ready4useRepos)$Currencies_By_Country_Lookup
    }
    currency_1L_chr <- currency_tb[which(currency_1L_chr == (currency_tb[,which(names(currency_tb) == match_var_nm_1L_chr)] %>%
                                                               unlist() %>% as.vector()))[1],
                                   which(names(currency_tb) == ready4::get_from_lup_obj(x_ready4show_correspondences,
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
                              x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                              X_Ready4useRepos = ready4use::Ready4useRepos()){
  type_1L_chr <- match.arg(type_1L_chr)
  x_ready4show_correspondences <- update_currency_correspondences(x_ready4show_correspondences, country_var_nms_chr = country_var_nms_chr, type_1L_chr = "Both")
  if(type_1L_chr != "Both"){
    element_1L_chr <- ready4::get_from_lup_obj(x_ready4show_correspondences, match_var_nm_1L_chr = "old_nms_chr",
                                               match_value_xx = type_1L_chr, target_var_nm_1L_chr = "new_nms_chr")
    indcs_int <- indcs_int[which(x_ready4show_correspondences$new_nms_chr == element_1L_chr)]
    x_ready4show_correspondences <- update_currency_correspondences(x_ready4show_correspondences, country_var_nms_chr = country_var_nms_chr, type_1L_chr = type_1L_chr)
  }
  if(identical(X_Ready4useRepos, ready4use::Ready4useRepos())){
    currency_tbls_ls <- url_1L_chr %>% rvest::read_html() %>% rvest::html_table()
    currency_tbls_ls <- currency_tbls_ls[indcs_int] %>%
      stats::setNames(x_ready4show_correspondences$new_nms_chr)
  }else{
    currency_tbls_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = x_ready4show_correspondences$new_nms_chr, metadata_1L_lgl = F)
  }
  if(length(currency_tbls_ls) == 1){
    currency_tbl_xx <- currency_tbls_ls %>% purrr::pluck(1)
  }else{
    currency_tbl_xx <- currency_tbls_ls
  }
  return(currency_tbl_xx)
}
get_seed_curencies <- function(names_1L_lgl = F,
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
  seed_curencies_xx <- get_seed_ds(type_1L_chr, default_pkg_ds_chr = default_pkg_ds_chr, indices_int = indices_int, tbl_index_1L_int = tbl_index_1L_int, url_1L_chr = url_1L_chr, what_1L_chr = what_1L_chr,  X_Ready4useRepos = X_Ready4useRepos)
  return(seed_curencies_xx)
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
        #stats::setNames(x_ready4show_correspondences$new_nms_chr) %>%
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
get_timezone <- function(country_1L_chr, ## Depends on maps package
                         method_1L_chr = "accurate"){
  ### WIP


  latitude_1L_dbl <- -37.8
  longitude_1L_dbl <- 144.9

  lutz::tz_lookup_coords(lat = latitude_1L_dbl, lon = longitude_1L_dbl, method = method_1L_chr)
}
make_country_standards <- function(as_list_1L_lgl = F,
                                   append_ls = NULL){
  X <- ready4use::Ready4useDyad()
  X@ds_tb <- get_country_standards() %>% tibble::as_tibble()
  X@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X@dictionary_r3,
                                                           var_nm_chr = names(X@ds_tb),
                                                           var_ctg_chr = c("A2", "A3" ,"N", "Country", "Official", "Common"),
                                                           var_desc_chr = c("Alpabetical country code (two letters)","Alpabetical country code (three letters)",
                                                                            "Numeric country code", "Country name", "Country name (official)", "Country name (common alternative)"),
                                                           var_type_chr = purrr::map_chr(X@ds_tb, ~class(.x)[1]) %>% unname())
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
  X <- ready4use::Ready4useDyad()
  X@ds_tb <- get_currency_standards() %>% tibble::as_tibble()
  X@dictionary_r3 <- ready4use::renew.ready4use_dictionary(X@dictionary_r3,
                                                           var_nm_chr = names(X@ds_tb),
                                                           var_ctg_chr = c("A3" ,"N", "Currency"),
                                                           var_desc_chr = c("Alpabetical currency code (three letters)",
                                                                            "Numeric currency code", "Currency name"),
                                                           var_type_chr = purrr::map_chr(X@ds_tb, ~ class(.x)[1]) %>% unname())
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
                     x_ready4show_correspondences = ready4show::ready4show_correspondences())

  default_xx <- default_ls
  if(what_1L_chr == c("arguments")){
    default_xx <- default_xx$args_ls
  }
  if(what_1L_chr == c("correspondences")){
    default_xx <- default_xx$x_ready4show_correspondences
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
make_standardised_dss <- function(label_1L_chr = "Country",
                                  lookup_Ready4useDyad = ready4use::Ready4useDyad(),
                                  seed_Ready4useDyad = ready4use::Ready4useDyad(),
                                  standards_Ready4useDyad = ready4use::Ready4useDyad(),
                                  type_chr = c("Input", "Output"),
                                  validation_ls = list(),
                                  what_chr = c("Correspondences","Lookup", "Seed", "Standards", "Validation"),
                                  x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                                  y_ready4show_correspondences = ready4show::ready4show_correspondences()){
  testit::assert("label_1L_chr needs to be a length one character vector", length(label_1L_chr)==1 && is.character(label_1L_chr))
  type_chr <- match.arg(type_chr, several.ok = T) %>% sort()
  what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
  standardised_dss_ls <- list(x_ready4show_correspondences, seed_Ready4useDyad, standards_Ready4useDyad, y_ready4show_correspondences, lookup_Ready4useDyad, validation_ls) %>% stats::setNames(make_ds_names(label_1L_chr))
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
transform_country <- function(country_1L_chr, # Belongs in vicinity?
                              from_1L_chr = "Name",
                              standards_df = data.frame(),
                              what_1L_chr = "Alpha_3"){
  if(identical(standards_df, data.frame())){
    utils::data("ISO_3166_1", package = "ISOcodes", envir = environment())
    standards_df <- ISO_3166_1
    rm(ISO_3166_1)
  }
  what_1L_chr <- match.arg(what_1L_chr, choices = setdiff(names(standards_df), from_1L_chr))
  country_1L_chr <-  ready4::get_from_lup_obj(standards_df,
                                              match_value_xx = country_1L_chr,
                                              match_var_nm_1L_chr = from_1L_chr,
                                              target_var_nm_1L_chr = what_1L_chr)
  return(country_1L_chr)
}
update_correspondences <- function(x_ready4show_correspondences = ready4show::ready4show_correspondences(),
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
    y_ready4show_correspondences <- purrr::map2_dfr(matched_ls, names(matched_ls),
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
    x_ready4show_correspondences <- dplyr::bind_rows(x_ready4show_correspondences,
                                                     y_ready4show_correspondences) %>%
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
      dplyr::filter(!old_nms_chr %in% x_ready4show_correspondences$old_nms_chr)
    x_ready4show_correspondences <- dplyr::bind_rows(x_ready4show_correspondences,
                                                     z_ready4show_correspondences) %>%
      dplyr::arrange(old_nms_chr)
  }
  if(force_standard_1L_lgl){
    x_ready4show_correspondences <- x_ready4show_correspondences %>%
      dplyr::filter(new_nms_chr %in% standards_df$Name)
  }
  return(x_ready4show_correspondences)
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
  x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(),
                                                                               old_nms_chr = old_nms_chr,
                                                                               new_nms_chr = new_nms_chr)
  return(x_ready4show_correspondences)
}
update_country_default_ls <- function(default_ls,
                                      force_standard_1L_lgl = F){

  default_ls$args_ls$case_when_true_ls <- list(capital = "name == 'Pristina' ~ 1")
  default_ls$args_ls$case_when_var_1L_chr <- default_ls$args_ls$case_when_false_1L_chr <- "capital"
  default_ls$CostlySeed_r4 <- add_default_country_seed(default_ls$CostlySeed_r4)
  default_ls$x_ready4show_correspondences <- make_country_correspondences("cities")
  default_ls$x_ready4show_correspondences <- update_correspondences(x_ready4show_correspondences = default_ls$x_ready4show_correspondences,
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
update_currency_correspondences <- function(x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                                            country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
                                            type_1L_chr = c("Both","Country","Currency")){
  type_1L_chr <- match.arg(type_1L_chr)
  if(identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())){
    x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences, old_nms_chr = c("Country","Currency"),
                                                                                 new_nms_chr = c("by_country_tb","by_currency_tb"))
  }
  if(type_1L_chr %in% x_ready4show_correspondences$old_nms_chr){
    element_1L_chr <- ready4::get_from_lup_obj(x_ready4show_correspondences, match_var_nm_1L_chr = "old_nms_chr",
                                               match_value_xx = type_1L_chr, target_var_nm_1L_chr = "new_nms_chr")
    country_var_nms_chr <- country_var_nms_chr[which(x_ready4show_correspondences$new_nms_chr == element_1L_chr)]
    x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences , filter_cdn_1L_chr = paste0("new_nms_chr == ", deparse(element_1L_chr)))
  }
  return(x_ready4show_correspondences)
}

update_currency_default_ls <- function(default_ls,
                                       force_standard_1L_lgl = F){
  default_ls$CostlySeed_r4 <- add_default_currency_seed(default_ls$CostlySeed_r4)
  return(default_ls)
}
update_module_slot <- function(x_r4, # Be careful about potentially making this a Method of Ready4Module - need to think through renew syntax
                               y_r3,
                               what_1L_chr,
                               new_val_xx = NULL){
  testit::assert("x_r4 must be a Ready4Module",
                 inherits(x, "Ready4Module"))
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
                                  x_ready4show_correspondences = ready4show::ready4show_correspondences(),
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
    x_ready4show_correspondences <- update_correspondences(x_ready4show_correspondences = x_ready4show_correspondences,
                                                           seed_df = seed_df,
                                                           reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                                                           force_standard_1L_lgl = force_standard_1L_lgl,
                                                           fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                                           max_distance_1L_dbl = max_distance_1L_dbl,
                                                           standards_df = standards_df,
                                                           standards_var_nms_chr = standards_var_nms_chr)
  }
  standardised_ds_df <- seed_df %>% dplyr::mutate(!!rlang::sym(reference_var_nm_1L_chr) := !!rlang::sym(reference_var_nm_1L_chr) %>%
                                                    purrr::map_chr(~ifelse(.x %in% x_ready4show_correspondences$old_nms_chr, # Replace when manufacture method is updated??
                                                                           ready4::get_from_lup_obj(x_ready4show_correspondences,
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
## Classes
library(ready4show)
CostlySource <- methods::setClass("CostlySource",
                                  contains = "Ready4Module",
                                  slots = c(Ready4useDyad_r4 = "Ready4useDyad",
                                            include_chr = "character",
                                            label_1L_chr = "character",
                                            match_1L_chr = "character"),
                                  prototype =  list(Ready4useDyad_r4 = ready4use::Ready4useDyad(),
                                                    include_chr = NA_character_,
                                                    label_1L_chr = "Standardised",
                                                    match_1L_chr = "A3"))
CostlySeed <- methods::setClass("CostlySeed",
                                contains = "CostlySource",
                                slots = c(args_ls = "list"),
                                prototype =  list(args_ls = list()))
CostlyStandards <- methods::setClass("CostlyStandards",
                                     contains = "CostlySource",
                                     slots = c(args_ls = "list"),
                                     prototype =  list(args_ls = list()))
CostlyCorrespondences <- methods::setClass("CostlyCorrespondences",
                                           contains = "Ready4Module",
                                           slots = c(CostlySeed_r4 = "CostlySeed",
                                                     CostlyStandards_r4 = "CostlyStandards",
                                                     correspondences_r3 = "ready4show_correspondences",
                                                     args_ls = "list",
                                                     force_standard_1L_lgl = "logical",
                                                     fuzzy_logic_1L_chr = "character",
                                                     max_distance_1L_dbl = "numeric",
                                                     results_ls = "list"),
                                           prototype =  list(CostlySeed_r4 = CostlySeed(),
                                                             CostlyStandards_r4 = CostlyStandards(),
                                                             correspondences_r3 = ready4show::ready4show_correspondences(),
                                                             args_ls = list(),
                                                             force_standard_1L_lgl = F,
                                                             fuzzy_logic_1L_chr = character(0),
                                                             max_distance_1L_dbl = Inf,
                                                             results_ls = list()))
CostlyCountries <- methods::setClass("CostlyCountries",
                                     contains = "CostlyCorrespondences",
                                     prototype = list(CostlySeed_r4 = add_source_label(CostlySeed(),"Country"),
                                                      CostlyStandards_r4 = make_country_standards(),
                                                      fuzzy_logic_1L_chr = "jw"))
CostlyCurrencies <- methods::setClass("CostlyCurrencies",
                                      contains = "CostlyCorrespondences",
                                      prototype = list(CostlySeed_r4 = add_source_label(CostlySeed(),"Currency"),
                                                       CostlyStandards_r4 = make_currency_standards()))

## Methods
methods::setMethod("manufacture", "CostlyCorrespondences", function(x,
                                                                    sort_1L_lgl = T,
                                                                    type_chr = make_ds_names(type_chr = character(0)),
                                                                    what_1L_chr = c("dataset",
                                                                                    "defaults", "shorthand"),
                                                                    ...){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr %in% c("dataset",
                        "defaults", "shorthand")){
    if(what_1L_chr == "dataset"){
      if(type_chr[1] == "Correspondences"){
        object_xx <- update_with_standards(x@CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
                                           x_ready4show_correspondences = x@correspondences_r3,
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
        standard_matches_chr <- c(x@CostlyStandards_r4@label_1L_chr,x@CostlyStandards_r4@match_1L_chr)
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
})
methods::setMethod("manufacture", "CostlyCountries", function(x,
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
})
methods::setMethod("manufacture", "CostlyCurrencies", function(x,
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
})
methods::setMethod("procure", "CostlySource", function(x,
                                                       matches_chr = character(0),
                                                       what_1L_chr = c("names","concepts")){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr %in% c("names","concepts")){
    if(identical(matches_chr, character(0))){
      if(what_1L_chr == "names"){
        matches_chr <- x@include_chr
      }else{
        matches_chr <- get_corresponding_var(x@Ready4useDyad_r4,
                                             matches_chr = x@include_chr,
                                             what_1L_chr = "names")
      }
    }
    object_xx <- get_corresponding_var(x@Ready4useDyad_r4,
                                       matches_chr = matches_chr,
                                       what_1L_chr = what_1L_chr)
  }
  return(object_xx)
})
methods::setMethod("ratify", "CostlyCorrespondences", function(x,
                                                               new_val_xx = "identity",
                                                               sort_1L_lgl = T,
                                                               type_1L_chr = make_ds_names(type_chr = character(0))[1:2],
                                                               what_chr = make_ds_names(type_chr = character(0))){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "Correspondences"){
    if(!identical(new_val_xx, "identity") && !is.null(new_val_xx)){
      x_ready4show_correspondences <- new_val_xx
    }else{
      x_ready4show_correspondences <- x@correspondences_r3
    }
    x <- renew(x, new_val_xx = new_val_xx, type_1L_chr = "final", what_1L_chr = "correspondences")

  }else{
    x_ready4show_correspondences <- x@correspondences_r3
  }
  # match_1L_chr <- x@CostlySeed_r4@label_1L_chr ## @include_chr[1]
  ds_df <- manufacture(x, type_chr = type_1L_chr, what_1L_chr = "dataset")
  validation_ls <- make_validation_ls(x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>% dplyr::pull(procure(x@CostlyStandards_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, what_1L_chr = "names")),
                                      ds_df = ds_df,
                                      var_nm_1L_chr = procure(x@CostlySeed_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, #match_1L_chr
                                                              what_1L_chr = "names"),
                                      sort_1L_lgl = sort_1L_lgl)
  x@results_ls <- make_standardised_dss(x@CostlySeed_r4@label_1L_chr,
                                        lookup_Ready4useDyad = ready4use::Ready4useDyad(ds_tb = tibble::as_data_frame(ds_df), dictionary_r3 = x@CostlySeed_r4@Ready4useDyad_r4@dictionary_r3),
                                        seed_Ready4useDyad = x@CostlySeed_r4@Ready4useDyad_r4,
                                        standards_Ready4useDyad = x@CostlyStandards_r4@Ready4useDyad_r4,
                                        validation_ls = validation_ls,
                                        what_chr = what_chr,
                                        x_ready4show_correspondences = x_ready4show_correspondences,
                                        y_ready4show_correspondences = x@correspondences_r3)
  return(x)
})
methods::setMethod("renew", "CostlyCorrespondences",
                   function (x, new_val_xx = NULL, type_1L_chr = "slot", what_1L_chr = "correspondences", ...) {
                     if(type_1L_chr == "default"){
                       default_ls <- manufacture(x, what_1L_chr = "defaults")
                       x@args_ls <- default_ls$args_ls
                       x@correspondences_r3 <- default_ls$x_ready4show_correspondences
                       x@fuzzy_logic_1L_chr <- default_ls$fuzzy_logic_1L_chr
                       x@CostlySeed_r4 <- default_ls$CostlySeed_r4
                     }
                     if(type_1L_chr %in% c("exploratory", "final","slot")){
                       y <- manufacture(x, what_1L_chr = "shorthand")
                       what_1L_chr <- match.arg(what_1L_chr, choices = y$old_nms_chr)
                       if(what_1L_chr == "correspondences"){
                         if(!is.null(new_val_xx) && !identical(new_val_xx, "identity")){
                           x@correspondences_r3 <- new_val_xx
                         }
                         if(is.null(new_val_xx) | !identical(new_val_xx, "identity")){
                           x@correspondences_r3 <- update_correspondences(x_ready4show_correspondences = x@correspondences_r3,
                                                                          standards_df = x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
                                                                          standards_var_nms_chr = procure(x@CostlyStandards_r4, matches_chr = x@CostlyStandards_r4@include_chr, what_1L_chr = "names"),
                                                                          seed_df = x@CostlySeed_r4@Ready4useDyad_r4@ds_tb %>% as.data.frame(),
                                                                          reference_var_nm_1L_chr = procure(x@CostlySeed_r4, matches_chr = x@CostlySeed_r4@label_1L_chr,
                                                                                                            what_1L_chr = "names"),
                                                                          force_standard_1L_lgl = x@force_standard_1L_lgl,
                                                                          fuzzy_logic_1L_chr = x@fuzzy_logic_1L_chr,
                                                                          max_distance_1L_dbl = x@max_distance_1L_dbl)
                         }
                         if(type_1L_chr == "final"){
                           x@fuzzy_logic_1L_chr <- character(0)
                         }
                       }else{
                         if(type_1L_chr == "slot"){
                           x <- update_module_slot(x,
                                                   y_r3 = y,
                                                   what_1L_chr = what_1L_chr,
                                                   new_val_xx = new_val_xx)
                         }
                       }
                     }
                     return(x)
                   })

