## Retrieve from repo
## Apply to grouped ds
## Create repos
## Validate
get_standardised_dss <- function(X_Ready4useRepos,
                                 type_1L_chr = "Country",
                                 what_chr = make_ds_names(file_nm_1L_lgl = F)){
  what_chr <- match.arg(what_chr, several.ok = T)
  standardised_dss_ls <- ingest(X_Ready4useRepos, fls_to_ingest_chr = make_ds_names(type_1L_chr, what_chr = what_chr), metadata_1L_lgl = F)
  if(length(standardised_dss_ls) == 1){
    standardised_dss_xx <- standardised_dss_ls %>% purrr::pluck(1)
    }else{
      standardised_dss_xx <- standardised_dss_ls
      }
  return(standardised_dss_xx)
}
make_ds_names <- function(type_1L_chr = "Country",
                          file_nm_1L_lgl = T,
                          what_chr = c("Comparisons", "Correspondences", "Seed", "Standard", "Validated")){
  options_chr <- c("Comparisons", "Correspondences", "Seed", "Standard", "Validated")
  what_chr <- match.arg(what_chr, several.ok = T) %>% sort()
  if(file_nm_1L_lgl){
    correspondences_chr <- what_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(),
                                                                                                 old_nms_chr = options_chr,
                                                                                                 new_nms_chr = paste0(type_1L_chr,"_",options_chr)),
                                                                                 match_var_nm_1L_chr = "old_nms_chr",
                                                                                 match_value_xx = .x,
                                                                                 target_var_nm_1L_chr = "new_nms_chr"))
  }else{
    correspondences_chr <- what_chr
  }
  return(correspondences_chr)
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
                         tf_false_val_1L_lgl = T,
                         type_1L_chr = "Country",
                         url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                         #preferred_1L_chr = character(0),
                         what_1L_chr = "Symbol",
                         x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                         y_ready4show_correspondences = ready4show::ready4show_correspondences(),
                         X_Ready4useRepos = ready4use::Ready4useRepos()){
  if(format_1L_chr != "Alpha_3"){
    country_1L_chr <- transform_country(country_1L_chr = country_1L_chr, from_1L_chr = format_1L_chr)
  }
  currency_1L_chr <- country_1L_chr %>%
    countrycode::countrycode("iso3c","iso4217c")
  if(!is.na(what_1L_chr)){ #stringr::str_detect(what_1L_chr, stringr::regex("code", ignore_case = T))
    if(identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())){
      x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences, old_nms_chr = c("Name","Symbol","Code","Unit", "Number"),
                                                                                   new_nms_chr = c("Currency[1][2]","Symbol[D] orAbbrev.[3]","ISO code[2]","Fractionalunit","Numberto basic"))
    }
    if(is.null(currency_tb)){
      currency_tb <- make_currencies_df(case_when_false_1L_chr = case_when_false_1L_chr,
                                        case_when_true_1L_chr = case_when_true_1L_chr,
                                        case_when_true_ls = case_when_true_ls,
                                        case_when_var_1L_chr = case_when_var_1L_chr,
                                        country_var_nms_chr = country_var_nms_chr,
                                        filter_cdn_1L_chr = filter_cdn_1L_chr,
                                        force_standard_1L_lgl = force_standard_1L_lgl,
                                        fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                        indcs_int =indcs_int,
                                        max_distance_1L_dbl = max_distance_1L_dbl,
                                        seed_df = seed_df,
                                        tf_false_val_1L_lgl = tf_false_val_1L_lgl,
                                        type_1L_chr = type_1L_chr,
                                        url_1L_chr = url_1L_chr,
                                        x_ready4show_correspondences = y_ready4show_correspondences,
                                        X_Ready4useRepos = X_Ready4useRepos)
    }
    currency_1L_chr <- currency_tb[which(currency_1L_chr ==(currency_tb[,which(names(currency_tb) == match_var_nm_1L_chr)] %>%
                                                              unlist() %>% as.vector()))[1],
                                   which(names(currency_tb) == ready4::get_from_lup_obj(x_ready4show_correspondences,
                                                                                        match_var_nm_1L_chr = "old_nms_chr",
                                                                                        match_value_xx = Hmisc::capitalize(what_1L_chr),
                                                                                        target_var_nm_1L_chr = "new_nms_chr"))][[1,1]]
  }
  return(currency_1L_chr)
}
# From: https://stackoverflow.com/questions/60719592/r-built-in-list-of-currency-symbols
get_currency_tbls <- function(type_1L_chr = c("Both","Country","Currency"),#"by_country_tb",
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
get_timezone <- function(country_1L_chr, ## Depends on maps package
                         method_1L_chr = "accurate"){


  latitude_1L_dbl <- -37.8
  longitude_1L_dbl <- 144.9

  lutz::tz_lookup_coords(lat = latitude_1L_dbl, lon = longitude_1L_dbl, method = method_1L_chr)
}
make_cities_dss <- function(case_when_false_1L_chr = NA_character_,
                           case_when_true_1L_chr = NA_character_,
                           case_when_true_ls = NULL,
                           case_when_var_1L_chr = NA_character_,
                           country_var_nm_1L_chr = "country.etc",
                           filter_cdn_1L_chr = NA_character_,
                           force_standard_1L_lgl = F,
                           fuzzy_logic_1L_chr = character(0),
                           max_distance_1L_dbl = Inf,
                           seed_df = NULL,
                           sort_1L_lgl = F,
                           standards_df = data.frame(),
                           standards_var_nms_chr = c("Name", "Official_name", "Common_name", "Alpha_3", "Alpha_2"),
                           tf_false_val_1L_lgl = T,
                           what_chr = make_ds_names(file_nm_1L_lgl = F),
                           type_1L_chr = "Cities",
                           x_ready4show_correspondences = ready4show::ready4show_correspondences()){
  if(is.null(seed_df)){
    utils::data("world.cities", package = "maps", envir = environment())
    seed_df <- world.cities
    rm(world.cities)
    x_ready4show_correspondences <- ready4show::make_pt_ready4show_correspondences(old_nms_chr = c("Azores", "Canary Islands", "Easter Island",
                                                                                                     "East Timor", "Ivory Coast", "Kosovo", "Madeira",
                                                                                                     "Netherlands Antilles", "Sicily", "Vatican City"),
                                                                                     new_nms_chr = c("Portugal","Spain","Chile",
                                                                                                     "Timor-Leste", "Côte d'Ivoire", "Kosovo", "Portugal",
                                                                                                     "Bonaire, Sint Eustatius and Saba", "Italy", "Holy See (Vatican City State)")) %>%
      ready4show::ready4show_correspondences()
    if(is.null(case_when_true_ls)){
      case_when_true_ls <- list(capital = "name == 'Pristina' ~ 1")
      case_when_var_1L_chr <- case_when_false_1L_chr <- "capital"
    }
    if(identical(standards_df, data.frame())){
      utils::data("ISO_3166_1", package = "ISOcodes", envir = environment())
      standards_df <- ISO_3166_1
      rm(ISO_3166_1)
    }
    # fuzzy_logic_1L_chr <- "jw"
    if(identical(fuzzy_logic_1L_chr, character(0))){
      x_ready4show_correspondences <- update_country_correspondences(x_ready4show_correspondences = x_ready4show_correspondences,
                                                                     countries_df = seed_df,
                                                                     country_var_nm_1L_chr = country_var_nm_1L_chr,
                                                                     force_standard_1L_lgl = force_standard_1L_lgl,
                                                                     fuzzy_logic_1L_chr = "jw",
                                                                     max_distance_1L_dbl = max_distance_1L_dbl,
                                                                     standards_df = standards_df,
                                                                     standards_var_nms_chr = standards_var_nms_chr)
    }
    fuzzy_logic_1L_chr <- character(0)
  }
  cities_df <- update_countries_df(seed_df,
                                   x_ready4show_correspondences = x_ready4show_correspondences,
                                   case_when_false_1L_chr = case_when_false_1L_chr,
                                   case_when_true_1L_chr = case_when_true_1L_chr,
                                   case_when_true_ls = case_when_true_ls,
                                   case_when_var_1L_chr = case_when_var_1L_chr,
                                   country_var_nm_1L_chr = country_var_nm_1L_chr,
                                   filter_cdn_1L_chr = filter_cdn_1L_chr,
                                   force_standard_1L_lgl = force_standard_1L_lgl,
                                   fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                   max_distance_1L_dbl = max_distance_1L_dbl,
                                   standards_df = standards_df,
                                   standards_var_nms_chr = standards_var_nms_chr,
                                   tf_false_val_1L_lgl = tf_false_val_1L_lgl)
  validation_ls <- make_validation_ls(standards_df %>% dplyr::pull(standards_var_nms_chr[1]),
                                      ds_df = cities_df,
                                      var_nm_1L_chr = country_var_nm_1L_chr,
                                      sort_1L_lgl = sort_1L_lgl)
  cities_dss_ls <- make_standardised_dss("Cities",
                                         comparisons_df = cities_df,
                                         seed_df = seed_df,
                                         standards_df = standards_df,
                                         validation_ls = validation_ls,
                                         x_ready4show_correspondences = x_ready4show_correspondences,
                                         what_chr = what_chr)
  return(cities_dss_ls)
}
make_currencies_dss <- function(){

}
make_standardised_dss <- function(type_1L_chr = "Country",
                                  comparisons_df = data.frame(),
                                  seed_df = data.frame(),
                                  standards_df = data.frame(),
                                  validation_ls = list(),
                                  x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                                  what_chr = make_ds_names(file_nm_1L_lgl = F)){
  what_chr <- match.arg(what_chr, several.ok = T)
  standardised_dss_ls <- list(comparisons_df, x_ready4show_correspondences, seed_df, standards_df, validation_ls) %>% stats::setNames(make_ds_names(type_1L_chr))
  standardised_dss_ls <- standardised_dss_ls[make_ds_names(type_1L_chr, what_chr = what_chr)]
  return(standardised_dss_ls)
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
  #make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Absent", what_1L_chr = "Values")
  #make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Values")
  #make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Values")
  #make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Cases")
  #make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Cases")
  # make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Absent", what_1L_chr = "Values")
  # make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Values")
  # make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Values")
  # make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Cases")
  # make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Cases")
}
make_currencies_df <- function(case_when_false_1L_chr = NA_character_,
                               case_when_true_1L_chr = NA_character_,
                               case_when_true_ls = NULL,
                               case_when_var_1L_chr = NA_character_,
                               #country_var_nm_1L_chr = "country.etc",
                               country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
                               filter_cdn_1L_chr = NA_character_,
                               force_standard_1L_lgl = F,
                               fuzzy_logic_1L_chr = character(0),#"jw",
                               indcs_int = c(2:3),
                               max_distance_1L_dbl = Inf,
                               seed_df = NULL,
                               standards_df = data.frame(),
                               standards_var_nms_chr = c("Name", "Official_name", "Common_name", "Alpha_3", "Alpha_2"),
                               tf_false_val_1L_lgl = T,
                               type_1L_chr = c("Country","Currency"),
                               url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                               x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                               X_Ready4useRepos = ready4use::Ready4useRepos()
                               ){
  type_1L_chr <- match.arg(type_1L_chr)
  if(is.null(seed_df)){
    seed_df <- get_currency_tbls(type_1L_chr = type_1L_chr,
                                 country_var_nms_chr = country_var_nms_chr,
                                 indcs_int = indcs_int,
                                 url_1L_chr = url_1L_chr,
                                 x_ready4show_correspondences = x_ready4show_correspondences,
                                 X_Ready4useRepos = X_Ready4useRepos)
    if(identical(x_ready4show_correspondences, ready4show::ready4show_correspondences()) && identical(X_Ready4useRepos, ready4use::Ready4useRepos()) && url_1L_chr == "https://en.wikipedia.org/wiki/List_of_circulating_currencies"){
      correspondences_chr <- c(Abkhazia = "Abkhazia", `United Kingdom` = "Akrotiri and Dhekelia", Artsakh = "Artsakh", `Saint Helena, Ascension and Tristan da Cunha` = "Ascension Island", Congo = "Congo, Republic of the",
                               Guernsey = "Bailiwick of Guernsey", `Timor-Leste` = "East Timor", `Korea, Democratic People's Republic of` = "Korea, North", Kosovo = "Kosovo", `Northern Cyprus`= "Northern Cyprus", `Bonaire, Sint Eustatius and Saba` = "Saba",
                               `Western Sahara` = "Sahrawi Republic", `Somaliland` = "Somaliland", `South Ossetia` = "South Ossetia", Transnistria = "Transnistria", `Holy See (Vatican City State)`= "Vatican City")
      x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences,
                                                                                   old_nms_chr = unname(correspondences_chr),
                                                                                   new_nms_chr = names(correspondences_chr))
    }
  }
  if(!identical(fuzzy_logic_1L_chr, character(0))){ # ie, update skipped if not supplying logic
    if(identical(standards_df, data.frame())){
      utils::data("ISO_3166_1", package = "maps", envir = environment())
      standards_df <- ISO_3166_1
      rm(ISO_3166_1)
    }
    x_ready4show_correspondences <- update_country_correspondences(x_ready4show_correspondences = x_ready4show_correspondences,
                                                                   countries_df = seed_df,
                                                                   country_var_nm_1L_chr = country_var_nms_chr[which(c("Country","Currency")==type_1L_chr)],
                                                                   force_standard_1L_lgl = force_standard_1L_lgl,
                                                                   fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                                                   max_distance_1L_dbl = max_distance_1L_dbl,
                                                                   standards_df = standards_df,
                                                                   standards_var_nms_chr = standards_var_nms_chr)
  }
  currencies_df <- update_countries_df(seed_df,
                                       x_ready4show_correspondences = x_ready4show_correspondences,
                                       case_when_false_1L_chr = case_when_false_1L_chr,
                                       case_when_true_1L_chr = case_when_true_1L_chr,
                                       case_when_true_ls = case_when_true_ls,
                                       case_when_var_1L_chr = case_when_var_1L_chr,
                                       country_var_nm_1L_chr = country_var_nms_chr[which(c("Country","Currency")==type_1L_chr)],
                                       filter_cdn_1L_chr = filter_cdn_1L_chr,
                                       force_standard_1L_lgl = force_standard_1L_lgl,
                                       fuzzy_logic_1L_chr = character(0), #fuzzy_logic_1L_chr,
                                       max_distance_1L_dbl = max_distance_1L_dbl,
                                       tf_false_val_1L_lgl = tf_false_val_1L_lgl)
  return(currencies_df)
}
make_currency_ls <- function(country_1L_chr,
                             currency_tb = NULL,
                             format_1L_chr = "Name",
                             match_var_nm_1L_chr = "ISO code[2]",
                             what_1L_chr = "Symbol[D] orAbbrev.[3]"){
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
make_validation_output <- function(allowed_xx,
                                   ds_df,
                                   var_nm_1L_chr,
                                   sort_1L_lgl = F,
                                   type_1L_chr = "Valid",
                                   what_1L_chr = "Cases"){
  if(is.data.frame(ds_df) & !tibble::is_tibble(ds_df)){
    ds_tb <- tibble::as_tibble(ds_df)
    message("ds_df has been converted to a tibble")
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
transform_country <- function(country_1L_chr, # Belongs in vicinity
                              from_1L_chr = "Name",
                              what_1L_chr = "Alpha_3"){
  if(what_1L_chr %in% setdiff(names(ISOcodes::ISO_3166_1), from_1L_chr)){
    country_1L_chr <-  ready4::get_from_lup_obj(ISOcodes::ISO_3166_1,
                                                match_value_xx = country_1L_chr,
                                                match_var_nm_1L_chr = from_1L_chr,
                                                target_var_nm_1L_chr = what_1L_chr)
  }
  return(country_1L_chr)
}
update_countries_df <- function(countries_df,
                                standards_df,# = data.frame(),
                                standards_var_nms_chr,# = c("Name", "Official_name", "Common_name", "Alpha_3", "Alpha_2"),
                                x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                                case_when_false_1L_chr = NA_character_,
                                case_when_true_1L_chr = NA_character_,
                                case_when_true_ls = NULL,
                                case_when_var_1L_chr = NA_character_,
                                country_var_nm_1L_chr = "country.etc",
                                filter_cdn_1L_chr = NA_character_,
                                force_standard_1L_lgl = F,
                                fuzzy_logic_1L_chr = character(0),
                                max_distance_1L_dbl = Inf,
                                tf_false_val_1L_lgl = T){
  if(!identical(fuzzy_logic_1L_chr, character(0))){ # Strictly not necessary, but reminder how to avoid modifying correspondences
    x_ready4show_correspondences <- update_country_correspondences(x_ready4show_correspondences = x_ready4show_correspondences,
                                                                   countries_df = countries_df,
                                                                   country_var_nm_1L_chr = country_var_nm_1L_chr,
                                                                   force_standard_1L_lgl = force_standard_1L_lgl,
                                                                   fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                                                   max_distance_1L_dbl = max_distance_1L_dbl,
                                                                   standards_df = standards_df,
                                                                   standards_var_nms_chr = standards_var_nms_chr)
  }
  countries_df <- countries_df %>% dplyr::mutate(!!rlang::sym(country_var_nm_1L_chr) := !!rlang::sym(country_var_nm_1L_chr) %>%
                                                   purrr::map_chr(~ifelse(.x %in% x_ready4show_correspondences$old_nms_chr, # Replace when manufacture method is fixed.
                                                                          ready4::get_from_lup_obj(x_ready4show_correspondences,
                                                                                                   match_value_xx = .x,
                                                                                                   match_var_nm_1L_chr = "old_nms_chr",
                                                                                                   target_var_nm_1L_chr = "new_nms_chr"),
                                                                          .x)))
  if(force_standard_1L_lgl){
    countries_df <- countries_df %>%
      dplyr::filter(!!rlang::sym(country_var_nm_1L_chr) %in% ISOcodes::ISO_3166_1$Name)
  }
  if(!is.null(case_when_true_ls)){
    countries_df <- countries_df %>% ready4::update_tb_r3(case_when_true_ls = case_when_true_ls, case_when_var_1L_chr = case_when_var_1L_chr, case_when_false_1L_chr = case_when_var_1L_chr, filter_cdn_1L_chr = filter_cdn_1L_chr, tf_false_val_1L_lgl = tf_false_val_1L_lgl)
  }
  return(countries_df)
}
update_country_correspondences <- function(x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                                           standards_df,# = data.frame(),
                                           standards_var_nms_chr,# = c("Name", "Official_name", "Common_name", "Alpha_3", "Alpha_2")
                                           countries_df = NULL,
                                           country_var_nm_1L_chr = "country.etc",
                                           force_standard_1L_lgl = T,
                                           fuzzy_logic_1L_chr = "jw",
                                           max_distance_1L_dbl = Inf
                                           ){
  # if(identical(standards_df, data.frame())){
  #   utils::data("ISO_3166_1", package = "ISOcodes", envir = environment())
  #   standards_df <- ISO_3166_1
  #   rm(ISO_3166_1)
  # }
  if(!identical(fuzzy_logic_1L_chr, character(0))){
    countries_chr <- countries_df %>%
      dplyr::pull(country_var_nm_1L_chr) %>%
      unique() %>% sort()
    matched_ls <- countries_chr %>%
      purrr::map(~ {
        country_1L_chr <- .x
        standards_var_nms_chr %>%
          purrr::map_lgl(~ country_1L_chr %in% (standards_df %>% dplyr::pull(.x)))
      }
                 ) %>%
      stats::setNames(countries_chr)
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
    indcs_int <- indcs_int[which(x_ready4show_correspondences$new_nms_chr == element_1L_chr)]
    country_var_nms_chr <- country_var_nms_chr[which(x_ready4show_correspondences$new_nms_chr == element_1L_chr)]
    x_ready4show_correspondences <- ready4show::renew.ready4show_correspondences(x_ready4show_correspondences , filter_cdn_1L_chr = "new_nms_chr == element_1L_chr", element_1L_chr = element_1L_chr)
  }
  return(x_ready4show_correspondences)
}
# update_currency_tbls <- function(currency_tbls_ls,
#                                  country_var_nms_chr = c("State or territory[1]","Countries/ territories"),
#                                  type_1L_chr = c("Both","Country","Currency"),
#                                  x_ready4show_correspondences = ready4show::ready4show_correspondences()){
#
#   #currency_tbls_ls$by_country_tb
#   # currency_tbls_ls <- currency_tbls_ls %>% purrr::map2(country_var_nms_chr,
#   #                                                      ~{
#   #
#   #                                                      })
#
# }

#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Absent", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Cases")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Cases")
# make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Absent", what_1L_chr = "Values")
# make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Values")
# make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Values")
# make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Cases")
# make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_df = currencies_df, var_nm_1L_chr = country_var_nms_chr[1], sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Cases")

# A <- vicinity::VicinityProfile()
# A@country_chr <- "Australia"
# ready4::get_from_lup_obj(ISOcodes::ISO_3166_1,
#                          match_value_xx = country_1L_chr,
#                          match_var_nm_1L_chr = "Name",
#                          target_var_nm_1L_chr = "Alpha_3") %>%
#   countrycode::countrycode("iso3c","iso4217c")


