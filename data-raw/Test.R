# From: https://stackoverflow.com/questions/60719592/r-built-in-list-of-currency-symbols
get_currency_tbl <- function(what_1L_chr = "by_country_tb",
                             indcs_int = c(2:3),
                             names_chr = c("by_country_tb","by_currency_tb"),
                             url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies"){
  currency_tbls_ls <- url_1L_chr %>% rvest::read_html() %>% rvest::html_table()
  currency_tbls_ls <- currency_tbls_ls[indcs_int] %>%
    stats::setNames(names_chr)
  if(what_1L_chr %in% names_chr){
    currency_xx <- currency_tbls_ls %>% purrr::pluck(what_1L_chr)
  }else{
    currency_xx <- currency_tbls_ls
  }
  return(currency_xx)
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
get_currency <- function(country_1L_chr,
                         currency_tb = NULL,
                         format_1L_chr = "Name",
                         indcs_int = c(2:3),
                         names_chr = c("by_country_tb","by_currency_tb"),
                         url_1L_chr = "https://en.wikipedia.org/wiki/List_of_circulating_currencies",
                         match_var_nm_1L_chr = "ISO code[2]",
                         type_1L_chr = "by_country_tb",
                         what_1L_chr = "Symbol[D] orAbbrev.[3]"){
  if(format_1L_chr != "Alpha_3"){
    country_1L_chr <- transform_country(country_1L_chr = country_1L_chr,
                                        from_1L_chr = format_1L_chr)
  }
  currency_1L_chr <- country_1L_chr %>%
    countrycode::countrycode("iso3c","iso4217c")
  if(!is.na(what_1L_chr)){ #stringr::str_detect(what_1L_chr, stringr::regex("code", ignore_case = T))
    if(is.null(currency_tb)){
      currency_tb <- get_currency_tbl(what_1L_chr = type_1L_chr,
                                      indcs_int = indcs_int,
                                      names_chr = names_chr,
                                      url_1L_chr = url_1L_chr)
    }
   currency_1L_chr <- currency_tb[which(currency_1L_chr ==(currency_tb[,which(names(currency_tb) == match_var_nm_1L_chr)] %>%
                                                             unlist() %>% as.vector()))[1],
                                  which(names(currency_tb) == what_1L_chr)][[1,1]]

  }
  return(currency_1L_chr)
}
# transform_country("Australia")
make_country_correspondences <- function(x_ready4show_correspondences = NULL,
                                         countries_df = NULL,
                                         country_var_nm_1L_chr = "country.etc",
                                         force_ISO_1L_lgl = T,
                                         fuzzy_logic_1L_chr = "jw",
                                         max_distance_1L_dbl = Inf
                                         ){
  if(is.null(countries_df)){
    utils::data("world.cities", package = "maps", envir = environment())
    countries_df <- world.cities
    rm(world.cities)
  }
  if(is.null(x_ready4show_correspondences)){
    x_ready4show_correspondences <- ready4show::make_pt_ready4show_correspondences(old_nms_chr = c("Azores", "Canary Islands", "Easter Island",
                                                                                                   "East Timor", "Ivory Coast", "Kosovo", "Madeira",
                                                                                                   "Netherlands Antilles", "Sicily", "Vatican City"),
                                                                                   new_nms_chr = c("Portugal","Spain","Chile",
                                                                                                   "Timor-Leste", "Côte d'Ivoire", "Kosovo", "Portugal",
                                                                                                   "Bonaire, Sint Eustatius and Saba", "Italy", "Holy See (Vatican City State)")) %>%
      ready4show::ready4show_correspondences()
  }else{
    x_ready4show_correspondences <- ready4show::ready4show_correspondences()
  }
  if(!identical(fuzzy_logic_1L_chr, character(0))){
    countries_chr <- countries_df %>%
      dplyr::pull(country_var_nm_1L_chr) %>%
      unique() %>% sort()
    matched_ls <- countries_chr %>%
      purrr::map(~ c(.x %in% ISOcodes::ISO_3166_1$Name,
                     .x %in% ISOcodes::ISO_3166_1$Official_name,
                     .x %in% ISOcodes::ISO_3166_1$Common_name,
                     .x %in% ISOcodes::ISO_3166_1$Alpha_3,
                     .x %in% ISOcodes::ISO_3166_1$Alpha_2)) %>%
      stats::setNames(countries_chr)
    y_ready4show_correspondences <- purrr::map2_dfr(matched_ls, names(matched_ls),
                                    ~ {

                                      replace_1L_chr <- c(NA_character_, "Official_name", "Common_name", "Alpha_3", "Alpha_2")[which(.x)[1]]
                                      if(identical(replace_1L_chr, character(0)))
                                        replace_1L_chr <- NA_character_
                                      with_1L_chr <- ifelse(any(.x[2:5]),"Name",NA_character_)

                                      if(is.na(replace_1L_chr) | is.na(with_1L_chr)){
                                        ready4show::make_pt_ready4show_correspondences(#old_nms_chr = NA_character_, new_nms_chr = NA_character_
                                                                                       )
                                      }else{
                                        ready4show::make_pt_ready4show_correspondences(old_nms_chr = .y,
                                                                                       new_nms_chr = ready4::get_from_lup_obj(ISOcodes::ISO_3166_1,
                                                                                                                              match_value_xx = .y,
                                                                                                                              match_var_nm_1L_chr = replace_1L_chr,
                                                                                                                              target_var_nm_1L_chr = with_1L_chr))
                                      }
                                    }) %>% ready4show::ready4show_correspondences()

    x_ready4show_correspondences <- dplyr::bind_rows(x_ready4show_correspondences,
                                                     y_ready4show_correspondences) %>%
      dplyr::arrange(old_nms_chr)
    matched_lgl <- purrr::map_lgl(matched_ls, ~ any(.x))
    matches_chr <- names(matched_lgl)[matched_lgl]#(countries_df %>% dplyr::pull(country_var_nm_1L_chr) %>% unique())[matched_lgl] %>% sort()
    unmatched_tb <- ISOcodes::ISO_3166_1 %>% dplyr::filter(!Name %in% matches_chr & !Official_name %in% matches_chr & !Common_name %in% matches_chr & !Alpha_3 %in% matches_chr & !Alpha_2 %in% matches_chr)
    unmatched_chr <- setdiff(names(matched_lgl),matches_chr) #(countries_df %>% dplyr::pull(country_var_nm_1L_chr) %>% unique())[!matched_lgl] %>% sort()
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
  # no_cities_chr <- setdiff(unmatched_tb$Name, x_ready4show_correspondences$new_nms_chr %>% unique())
  if(force_ISO_1L_lgl){
    x_ready4show_correspondences <- x_ready4show_correspondences %>%
      dplyr::filter(new_nms_chr %in% ISOcodes::ISO_3166_1$Name)
  }
  return(x_ready4show_correspondences)
}
update_cities_df <- function(x_ready4show_correspondences = NULL,
                             case_when_false_1L_chr = NA_character_,
                             case_when_true_1L_chr = NA_character_,
                             case_when_true_ls = NULL,
                             case_when_var_1L_chr = NA_character_,
                             cities_df = NULL,
                             country_var_nm_1L_chr = "country.etc",
                             filter_cdn_1L_chr = NA_character_,
                             force_ISO_1L_lgl = F,
                             fuzzy_logic_1L_chr = "jw",
                             max_distance_1L_dbl = Inf,
                             tf_false_val_1L_lgl = T){
  if(is.null(cities_df)){
    utils::data("world.cities", package = "maps", envir = environment())
    cities_df <- world.cities
    rm(world.cities)
    if(is.null(case_when_true_ls)){
      case_when_true_ls <- list(capital = "name == 'Pristina' ~ 1")
      case_when_var_1L_chr <- case_when_false_1L_chr <- "capital"
    }
  }
  x_ready4show_correspondences <- make_country_correspondences(x_ready4show_correspondences = x_ready4show_correspondences,
                                                               countries_df = cities_df,
                                                               country_var_nm_1L_chr = country_var_nm_1L_chr,
                                                               force_ISO_1L_lgl = force_ISO_1L_lgl,
                                                               fuzzy_logic_1L_chr = fuzzy_logic_1L_chr,
                                                               max_distance_1L_dbl = max_distance_1L_dbl)
  cities_df <- cities_df %>% dplyr::mutate(!!rlang::sym(country_var_nm_1L_chr) := !!rlang::sym(country_var_nm_1L_chr) %>%
                                             purrr::map_chr(~ifelse(.x %in% x_ready4show_correspondences$old_nms_chr, # Replace when manufacture method is fixed.
                                                                    ready4::get_from_lup_obj(x_ready4show_correspondences,
                                                                                             match_value_xx = .x,
                                                                                             match_var_nm_1L_chr = "old_nms_chr",
                                                                                             target_var_nm_1L_chr = "new_nms_chr"),
                                                                    .x)))
  if(force_ISO_1L_lgl){
    cities_df <- cities_df %>%
      dplyr::filter(!!rlang::sym(country_var_nm_1L_chr) %in% ISOcodes::ISO_3166_1$Name)
  }
  if(!is.null(case_when_true_ls)){
    cities_df <- cities_df %>% update_tb_r3(case_when_true_ls = case_when_true_ls, case_when_var_1L_chr = case_when_var_1L_chr, case_when_false_1L_chr = case_when_var_1L_chr, filter_cdn_1L_chr = filter_cdn_1L_chr, tf_false_val_1L_lgl = tf_false_val_1L_lgl)
  }
  return(cities_df)
}
make_validation_output <- function(allowed_xx,
                                   ds_tb,
                                   var_nm_1L_chr,
                                   sort_1L_lgl = F,
                                   type_1L_chr = "Valid",
                                   what_1L_chr = "Cases"){
  if(is.data.frame(ds_tb) & !tibble::is.tibble(ds_tb)){
    ds_tb <- tibble::as_tibble(ds_tb)
    message("ds_tb has been converted to a tibble")
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
      output_xx <- setdiff(allowed_xx,test_xx)
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
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_tb = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Absent", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_tb = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_tb = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Values")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_tb = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Valid", what_1L_chr = "Cases")
#make_validation_output(ISOcodes::ISO_3166_1$Name %>% sort(),ds_tb = cities_df, var_nm_1L_chr = "country.etc", sort_1L_lgl = T, type_1L_chr = "Invalid", what_1L_chr = "Cases")
get_timezone <- function(country_1L_chr, ## Depends on maps package
                         method_1L_chr = "accurate"){


  latitude_1L_dbl <- -37.8
  longitude_1L_dbl <- 144.9

  lutz::tz_lookup_coords(lat = latitude_1L_dbl, lon = longitude_1L_dbl, method = method_1L_chr)
}
A <- vicinity::VicinityProfile()
A@country_chr <- "Australia"
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

# ready4::get_from_lup_obj(ISOcodes::ISO_3166_1,
#                          match_value_xx = country_1L_chr,
#                          match_var_nm_1L_chr = "Name",
#                          target_var_nm_1L_chr = "Alpha_3") %>%
#   countrycode::countrycode("iso3c","iso4217c")


