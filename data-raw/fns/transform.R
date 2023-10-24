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
