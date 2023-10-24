source("data-raw/ToExport.R")
## Country example
x <- CostlyCountries()
x <- renew(x, type_1L_chr = "default")
x <- ratify(x)
## Currency example (brief)
x <- CostlyCountries()
x <- renew(x,
           new_val_xx = add_default_currency_seed(x@CostlySeed_r4, include_1L_chr = "Country"), # add to renew method
           what_1L_chr = "seed")
x <- renew(x, new_val_xx = make_country_correspondences("currencies"), # add to renew method
           type_1L_chr = "final", what_1L_chr = "correspondences")
x <- ratify(x)
y <- CostlyCurrencies()
y <- renew(y, new_val_xx = add_default_currency_seed(y@CostlySeed_r4,
                                                     Ready4useDyad_r4 = x@results_ls$Country_Output_Lookup), #x@results_ls$Country_Input_Seed
           what_1L_chr = "seed")
y <- ratify(y, type_1L_chr = "Lookup")
## Country process, currency dataset example (exploratory logic)
x <- CostlyCountries()
x <- renew(x,
           new_val_xx = add_default_currency_seed(x@CostlySeed_r4, include_1L_chr = "Country"), # add to renew method
           what_1L_chr = "seed")
y <- ratify(x)
#y@results_ls$Country_Output_Validation
y <- ratify(x, new_val_xx = NULL)
#y@results_ls$Country_Output_Correspondences
y <- ratify(renew(x, new_val_xx = character(0), what_1L_chr = "logic"),
            new_val_xx = make_country_correspondences("currencies"))
#y@results_ls
y <- ratify(x, new_val_xx = make_country_correspondences("currencies"))




# A <- vicinity::VicinityProfile()
# A@country_chr <- "Australia"
# ready4::get_from_lup_obj(ISOcodes::ISO_3166_1,
#                          match_value_xx = country_1L_chr,
#                          match_var_nm_1L_chr = "Name",
#                          target_var_nm_1L_chr = "Alpha_3") %>%
#   countrycode::countrycode("iso3c","iso4217c")


