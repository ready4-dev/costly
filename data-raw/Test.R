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


# ```{r}
#
# # x <- renew(x,
# #            new_val_xx = add_default_currency_seed(x@CostlySeed_r4, include_1L_chr = "Country"),
# #            what_1L_chr = "seed")
# #x <- renew(x, "jw", type_1L_chr = "slot", what_1L_chr = "logic")
#
# ```
#
#
# ```{r}
# # y <- ratify(x, new_val_xx = "identity")
# ```
#
# ```{r eval=FALSE}
# # y@results_ls$Country_Output_Validation$Invalid_Values
# ```
# ```{r}
# # x <- renew(x, "jw", type_1L_chr = "slot", what_1L_chr = "logic")
# # y <- ratify(x, new_val_xx = NULL)
# ```
#
# ```{r eval=FALSE}
# # (Not run) Inspect results
# #y@results_ls$Country_Output_Validation$Invalid_Values
# ```
# ```{r eval=FALSE}
# # (Not run) Inspect results
# #y@results_ls$Country_Output_Correspondences
# ```
#
# ```{r}
# # Not run
# # z <- ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(),
# #         old_nms_chr = c("old_name_1", "old_name_2", "etc...."), new_nms_chr = c("new_name_1", "new_name_2", "etc...."))
# ```
#
# ```{r}
# # z <- make_country_correspondences("currencies")
# ```
#
# ```{r}
# # z
# ```
#
# ```{r echo=FALSE}
# # To apply correspondences table without fuzzy logic
# # x <- renew(x, new_val_xx = character(0), what_1L_chr = "logic")
# ```
#
# ```{r}
# # y <- ratify(x, new_val_xx = z)
# ```
# ```{r eval=FALSE}
# # Inspect results
# y@results_ls$Country_Output_Validation$Invalid_Values
# ```
# ```{r}
# y <- renew(y, T, type_1L_chr = "slot", what_1L_chr = "force")
# y <- ratify(y, new_val_xx = "identity")
# ```
#
# ```{r eval=FALSE}
# # Inspect results
# y@results_ls$Country_Output_Validation$Invalid_Values
# ```
# ```{r}
# y@results_ls$Country_Output_Correspondences
# ```
#
# ```{r}
# X <- y@results_ls$Country_Output_Lookup
# ```
#
# ```{r}
# renew(X, type_1L_chr = "label") %>%
#   exhibit(scroll_box_args_ls = list(width = "100%"))
# ```



