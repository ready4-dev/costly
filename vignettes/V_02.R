## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(ready4)
library(ready4use)
library(costly)

## -----------------------------------------------------------------------------
X <- CostlyCountries()
X <- renew(X,
           new_val_xx = add_default_currency_seed(X@CostlySeed_r4, include_1L_chr = "Country"), 
           what_1L_chr = "seed")
X <- renew(X, "jw", type_1L_chr = "slot", what_1L_chr = "logic") 
X <- renew(X, new_val_xx = make_country_correspondences("currencies"), what_1L_chr = "correspondences") 
X <- renew(X, T, type_1L_chr = "slot", what_1L_chr = "force") 
X <- ratify(X)
Y <- CostlyCurrencies()
Y <- renew(Y, new_val_xx = add_default_currency_seed(Y@CostlySeed_r4,
                                                     Ready4useDyad_r4 = X@results_ls$Country_Output_Lookup), 
           what_1L_chr = "seed")
Y <- ratify(Y, type_1L_chr = "Lookup")
Y <- renew(Y, T, type_1L_chr = "slot", what_1L_chr = "force") 
Y <- ratify(Y, type_1L_chr = "Lookup")

## -----------------------------------------------------------------------------
X <- CostlyCorrespondences()

## ----eval = F-----------------------------------------------------------------
#  # Not run
#  # A <- CostlySeed(Ready4useDyad_r4 = Ready4useDyad(ds_tb = tibble::tibble(), dictionary_r3 = ready4use_dictionary()), include_chr = c("Country"), label_1L_chr = "Country")

## -----------------------------------------------------------------------------
A <- CostlySeed() %>% add_default_currency_seed()

## -----------------------------------------------------------------------------
A@include_chr <- A@label_1L_chr <- "Country"
Y <- CostlyCountries(CostlySeed_r4 = A) %>%
  renew("jw", type_1L_chr = "slot", what_1L_chr = "logic") %>%
  renew(new_val_xx = make_country_correspondences("currencies"), what_1L_chr = "correspondences") %>%
  renew(T, type_1L_chr = "slot", what_1L_chr = "force") %>%
  ratify()

## -----------------------------------------------------------------------------
X <- renew(X, new_val_xx = CostlySeed(Ready4useDyad_r4 = Y@results_ls$Country_Output_Lookup), what_1L_chr = "seed") # 

## -----------------------------------------------------------------------------
renewSlot(X, "CostlySeed_r4@Ready4useDyad_r4", type_1L_chr = "label") %>%
exhibitSlot("CostlySeed_r4@Ready4useDyad_r4", display_1L_chr = "head", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
exhibitSlot(X, "CostlySeed_r4@Ready4useDyad_r4", type_1L_chr = "dict", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
X@CostlySeed_r4@label_1L_chr <- "Currency"
X@CostlySeed_r4@match_1L_chr <- "A3"

## -----------------------------------------------------------------------------
B <- CostlyStandards(Ready4useDyad_r4 = Ready4useDyad() %>% add_currency_standards())

## -----------------------------------------------------------------------------
renewSlot(B, "Ready4useDyad_r4", type_1L_chr = "label") %>% 
  exhibitSlot("Ready4useDyad_r4", display_1L_chr = "head", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
exhibitSlot(B, "Ready4useDyad_r4", type_1L_chr = "dict", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
#B@include_chr <- c("Currency", "Letter")
B@label_1L_chr <- "Currency"
B@match_1L_chr <- "A3"

## -----------------------------------------------------------------------------
X <- renew(X, B, what_1L_chr = "standards")

## -----------------------------------------------------------------------------
X <- ratify(X, new_val_xx = "identity")
X@results_ls$Currency_Output_Validation$Invalid_Values

## ----eval=FALSE---------------------------------------------------------------
#  X@results_ls$Currency_Output_Validation$Absent_Values

## -----------------------------------------------------------------------------
X <- ratify(X, type_1L_chr = "Lookup")

## -----------------------------------------------------------------------------
X@results_ls$Currency_Output_Validation$Invalid_Values

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # X@results_ls$Currency_Output_Validation$Absent_Values

## -----------------------------------------------------------------------------
X <- renew(X, T, type_1L_chr = "slot", what_1L_chr = "force") 
X <- ratify(X, type_1L_chr = "Lookup")

## -----------------------------------------------------------------------------
X@results_ls$Currency_Output_Lookup %>%
  renew(type_1L_chr = "label") %>%
  exhibit(scroll_box_args_ls = list(width = "100%"))

