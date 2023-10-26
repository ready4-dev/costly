## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(ready4)
library(ready4use)
library(costly)

## -----------------------------------------------------------------------------
X <- CostlyCorrespondences()

## -----------------------------------------------------------------------------
# Not run
# Y <- Ready4useDyad(ds_tb = tibble::tibble(), dictionary_r3 = ready4use_dictionary())

## -----------------------------------------------------------------------------
Y <- Ready4useDyad() %>% add_country_standards()

## -----------------------------------------------------------------------------
renew(Y, type_1L_chr = "label") %>% 
  exhibit(display_1L_chr = "head", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
exhibit(Y, type_1L_chr = "dict", scroll_box_args_ls = list(width = "100%"))

## -----------------------------------------------------------------------------
Z <- CostlyStandards(Ready4useDyad_r4 = Y, include_chr = c("Country", "Official","Common","A3","A2"), label_1L_chr = "Country")

