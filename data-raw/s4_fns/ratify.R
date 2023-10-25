ratify_CostlyCorrespondences <- function(x,
                                         new_val_xx = "identity",
                                         sort_1L_lgl = T,
                                         type_1L_chr = make_ds_names(type_chr = character(0))[1:2],
                                         what_chr = make_ds_names(type_chr = character(0)),
                                         ...){
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "Correspondences"){
    if(!identical(new_val_xx, "identity") && !is.null(new_val_xx)){
      correspondences_x_r3 <- new_val_xx
    }else{
      correspondences_x_r3 <- x@correspondences_r3
    }
    x <- renew(x, new_val_xx = new_val_xx, type_1L_chr = "final", what_1L_chr = "correspondences")

  }else{
    correspondences_x_r3 <- x@correspondences_r3
  }
  # match_1L_chr <- x@CostlySeed_r4@label_1L_chr ## @include_chr[1]
  ds_df <- manufacture(x, type_chr = type_1L_chr, what_1L_chr = "dataset")
  validation_ls <- make_validation_ls(x@CostlyStandards_r4@Ready4useDyad_r4@ds_tb %>% dplyr::pull(procure(x@CostlyStandards_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, what_1L_chr = "names")),
                                      ds_df = ds_df,
                                      var_nm_1L_chr = procure(x@CostlySeed_r4, matches_chr = x@CostlySeed_r4@label_1L_chr, #match_1L_chr
                                                              what_1L_chr = "names"),
                                      sort_1L_lgl = sort_1L_lgl)
  x@results_ls <- make_standardised_dss(x@CostlySeed_r4@label_1L_chr,
                                        lookup_Ready4useDyad = ready4use::Ready4useDyad(ds_tb = as.data.frame(ds_df), dictionary_r3 = x@CostlySeed_r4@Ready4useDyad_r4@dictionary_r3),
                                        seed_Ready4useDyad = x@CostlySeed_r4@Ready4useDyad_r4,
                                        standards_Ready4useDyad = x@CostlyStandards_r4@Ready4useDyad_r4,
                                        validation_ls = validation_ls,
                                        what_chr = what_chr,
                                        correspondences_x_r3 = correspondences_x_r3,
                                        correspondences_y_r3 = x@correspondences_r3)
  return(x)
}
