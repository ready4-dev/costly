renew_CostlyCorrespondences <- function (x,
                                         new_val_xx = NULL,
                                         type_1L_chr = "slot",
                                         what_1L_chr = "correspondences",
                                         ...) {
  if(type_1L_chr == "default"){
    default_ls <- manufacture(x, what_1L_chr = "defaults")
    x@args_ls <- default_ls$args_ls
    x@correspondences_r3 <- default_ls$correspondences_x_r3
    x@fuzzy_logic_1L_chr <- default_ls$fuzzy_logic_1L_chr
    x@CostlySeed_r4 <- default_ls$CostlySeed_r4
  }
  if(type_1L_chr %in% c("exploratory", "final","slot")){
    y <- manufacture(x, what_1L_chr = "shorthand")
    #what_1L_chr <- match.arg(what_1L_chr, choices = y$old_nms_chr)
    if(what_1L_chr == "correspondences"){
      if(!is.null(new_val_xx) && !identical(new_val_xx, "identity")){
        x@correspondences_r3 <- new_val_xx
      }
      if(is.null(new_val_xx) | !identical(new_val_xx, "identity")){
        x@correspondences_r3 <- update_correspondences(correspondences_x_r3 = x@correspondences_r3,
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
}
