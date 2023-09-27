library(ready4)
library(ready4use)
library(ready4fun)
library(ready4class)
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Develop, Use and Share Unit Cost Datasets for Health Economic Analysis",
                                 pkg_desc_1L_chr = "Tools for undertaking unit costing in conjunction with computational health economic models developed with the ready4 framework (https://ready4-dev.github.io/ready4/).
                            This development version of the costly package has been made available as part of the process of testing and documenting the package.
                            If you have any questions, please contact the authors (matthew.hamilton2@monash.edu).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton1@monash.edu", role = c("aut", "cre", "fnd"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person("Monash University", role = c("cph", "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/costly/",
                                              "https://github.com/ready4-dev/costly",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("knitr","knitrBootstrap","rmarkdown")#,
                                                                       #imports_chr = c(),
                                                                       #depends_chr = c()
                                                                       ),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(),##
                           dev_pkgs_chr = c(#"cmdstanr",
                                            "ready4",#"ready4fun",
                                            "ready4use","ready4show"#,
                                            #"youthvars","scorz",
                                            #"specific"
                                            ),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/costly-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "modelling"#,
                           #zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5646593.svg)](https://doi.org/10.5281/zenodo.5646593)"
                           )
#y <- ready4class::ready4class_constructor()
z <- ready4pack::make_pt_ready4pack_manifest(x#, constructor_r3 = y
                                             ) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
