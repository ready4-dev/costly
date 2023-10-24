library(ready4)
library(ready4use)
library(ready4fun)
library(ready4class)
library(ready4show)
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
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                name_stub_chr = "Source",
                                                                slots_ls = list("Ready4useDyad_r4",
                                                                                "include_chr",
                                                                                "label_1L_chrs",
                                                                                "match_1L_chr") %>% list(),
                                                                pt_ls = list("Ready4useDyad",
                                                                             "character",
                                                                             "character",
                                                                             "character") %>% list(),
                                                                vals_ls = list(list(Ready4useDyad_r4 = "ready4use::Ready4useDyad()",
                                                                                    include_chr = "NA_character_",
                                                                                    label_1L_chr = "\"Standardised\"",
                                                                                    match_1L_chr = "\"A3\"")),
                                                                class_desc_chr = "Input dataset (and metadata) for generating standardised costing datasets.",
                                                                parent_class_chr = "Ready4Module")#,
                   # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                   #                                              name_stub_chr = "Seed",
                   #                                              slots_ls = list("args_ls") %>% list(),
                   #                                              pt_ls = list("list") %>% list(),
                   #                                              vals_ls = list(list(args_ls = "list()")),
                   #                                              class_desc_chr = "Original (non-standardised) dataset (and metadata).",
                   #                                              parent_class_chr = "CostlySource"),
                   # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                   #                                              name_stub_chr = "Standards",
                   #                                              slots_ls = list("args_ls") %>% list(),
                   #                                              pt_ls = list("list") %>% list(),
                   #                                              vals_ls = list(list(args_ls = "list()")),
                   #                                              class_desc_chr = "Dataset (and metadata) defining the allowable values of specified variables.",
                   #                                              parent_class_chr = "CostlySource"),
                   # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                   #                                              name_stub_chr = "Correspondences",
                   #                                              slots_ls = list("CostlySeed_r4",
                   #                                                              "CostlyStandards_r4",
                   #                                                              "correspondences_r3",
                   #                                                              "args_ls",
                   #                                                              "force_standard_1L_lgl",
                   #                                                              "fuzzy_logic_1L_chr",
                   #                                                              "max_distance_1L_dbl",
                   #                                                              "results_ls") %>% list(),
                   #                                              pt_ls = list("CostlySeed",
                   #                                                           "CostlyStandards",
                   #                                                           "ready4show_correspondences",
                   #                                                           "list",
                   #                                                           "logical",
                   #                                                           "character",
                   #                                                           "numeric",
                   #                                                           "list") %>% list(),
                   #                                              vals_ls = list(list(CostlySeed_r4 = "CostlySeed()",
                   #                                                                  CostlyStandards_r4 = "CostlyStandards()",
                   #                                                                  correspondences_r3 = "ready4show::ready4show_correspondences()",
                   #                                                                  args_ls = "list()",
                   #                                                                  force_standard_1L_lgl = "F",
                   #                                                                  fuzzy_logic_1L_chr = "character(0)",
                   #                                                                  max_distance_1L_dbl = "Inf",
                   #                                                                  results_ls = "list()")),
                   #                                              class_desc_chr = "Collection of input, standards definition and results datasets for projects to generate standardised costing datasets.",
                   #                                              parent_class_chr = "Ready4Module",
                   #                                              inc_clss_ls = list("CostlySeed","CostlyStandards") %>% list()),
                   # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                   #                                              name_stub_chr = "Countries",
                   #                                              vals_ls = list(list(CostlySeed_r4 = "add_source_label(CostlySeed(),\"Country\")",
                   #                                                                  CostlyStandards_r4 = "make_country_standards()",
                   #                                                                  fuzzy_logic_1L_chr = "\"jw\"")),
                   #                                              class_desc_chr = "Collection of input, standards definition and results datasets for projects to generate standardised country data for use in costing datasets.",
                   #                                              parent_class_chr = "CostlyCorrespondences",
                   #                                              inc_clss_ls = list("CostlySeed","CostlyStandards","CostlyCorrespondences") %>% list()),
                   # ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                   #                                              name_stub_chr = "Currencies",
                   #                                              vals_ls = list(list(CostlySeed_r4 = "add_source_label(CostlySeed(),\"Currency\")",
                   #                                                                  CostlyStandards_r4 = "make_currency_standards()")),
                   #                                              class_desc_chr = "Collection of input, standards definition and results datasets for projects to generate standardised currency data for use in costing datasets.",
                   #                                              parent_class_chr = "CostlyCorrespondences",
                   #                                              inc_clss_ls = list("CostlySeed","CostlyStandards","CostlyCorrespondences") %>% list())
                   )
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
ready4::write_extra_pkgs_to_actions(consent_1L_chr = "Y")
ready4::write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
readLines("_pkgdown.yml") %>%
  stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
  writeLines(con = "_pkgdown.yml")
devtools::build_vignettes()
