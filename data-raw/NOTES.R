library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 treat_as_words_chr = c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr,"standardised") %>% sort()
                 )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
