procure_CostlySource <- function(x,
                                 matches_chr = character(0),
                                 what_1L_chr = c("names","concepts"),
                                 ...){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr %in% c("names","concepts")){
    if(identical(matches_chr, character(0))){
      if(what_1L_chr == "names"){
        matches_chr <- x@include_chr
      }else{
        matches_chr <- get_corresponding_var(x@Ready4useDyad_r4,
                                             matches_chr = x@include_chr,
                                             what_1L_chr = "names")
      }
    }
    object_xx <- get_corresponding_var(x@Ready4useDyad_r4,
                                       matches_chr = matches_chr,
                                       what_1L_chr = what_1L_chr)
  }
  return(object_xx)
}
