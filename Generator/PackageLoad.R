loadLib <- function(){
  list.of.packages <- c("tidyverse","knitr","eulerr",
                        "gridExtra","readxl","readr","progress","boot",
                        "xtable","english")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  foo = lapply(list.of.packages, require, character.only = TRUE)
}

loadLib()
