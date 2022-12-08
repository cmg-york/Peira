prelim <- function(){
  list.of.packages <- c("tidyverse","knitr","eulerr",
                        "gridExtra","readxl","progress","boot",
                        "xtable","irr","PropCIs")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  foo = lapply(list.of.packages, require, character.only = TRUE)
}
library(knitr)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, error = TRUE)
prelim()