#' ---
#' title: "Multi-selection assessment and test-retest reliability analysis."
#' author: "Sotirios Liaskos"
#' date: "**Notebook compilation time:** `r format(Sys.time(), '%B %d, %Y [%H:%M:%S]')`"
#' output:
#'  html_document:
#'    toc: true
#'    toc_depth: 2
#'    toc_float: true
#'    theme: united
#' ---
#' 


#' # Package Load
# Package Load --------------
library(knitr)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, error = TRUE) 

{
  source("PackageLoad.R")
  #' # Data Generation Settings
  # Data Generation Settings --------------
  (printData = TRUE)
  (generatePlots = TRUE)
}

#
#' # Multi-selection analysis
#' Has the introduction of videos and directions inproved multi-selection?
{
# # Multi-selection analysis ---------------------
 source("Library.R")
  setwd("../Instrument/Generator/")
  source("Library.R")
  source("Preliminary.R")
  setwd("../../Stats")
  dPath = "../Data/Main/Clean/"
  dPath_Pilot = "../Data/Pilot 1/Clean/"
  mdPath = "../Instrument/InstrumentData/"
}

gmo_data = getData("gmo",dPath,mdPath)
ds <- gmo_data[[5]]
gme_data = getData("gme",dPath,mdPath)
df <- gme_data[[1]]
gme_data_pl = getData("gme",dPath_Pilot,mdPath)
dfpl <- gme_data_pl[[1]]
#

t1 = table(df %>% group_by(participant,item) %>% 
  summarise(c = n(), s = sum(response)) %>% pull(s))

t2 = table(dfpl %>% group_by(participant,item) %>% 
        summarise(c = n(), s = sum(response)) %>% pull(s))

multi_ratings = c(sum(t1[2:3]),sum(t2[2:3]))
all_ratings = c(sum(t1),sum(t2))
props = prop.test(multi_ratings,all_ratings,conf.level = 0.95, correct = F)
if (printData)
  props

odds = orscoreci(t1[2],t1[1],t2[2],t2[1], conf.level = 0.95)
if (printData)
  odds

morelikely = round((odds$conf.int[1] - 1) * 100,0)

#' In the updated version participants are `r morelikely`\% more likely 
#' to make multiple selections.
#' 
#' The difference in proportions is statistically significant with p = `r props$p.value`

#
#' # Reliability analysis
# # Reliability analysis ---------------------
#


#
#
#' ## Simple Analysis: each check-box is treated as a binary rating.
## Simple Analysis: each check-box is treated as a binary rating.-----
# 
{
  PIDs = unique(ds$PROLIFIC_PID)
  result = tribble(~PID,~value,~subjects,~raters,~method)
  for (currentPID in PIDs){
    mat = as.matrix(ds %>% filter(PROLIFIC_PID == currentPID) %>% select(-participant,-PROLIFIC_PID,-TIME_end))
    krip = kripp.alpha(mat)
    result = result %>% add_row(
      PID = currentPID,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      method = krip$method)
  }
}
result  
matBinaryExample = mat



#' ## Strict (almost unfair) Analysis: each question is treated as a rating with 2^(size) options.
# ## Strict (almost unfair) Analysis: each question is treated as a rating with 2^(size) options. -----------
# 
ds.st = ds %>% 
  select(participant,PROLIFIC_PID,starts_with("gmo")) %>% 
  pivot_longer(cols = starts_with("gmo"),
               names_to = "psy_item",
               values_to = "response") %>% separate(psy_item,into = c("item","option"),sep=":") %>%
  mutate(rating = as.numeric(option)*response) %>%
  group_by(participant,PROLIFIC_PID,item) %>% summarise(rating = sum(rating),c=n()) %>% ungroup()

ds.strict = list(concepts = ds.st %>% filter(c==4),
                 relationships = ds.st %>% filter(c==5))


result = tribble(~PID,~set,~value,~subjects,~raters,~values,~method)
for (df in ds.strict){
  PIDs = unique(df$PROLIFIC_PID)  
  df = df %>% pivot_wider(names_from = item,values_from = rating) 
  for (currentPID in PIDs){
    optionCount = unique(df$c)[1]
    mat = as.matrix(df %>% filter(PROLIFIC_PID == currentPID) %>% select(-participant,-PROLIFIC_PID,-c))
    krip = kripp.alpha(mat)
    result = result %>% add_row(
      PID = currentPID,
      set = optionCount,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      values = paste(krip$data.values,collapse = ","),
      method = krip$method)
  }
}
result


#
#' ## Indices To Study
# ## To study  --------------
#
kappam.fleiss(t(mat),detail = T) # VERY GOOOOD! Has category-wise kapas!
kappam.light(t(mat))
maxwell(t(matBinaryExample))


