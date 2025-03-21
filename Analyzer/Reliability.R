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

staging = FALSE
if (staging) {
  stage <- tibble(read.csv(paste0(dPath,"/gmo/stage.csv"))) %>% distinct() %>% arrange(participant)
  assert(length(stage$participant) == length(unique(stage$participant)))
  uniqueIDNo = length(unique(stage$PROLIFIC_PID)) 
  didRetestNo = length((stage$PROLIFIC_PID)) -  length(unique(stage$PROLIFIC_PID))
}


gmo_data = getData("gmo",dPath,mdPath)
ds <- gmo_data$obs
gme_data = getData("gme",dPath,mdPath)
df <- gme_data$obs


{
  gme_data_pl = getData("gme",dPath_Pilot,mdPath)
  dfpl <- gme_data_pl$obs
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
} 
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
  retest.key = gmo_data$retest$key 
  ds.retest.obs = gmo_data$retest$obs.retest
  
  unique(gmo_data$self$psy_item)
  
  d2.self.overlap = gmo_data$self %>% filter(grepl("conceptOverlap*",psy_item))
  d2.self.relevance = gmo_data$self %>% filter(grepl("conceptRelevance*",psy_item)|grepl("relationshipRelevance*",psy_item))
  d2.self.completeness = gmo_data$self %>% filter(grepl("conceptCompleteness*",psy_item)|grepl("relationshipCompleteness*",psy_item))
  
  d2.self.overlap.retest = gmo_data$retest$self.retest %>% filter(grepl("conceptOverlap*",psy_item))
  d2.self.relevance.retest = gmo_data$retest$self.retest %>% filter(grepl("conceptRelevance*",psy_item)|grepl("relationshipRelevance*",psy_item))
  d2.self.completeness.retest = gmo_data$retest$self.retest %>% filter(grepl("conceptCompleteness*",psy_item)|grepl("relationshipCompleteness*",psy_item))
  
  
  PIDs = unique(retest.key$PROLIFIC_PID)
  result = tribble(~PID,~value,~subjects,~raters,~class,~method)
  for (currentPID in PIDs){
    #currentPID = PIDs[1]
    curr.key = retest.key %>% filter(PROLIFIC_PID == currentPID)
    assert(nrow(curr.key) == 1)

    #
    # Observations
    #
    mat = as.matrix(
      rbind(
        ds %>% 
          filter(participant == curr.key$maintest.id) %>% 
          select(psy_item,response) %>% 
          pivot_wider(names_from = "psy_item",values_from = "response"),
        ds.retest.obs %>% 
          filter(participant == curr.key$retest.id) %>% 
          select(psy_item,response) %>% 
          pivot_wider(names_from = "psy_item",values_from = "response")
      ))
    krip = kripp.alpha(mat)
    result = result %>% add_row(
      PID = currentPID,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      class = "Observations",
      method = krip$method)
    cohen = data.frame(t(mat))
    cohen$X1 = factor(cohen$X1)
    cohen$X2 = factor(cohen$X2)
    
    kap = kappa2(cohen[,1:2])

    
    
    result = result %>% add_row(
      PID = currentPID,
      value = kap$value,
      subjects = kap$subjects,
      raters = kap$raters,
      class = "Observations",
      method = kap$method)
    
    
    #
    # Overlap
    #    
    mat = as.matrix(
      rbind(
        d2.self.overlap %>% 
          filter(participant == curr.key$maintest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value"),
        d2.self.overlap.retest %>% 
          filter(participant == curr.key$retest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value")
      ))
    krip = kripp.alpha(mat,"interval")
    result = result %>% add_row(
      PID = currentPID,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      class = "Overlap",
      method = krip$method)

    #
    # Relevance
    #    
    mat = as.matrix(
      rbind(
        d2.self.relevance %>% 
          filter(participant == curr.key$maintest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value"),
        d2.self.relevance.retest %>% 
          filter(participant == curr.key$retest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value")
      ))
    
    krip = kripp.alpha(mat,"interval")
    result = result %>% add_row(
      PID = currentPID,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      class = "Relevance",
      method = krip$method)
    
    
    
    #
    # Completeness
    #    
    mat = as.matrix(
      rbind(
        d2.self.completeness %>% 
          filter(participant == curr.key$maintest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value"),
        d2.self.completeness.retest %>% 
          filter(participant == curr.key$retest.id) %>% 
          select(psy_item,value) %>% 
          pivot_wider(names_from = "psy_item",values_from = "value")
      ))
    
    krip = kripp.alpha(mat,"interval")
    result = result %>% add_row(
      PID = currentPID,
      value = krip$value,
      subjects = krip$subjects,
      raters = krip$raters,
      class = "Incompleteness",
      method = krip$method)
  }
  
}
result

kappaSum = summary(result %>% filter(class == "Observations", method == kap$method) %>% pull(value))
obsCountKappa = paste0(result %>% filter(class == "Observations", method == kap$method) %>% summarise(n = sum(value > 0.60))%>% pull(n),"/",
                  result %>% filter(class == "Observations", method == krip$method) %>% summarise(n = n()) %>% pull(n))

(obsKappa = ggplot(data = result %>% filter(class == "Observations",method == kap$method),
              aes(x = value)) + 
    geom_histogram(binwidth =  0.1,boundary = 0, color = "black") + 
    bigthema +
    geom_density(alpha=.2, fill="#66FF66") + #xlim(c(0,1)) + 
    xlab(kap$method) + ylab("Frequency") + 
    ggtitle(paste0("Test-retest agreement values (",nrow(result %>% filter(class == "Observations",method == kap$method))," participants).")) +
    annotate("rect",xmin = 0.8,xmax = 1,ymin = 0,ymax = 5.0, alpha = 0.1, fill = "yellow",color = "darkred", size = 1) + 
    annotate("rect",xmin = 0.6,xmax = 1.0,ymin = 0,ymax = 5.0, alpha = 0.1, fill = "yellow",color = "darkred", size = 1) +
    annotate("label", x = 0.8 + (1.0-0.8)/2, y = 5.2, label = "Almost Perfect", size = 5) +
    annotate("label", x = 0.6 + (0.8-0.6)/2, y = 5.2, label = "Substantial", size = 5) +
    annotate("label", x = 0.6 + (1.0-0.6)/2, y = 3.5, label = obsCountKappa, size = 9) +
    scale_x_continuous(breaks=seq(0,1.0,0.1))
)



if (printIMGData){
  ggsave(plot = obsKappa, paste0(paperPath,"7.4.Fig-kapa.hist.pdf"), width = 350/1.3, height = 180/1, units = "mm")
}

if (printTEXData){

  sink(file =  paste0(paperPath,"7.0.1. gmoTook"), append = FALSE)
  cat(reportNumber(nrow(result %>% filter(class == "Observations", method == kap$method))))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. kappaMed.tex"), append = FALSE)
  cat(round(kappaSum[["Median"]],2))
  sink(file = NULL)

  sink(file =  paste0(paperPath,"7.4. kappaSubstantial.tex"), append = FALSE)
  cat(reportNumber(result %>% filter(class == "Observations", method == kap$method) %>% summarise(n = sum(value >= 0.61)) %>% pull(n)))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. kappaPerfect.tex"), append = FALSE)
  cat(reportNumber(result %>% filter(class == "Observations", method == kap$method) %>% summarise(n = sum(value >= 0.81)) %>% pull(n)))
  sink(file = NULL)
  
}



summary(result %>% filter(class == "Observations", method == krip$method) %>% pull(value))
obsCount = paste0(result %>% filter(class == "Observations", method == krip$method) %>% summarise(n = sum(value > 0.677))%>% pull(n),"/",
       result %>% filter(class == "Observations", method == krip$method) %>% summarise(n = n()) %>% pull(n))


overSum = summary(result %>% filter(class == "Overlap") %>% pull(value))
overCount = paste0(result %>% filter(class == "Overlap") %>% summarise(n = sum(value > 0.677))%>% pull(n),"/",
                  result %>% filter(class == "Overlap") %>% summarise(n = n()) %>% pull(n))
overCount1 = result %>% filter(class == "Overlap") %>% summarise(n = sum(value > 0.61))%>% pull(n)

relSum = summary(result %>% filter(class == "Relevance") %>% pull(value))
relCount = paste0(result %>% filter(class == "Relevance") %>% summarise(n = sum(value > 0.677))%>% pull(n),"/",
                   result %>% filter(class == "Relevance") %>% summarise(n = n()) %>% pull(n))
relCount1 = result %>% filter(class == "Relevance") %>% summarise(n = sum(value > 0.61))%>% pull(n)

incSum = summary(result %>% filter(class == "Incompleteness") %>% pull(value))
incCount = paste0(result %>% filter(class == "Incompleteness") %>% summarise(n = sum(value > 0.677))%>% pull(n),"/",
                  result %>% filter(class == "Incompleteness") %>% summarise(n = n()) %>% pull(n))
incCount1 = result %>% filter(class == "Incompleteness") %>% summarise(n = sum(value > 0.61))%>% pull(n)


if (printTEXData){
  
  sink(file =  paste0(paperPath,"7.4. overMed.tex"), append = FALSE)
  cat(round(overSum[["Median"]],2))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. overCount.tex"), append = FALSE)
  cat(reportNumber(overCount1))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. relMed.tex"), append = FALSE)
  cat(round(relSum[["Median"]],2))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. relCount.tex"), append = FALSE)
  cat(reportNumber(relCount1))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. incMed.tex"), append = FALSE)
  cat(round(incSum[["Median"]],2))
  sink(file = NULL)
  
  sink(file =  paste0(paperPath,"7.4. incCount.tex"), append = FALSE)
  cat(reportNumber(incCount1))
  sink(file = NULL)
  
}


(obs = ggplot(data = result %>% filter(class == "Observations"),
       aes(x = value)) + 
  geom_histogram(binwidth =  0.1,boundary = 0, color = "black") + 
  thema +
  geom_density(alpha=.2, fill="#66FF66") + #xlim(c(0,1)) + 
  xlab("Krippendorff's alpha value") + ylab("Frequency") + 
  ggtitle(paste0("Test-retest agreement values (",nrow(result %>% filter(class == "Observations"))," participants).")) +
  annotate("rect",xmin = 0.8,xmax = 1,ymin = 0,ymax = 5.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) + 
  annotate("rect",xmin = 0.667,xmax = 1.0,ymin = 0,ymax = 5.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) +
  annotate("label", x = 0.8 + (1.0-0.8)/2, y = 5.2, label = "Reliable", size = 5) +
  annotate("label", x = 0.677 + (0.8-0.677)/2, y = 5.2, label = "Tentative", size = 5) +
  annotate("label", x = 0.677 + (1.0-0.677)/2, y = 3.5, label = obsCount, size = 9) +
  scale_x_continuous(breaks=seq(0,1.0,0.1))
  )

(over = ggplot(data = result %>% filter(class == "Overlap"),
              aes(x = value)) + 
  geom_histogram(binwidth =  0.1,boundary = 0,color = "black") + 
  thema +
  geom_density(alpha=.2, fill="#66FF66") + xlim(c(-0.8,1)) + 
  xlab("Krippendorff's alpha value") + ylab("Frequency") + 
  ggtitle(paste0("Test-retest agreement values (",nrow(result %>% filter(class == "Overlap"))," participants).")) +
  annotate("rect",xmin = 0.8,xmax = 1,ymin = 0,ymax = 3.1, alpha = 0.1, fill = "yellow",color = "black", size = 2) + 
  annotate("rect",xmin = 0.667,xmax = 1.0,ymin = 0,ymax = 3.1, alpha = 0.1, fill = "yellow",color = "black", size = 2) +
  annotate("label", x = 0.8 + (1.0-0.8)/2,     y = 3.2, label = "Reliable", size = 5) +
  annotate("label", x = 0.677 + (0.8-0.677)/2, y = 3.2, label = "Tentative", size = 5) +
  annotate("label", x = 0.677 + (1.0-0.677)/2, y = 2.5, label = overCount, size = 9) +
  scale_x_continuous(breaks=seq(-0.8,1.0,0.1)))

(rel = ggplot(data = result %>% filter(class == "Relevance"),
             aes(x = value)) + 
  geom_histogram(binwidth = 0.1, boundary = 0, color = "black") + 
  thema +
  geom_density(alpha=.2, fill="#66FF66") + xlim(c(-0.8,1)) + 
  xlab("Krippendorff's alpha value") + ylab("Frequency") + 
  ggtitle(paste0("Test-retest agreement values (",nrow(result %>% filter(class == "Relevance"))," participants).")) +
  annotate("rect",xmin = 0.8,xmax = 1,ymin = 0,ymax = 3.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) + 
  annotate("rect",xmin = 0.667,xmax = 1.0,ymin = 0,ymax = 3.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) +
  annotate("label", x = 0.8 + (1.0-0.8)/2, y = 3.2, label = "Reliable", size = 5) +
  annotate("label", x = 0.677 + (0.8-0.677)/2, y = 3.2, label = "Tentative", size = 5) +
  annotate("label", x = 0.677 + (1.0-0.677)/2, y = 2, label = relCount, size = 9) +
  scale_x_continuous(breaks=seq(-0.8,1.0,0.1))
)

(comp = ggplot(data = result %>% filter(class == "Incompleteness"),
              aes(x = value)) + 
  geom_histogram(binwidth = 0.1, boundary = 0, color = "black") + 
  thema +
  geom_density(alpha=.2, fill="#66FF66") + xlim(c(-0.8,1)) + 
  xlab("Krippendorff's alpha value") + ylab("Frequency") + 
  ggtitle(paste0("Test-retest agreement values (",nrow(result %>% filter(class == "Incompleteness"))," participants).")) +
  annotate("rect",xmin = 0.8,xmax = 1,ymin = 0,ymax = 3.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) + 
  annotate("rect",xmin = 0.667,xmax = 1.0,ymin = 0,ymax = 3.0, alpha = 0.1, fill = "yellow",color = "black", size = 2) +
  annotate("label", x = 0.8 + (1.0-0.8)/2, y = 3.2, label = "Reliable", size = 5) +
  annotate("label", x = 0.677 + (0.8-0.677)/2, y = 3.2, label = "Tentative", size = 5) +
  annotate("label", x = 0.677 + (1.0-0.677)/2, y = 1.5, label = incCount, size = 9) +
  scale_x_continuous(breaks=seq(-0.8,1.0,0.1))
)
grid.arrange(over,rel,comp)


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


