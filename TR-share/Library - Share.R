#' Adapted from scripts that accompany book:
#' G. Dikta and M. Scheer. Bootstrap Methods With Applications in R. Springer. 2021
#' Specifically: 
#'   Script k3.R, must be 
#'    (a) placed in a Boot directory, 
#'    (b) erased of all other code except bootstrap.ci
#'    (c) Renamed into k3adapted.R
#'   Script k4.R must be 
#'    (a) placed in a Boot directory, 
#'    (b) erased of all other code except twoSampleBootpvalue
#'    (c) Renamed into k4adapted.R
#'   

source("Boot/k3adapted.R")
source("Boot/k4adapted.R")

## ----bootpackage.ci--------------------------------------------------------------------------------------
bootstrap.ci.ad <- function(x, conf.level, R){
  ret_ <- bootstrap.ci(x, conf.level, R)
  ret = c("lower" = ret_[1],"mean" = mean(x),"upper" = ret_[2])
}




#' accuracyBoot 
#'
#' Creates confidence intervals for the observed accuracy values based on bootstrapping. 
#' 
#' @param ds A tibble with the response data, containing Concept, and accuracy, deficiency and excess events (1 or 0 otherwise) in columns acc, def and exc respectively
#' @param concepts A vector of concepts for each of which to run the analysis.
#' @param conf The confidence level for the interval
#' @param iter The number of bootstrapping iterations
#'
#' @return A tibble with the Recall and Precision confidence intervals.
#' @export
#'
accuracyBoot <- function(df,concepts,conf = 0.9,iter = 999) {
  
  allzero <- function(v){
    return( (length(unique(v)) == 1) & (v[1] == 0) | (length(v) == 0))
  }
  
  bootAcc = tribble(~Concept,~Precision,~Recall,
                    ~precLower,~precMean,~precUpper,
                    ~recLower,~recMean,~recUpper)
  
  for (c in concepts) {
    print(c)
    base = df %>% filter(Concept == c)
    
    if (!allzero(base$acc)) {
      accCI = bootstrap.ci(base$acc,conf.level = conf, R = iter) 
    } else {
      accCI = c(0,0,0)
    }
    
    
    if (!allzero(base$def)) {
      defCI = bootstrap.ci(base$def,conf.level = conf, R = iter)
    } else {
      decCI = c(0,0,0)
    }
    
    if (!allzero(base$exc)) {
      excCI = bootstrap.ci(base$exc,conf.level = conf, R = iter)
    } else {
      excCI = c(0,0,0)
    }
    
    precLower = accCI[1]/(accCI[1]+excCI[3])*100
    precUpper = accCI[3]/(accCI[3]+excCI[1])*100
    precMean = accCI[2]/(accCI[2]+excCI[2])*100
    
    
    recLower = accCI[1]/(accCI[1]+defCI[3])*100
    recUpper = accCI[3]/(accCI[3]+defCI[1])*100
    recMean = accCI[2]/(accCI[2]+defCI[2])*100
    
    bootAcc = bootAcc %>% add_row(Concept = c,  
                                  Precision = paste0(round(precMean,2), " [", 
                                                     round(precLower,2), ",",
                                                     round(precUpper,2), "]"),
                                  Recall = paste0(round(recMean,2), " [", 
                                                  round(recLower,2), ",",
                                                  round(recUpper,2), "]"),
                                  precLower = precLower,
                                  precMean = precMean,
                                  precUpper = precUpper,
                                  recLower = recLower,
                                  recMean = recMean,
                                  recUpper = recUpper)
  }
  return(bootAcc)
}


#' getData
#'
#' Reads pre-processed psytoolkit data containing the participant responses
#' 
#' @param languge The language for which data is read.
#' @param DataPath The path where the data is. 
#'
#' @return A list containing: (a) observational data, (b) self-reported data, (c) demographic data 
#' @export
#'
getData <- function(language,DataPath) {
  
  # Load Observational Descriptions and Authoritative
  kk <- tibble(read.csv("key.csv")) %>% filter(lang == language)
  
  # Load Attitudinal
  kka <- tibble(read.csv("key-att.csv")) %>% filter(lang == language)
  
  # Load Training Data - itMatters is TRUE when there is indeed an authoritative
  kkt <- read.csv(paste0(language,"-key-train.csv")) %>% filter(itMatters)
  
  # d0: concept understanding -------------
  d0 = read_csv(paste0(DataPath,language,"/",language,".csv")) %>% 
    select(participant, 
           starts_with("conceptUnderstandingDefn"),starts_with("conceptUnderstandingExample"),
           starts_with("relationshipUnderstandingDefn"),starts_with("relationshipUnderstandingExample")
    ) %>% 
    pivot_longer(cols = -participant,
                 names_to = "item",
                 values_to = "response") %>% right_join(kkt) 
  
  # Participant IDs that pass the preliminary test.
  clean = d0 %>% group_by(participant) %>% summarise(wrong = mean(response!=authoritative)*n(),total = n()) %>%
    filter(wrong < 3) %>% select(participant)
  
  
  # d1: observational data -------------
  d1 = read_csv(paste0(DataPath,language,"/",language,".csv")) %>% select(participant, starts_with(language)) %>% 
    pivot_longer(cols = starts_with(language),
                 names_to = "psy_item",
                 values_to = "response")  %>% right_join(kk) %>% right_join(clean)
  
  # d2: attitudinal data -----------------------
  
  d2 = read_csv(paste0(DataPath,language,"/",language,".csv")) %>% 
    select(participant, starts_with("concept"),starts_with("relationship"),-starts_with("conceptU"),-starts_with("relationshipU")) %>%
    pivot_longer(cols = c(starts_with("concept"),starts_with("relationship")),
                 names_to = "psy_item",
                 values_to = "value") %>% left_join(kka) %>% right_join(clean)
  
  # de: demographic data ---------------
  de = 
    rbind(
      read_csv(paste0(DataPath,language,"/",language,".csv")) %>% 
        select(participant,"Age:1") %>% rename(demo_item_label = "Age:1") %>% mutate(demo_item_name = "Age") 
      ,
      read_csv(paste0(DataPath,language,"/",language,".csv")) %>% select(participant,"Sex:1",starts_with("Fl:"),starts_with("english")) %>%
        pivot_longer(cols = c("Sex:1",starts_with("Fl:"),starts_with("english")),
                     names_to = "item",
                     values_to = "response") %>% inner_join(read_csv("demos.csv")) %>%
        select(participant,demo_item_name,demo_item_label)
    )  %>% inner_join(clean)
  
  
  
  return(list(d1,d2,de))
}



#' ov2
#'
#' Binary overlap between two categories on a given item. Assume an arbitrary of different categories and raters. 
#' 
#' @param n_s_1 The number of ratings the first category received.
#' @param n_s_2 The number of ratings the second category received. 
#' @param n_s The number of total ratings for the item. 
#'
#' @return The overlap
#' @export
#'
ov2 <- function(n_s_r1,n_s_r2,n_s){
  if(n_s_r1+n_s_r2 > n_s) {
    warning(paste("Sum of individual ratings more than total: ", n_s_r1,"+", n_s_r2,">",n_s))
  }
  if (n_s<2) {
    return (0)
  } else {
    return( (n_s_r1*n_s_r2)/ (ceiling(n_s/2)*floor(n_s/2)) )
  }
}




#' overlaps.per.participant
#'
#' Calculates the per-participant overlap given a set of ratings
#' 
#' @param ov_ratings A tibble with ratings
#' @param conceptList List of concepts for each pair of which to calculate the overlap
#'
#' @return TODO
#' @export
#'
# Overlaps per PARTICIPANT - Amenable to Statistical Analysis.
overlaps.per.participant <- function(ov_ratings,conceptList) {
  result = tribble(~participant, ~item, ~r1, ~r2, ~overlap)
  
  pb <- txtProgressBar(min = 0, 
                       max = length(unique(ov_ratings %>% pull(participant)))*length(unique(ov_ratings %>% pull(item))), 
                       style = 3)
  count = 0
  
  #  ov_ratings = ov_ratings_parti %>% filter(type == "concept")
  #  pt = "s.074adb2b-66a9-491b-855e-244ddc5f8a70.txt"
  #  it = "descr1_item1_concepts"
  #  r1 = "Actor"
  #  r2 = "Belief"
  
  for (pt in unique(ov_ratings %>% pull(participant))){
    for (it in unique(ov_ratings %>% pull(item))) {
      for (i in 1:(length(conceptList)-1)) {
        for (j in (i+1):(length(conceptList))) {
          r1 = conceptList[i]
          r2 = conceptList[j]
          resp = ov_ratings %>% ungroup() %>% filter(participant == pt, item == it,concept %in% c(r1,r2)) %>% select(n,n_s)
          result = result %>% add_row(participant = pt, 
                                      item = it, 
                                      r1 = r1, r2 = r2, 
                                      overlap = ov2(resp[[1,c("n")]],resp[[2,c("n")]],resp[[1,c("n_s")]]))
        }
      }
      count = count + 1
      setTxtProgressBar(pb, count)
    }
  }
  
  out = list(means = result %>%
               mutate(pair = paste0(r1,"-",r2)) %>%
               group_by(participant,r1,r2,pair) %>%
               summarise(Obs = mean(overlap),
                         mean = mean(overlap),
                         total = sum(overlap),
                         max = max(overlap),
                         median = median(overlap),
                         min = min(overlap)
               ),
             data = result)
  invisible(out)
}



#' overlaps.per.item
#'
#' Calculates the per-item overlap given a set of ratings
#' 
#' @param ov_ratings A tibble with ratings
#' @param conceptList List of concepts for each pair of which to calculate the overlap
#' @param threshold A percentage of total ratings to a concept exceeding which makes the concept relevant
#'
#' @return TODO
#' @export
#'
# Overlaps per ITEM --------------------------------
overlaps.per.item <- function(ov_ratings,conceptList,threshold = 0.1) {
  result = tribble(~item, ~Concept1, ~Concept2, ~overlap)
  redresult = tribble(~item, ~Concept1, ~Concept2, ~overlap)
  
  # ov_ratings = ds1_ratings %>% filter(type == "concept")
  # conceptList = lang[["gmo"]][["concepts"]]
  # r1 = "Actor"
  # r2 = "Belief"
  # it = "descr1_item1_concepts"
  
  
  # For all elements / items
  for (it in unique(ov_ratings %>% pull(item))) {
    # For each concept
    for (i in 1:((length(conceptList))-1)) {
      # For each other concept
      for (j in (i+1):(length(conceptList))) {
        r1 = conceptList[i]
        r2 = conceptList[j]
        # Outer concept ratings for the item in question
        n_s_r1 = ov_ratings %>% filter(item == it,concept == r1) %>% pull(n)
        # Inner concept ratings for the item in question
        n_s_r2 = ov_ratings %>% filter(item == it,concept == r2) %>% pull(n)
        # Total ratings  for the item in question
        n_s = ov_ratings %>% filter(item == it,concept == r1) %>% pull(n_s)
        
        # Add overlap to a results table - for reference
        result = result %>% add_row(item = it, Concept1 = r1, Concept2 = r2, overlap = ov2(n_s_r1,n_s_r2,n_s))
        
        # For redundancy calculations, add the record only if the item is relevant to one of the 
        # concepts.
        if ((n_s_r1 >= threshold*n_s) | (n_s_r2 >= threshold*n_s)) {
          redresult = redresult %>% add_row(item = it, Concept1 = r1, Concept2 = r2, overlap = ov2(n_s_r1,n_s_r2,n_s))
        }
      }
    }
  }
  
  # Each pair has as many ratings as the items.
  # Calculate various statistics
  stats = redresult %>% group_by(Concept1,Concept2) %>% 
    summarise(mean = mean(overlap),
              Obs = mean(overlap),
              max = max(overlap),
              q90 = quantile(overlap,0.9),
              q75 = quantile(overlap,0.75),
              median = quantile(overlap,0.5),
              q25 = quantile(overlap,0.25),
              q10 = quantile(overlap,0.1), 
              min = min(overlap))
  
  out = list(stats = stats,data = result)
  print(stats)
  invisible(out)
}