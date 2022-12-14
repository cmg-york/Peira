
## ----bootpackage.ci--------------------------------------------------------------------------------------
bootstrap.ci <- function(x, conf.level, R){
  # x - observed data
  # conf.level - confidence level
  # R number of MC simulations
  
  # calculate mean and variance for the bootstrapped sample
  mean_and_var <- function(d, i){
    # d - observed data
    # i - boot::boot tells us which indices to be used to 
    #     obtain the resampling version of the observed data
    
    d_boot <- d[i]
    c(mean_boot <- mean(d_boot), variance_boot = var(d_boot))
  }
  
  # resampled mean and variance
  b <- boot::boot(x, mean_and_var, R = R)
  
  # type = "stud" stands for studentized statistics
  ret <- boot::boot.ci(b, conf = conf.level, type = "stud", 
                       # var.t0 - variance of the observed data
                       var.t0 = var(x), 
                       # var.t - variances for every 
                       #         resampled data sets
                       var.t = b$t[,2])$student[4:5]
  ret <- c("lower" = ret[1],"mean" = mean(x),"upper" = ret[2]) 
  #names(ret) <- c("lower", "upper")
  ret
}

twoSampleBootpvalue = function(x, y, 
                               alternative = c("two-sided", "less", "greater"), R = 999){
  
  # x - observed data (first sample)
  # y - observed data (second sample)
  # alternative - specifies the alternative hypothesis
  # R - number of MC simulations
  
  alternative <- match.arg(alternative)
  
  n1 <- length(x)
  n2 <- length(y)
  
  # test statistic
  tstat <- function(d, i){
    boot.xy <- d[i]    
    x       <- boot.xy[1:n1]
    y       <- boot.xy[-(1:n1)]
    s       <- sqrt( var(x) / n1 + var(y) / n2 )
    mu.x    <- mean(x)
    mu.y    <- mean(y)
    (mu.x - mu.y) / s
  }
  # test statistic for the observed data
  xy <- c(x,y)
  t0 <- tstat(xy, 1:(n1+n2))  
  
  # in order to facilitate step (A) 
  # x and y are centered
  x  <- x - mean(x)
  y  <- y - mean(y)
  xy <- c(x,y)
  
  # R resampled test statistics
  # Note, the strata parameter explains boot() to resample
  # from the first n1 entries and from the last n2 entries
  # separately 
  bt <- boot::boot(xy, tstat, R = R, 
                   strata = c(rep(1, n1), rep(2,n2)))$t[,1]
  
  # return pvalue
  if(alternative == "greater") return(c(pvalue = mean(bt > t0)))
  if(alternative == "less") return(c(pvalue = mean(bt < t0)))
  c(pvalue = mean(abs(bt) > abs(t0)))
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
      defCI = c(0,0,0)
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
#' @param dataPath The path where the data is. 
#'
#' @return A list containing: (a) observational data, (b) self-reported data, (c) demographic data 
#' @export
#'
getData <- function(languag,dataPath,keyPath) {
  #languag= "gmo"
  #dataPath = "../Data/Main/Clean/"
  #dataPath = "../Data/Pilot 1/Clean/"
  #keyPath = "../Instrument/InstrumentData/"
  # Load Observational Descriptions and Authoritative
  kk <- tibble(read.csv(paste0(keyPath,"key.csv"))) %>% filter(language == languag)
  
  # Load Attitudinal
  kka <- tibble(read.csv(paste0(keyPath,"key-att.csv"))) %>% filter(language == languag)
  
  # Load Training Data - itMatters is TRUE when there is indeed an authoritative
  kkt <- read.csv(paste0(keyPath,languag,"-key-train.csv")) %>% filter(itMatters)

  d = read_csv(paste0(dataPath,languag,"/",languag,".csv"))
  
  # d0: concept understanding -------------
  # Participant IDs that pass the preliminary test.
  # Analysis does not filter anything with PILOT as authoritative have changed.
  d0 = d %>%
  #  filter(!participant %in% d.retest.ids$parti) %>%
    select(participant, 
           starts_with("conceptUnderstandingDefn"),starts_with("conceptUnderstandingExample"),
           starts_with("relationshipUnderstandingDefn"),starts_with("relationshipUnderstandingExample")
    ) %>% 
    pivot_longer(cols = -participant,
                 names_to = "item",
                 values_to = "response") %>% right_join(kkt)
  
  if ("PROLIFIC_PID" %in% colnames(d)){
    clean = d0 %>% group_by(participant) %>% summarise(wrong = mean(response!=authoritative)*n(),total = n()) %>%
      filter(wrong < 3) %>% select(participant)
    unclean = tibble(participant = setdiff(unique(d0$participant), clean$participant))
  } else {
    clean = unique(d %>% select(participant))
    unclean = tribble(~participant)
  }
  
  
  # Separate test from retest
  if ("PROLIFIC_PID" %in% colnames(d)){
    d.retest.basis = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% 
      select(participant, PROLIFIC_PID, TIME_end)
    # Extract those that are later and are of IDs that have been seen before
    d.retest.ids = d.retest.basis %>% 
      group_by(PROLIFIC_PID) %>% summarise(t = max(TIME_end),participant = first(participant)) %>%
      inner_join(d.retest.basis %>% group_by(PROLIFIC_PID) %>% summarise(n = n()) %>% filter(n>1)) %>%
      inner_join(clean)
  } else {
    d.retest.ids = tribble(~PROLIFIC_PID,~t,~parti,~n)
  }
  

  
  # d1.retest: observational data (retest) ----------------
  # It has to be the prolific version
  if ("PROLIFIC_PID" %in% colnames(read_csv(paste0(dataPath,languag,"/",languag,".csv")))){
    d1.retest.basis = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% 
      select(participant, PROLIFIC_PID, TIME_end,starts_with(languag))
    # Dataset for test-retest analysis
    d1.test.retest = d1.retest.basis %>% filter(participant %in% (d.retest.ids %>% pull(participant)))
  } else {
    d1.test.retest = NULL
    d1.retest.ids = tribble(~parti)
  }
  
  # d1: observational data -------------
  d1 = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% 
    # Exclude the retest entries
    filter(!participant %in% (d.retest.ids %>% pull(participant))) %>% 
    # Clean
    inner_join(clean) %>%
    select(participant,starts_with(languag)) %>% 
    pivot_longer(cols = starts_with(languag),
                 names_to = "psy_item",
                 values_to = "response")  %>% right_join(kk) 
  
  # d2: attitudinal data -----------------------
  d2 = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>%
    # Exclude the retest entries
    filter(!participant %in% (d.retest.ids %>% pull(participant))) %>% 
    # Clean
    inner_join(clean) %>%
    select(participant, starts_with("concept"),starts_with("relationship"),-starts_with("conceptU"),-starts_with("relationshipU")) %>%
    pivot_longer(cols = c(starts_with("concept"),starts_with("relationship")),
                 names_to = "psy_item",
                 values_to = "value") %>% left_join(kka)
  
  
  # de: demographic data ---------------
  de = 
    rbind(
      read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% 
      # Exclude the retest entries
      filter(!participant %in% (d.retest.ids %>% pull(participant))) %>% 
        select(participant,"Age:1") %>% rename(demo_item_label = "Age:1") %>% mutate(demo_item_name = "Age") 
      ,
      read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% select(participant,"Sex:1",starts_with("Fl:"),starts_with("english")) %>%
        # Exclude the retest entries
        filter(!participant %in% (d.retest.ids %>% pull(participant))) %>% 
        pivot_longer(cols = c("Sex:1",starts_with("Fl:"),starts_with("english")),
                     names_to = "item",
                     values_to = "response") %>% inner_join(read_csv(paste0(keyPath,"demos.csv"))) %>%
        select(participant,demo_item_name,demo_item_label)
    )  %>% inner_join(clean)
  
  
  if ("PROLIFIC_PID" %in% colnames(read_csv(paste0(dataPath,languag,"/",languag,".csv")))){
    cl = list(clean = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% select(participant,PROLIFIC_PID) %>% inner_join(clean),
              unclean = read_csv(paste0(dataPath,languag,"/",languag,".csv")) %>% select(participant,PROLIFIC_PID) %>% anti_join(clean))
  } else {
    cl = NULL
  }
  
  return(list(d1,d2,de,cl,d1.test.retest))
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



reportWilcox <- function(obj,alpha = 0.05,inMath = FALSE){
  W = round(obj$statistic,1)
  p = obj$p.value
  if (p <= 0.001) {
    res = paste0("W=",W,", p<0.001")
  } else if (p <= alpha) {
    res = paste0("W=",W,", p=", round(p,4))
  } else {
    res = paste0("W=",W,", p=", round(p,4), ", \\red{NOT REJECTED}")
  }
  if (inMath){
      return(res)
  } else {
      return(paste0("$",res,"$"))
  }
}

reportKendalCor <- function(obj,alpha = 0.05,inMath = FALSE){
  z = round(obj$statistic,1)
  p = obj$p.value
  tau = round(obj$estimate,2)
  if (p <= 0.001) {
    res = paste0("\\tau=",tau,", p<0.001")
  } else if (p <= alpha) {
    res = paste0("\\tau=",tau,", p=", round(p,4))
  } else {
    res = paste0("\\tau=",tau,", p=", round(p,4), ", \\red{NOT REJECTED}")
  }
  if (inMath){
    return(res)
  } else {
    return(paste0("$",res,"$"))
  }
}

reportNumber <- function(x){
  return (paste0(as.english(x)," (",round(x,0),")"))
}



# Global Parameters ----- 
thema = theme(text = element_text(size=14),
              strip.text.x = element_text(size=14),
              strip.text.y = element_text(size=14),
              axis.title = element_text(size=15),
              axis.text = element_text(size=14),
              title = element_text(size = 16),
              legend.text = element_text(size=14),
              panel.background = element_rect(fill = "gray90",
                                              colour = "gray90",
                                              size = 0.5, linetype = "solid"),
              legend.position = "bottom")

qualthema = theme(
  axis.title = element_text(size=22),
  axis.text = element_text(size=16),
  text = element_text(size=16),
  
  panel.background = element_rect(fill = "gray90",
                                  colour = "gray90",
                                  size = 0.5, linetype = "solid"),
  legend.position = "bottom")


bigthema = theme(
  title = element_text(size = 23),            
  axis.title = element_text(size=28),
  legend.title = element_text(size=28),
  
  axis.text = element_text(size=26),
  legend.text = element_text(size=26),
  
  text = element_text(size=28),
  strip.text.x = element_text(size=22),
  strip.text.y = element_text(size=22),
  
  panel.background = element_rect(fill = "gray90",
                                  colour = "gray90",
                                  size = 0.5, linetype = "solid"),
  legend.position = "bottom")




