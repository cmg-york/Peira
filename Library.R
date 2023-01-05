
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



# ACCURACY ----------------------


#' accuracyBoot 
#'
#' Creates confidence intervals for the observed accuracy values based on bootstrapping. 
#' 
#' @param df A tibble with the response data, containing language, concept, and accuracy, deficiency and excess events (1 or 0 otherwise) in columns acc, def and exc respectively
#' @param conceptStruct A list of named vectors each representing a concept set, where the name is the language as it appears in df. 
#' @param conf The confidence level for the interval
#' @param iter The number of bootstrapping iterations
#'
#' @return A tibble with the Recall and Precision confidence intervals.
#' @export
#'
accuracyBoot <- function(df, conceptStruct, conf = 0.9, iter = 999) {
  
  allzero <- function(v){
    return( (length(unique(v)) == 1) & (v[1] == 0) | (length(v) == 0))
  }
  
  bootAcc = tribble(~Language,~Concept,
                    ~Precision,~Recall,
                    ~precLower,~precMean,~precUpper,
                    ~recLower,~recMean,~recUpper)
  
  for (l in names(conceptStruct)) {
    for (c in conceptStruct[[l]]) {
      base = df %>% filter(concept == c, language == l)
      
      if (!allzero(base$acc)) {
        accCI = bootstrap.ci(base$acc,conf.level = conf, R = iter) # Bootstrap over all accuracy values.
      } else {
        accCI = c(0,0,0)
      }
      
      
      if (!allzero(base$def)) {
        defCI = bootstrap.ci(base$def,conf.level = conf, R = iter) # Bootstrap over all deficiency values.
      } else {
        defCI = c(0,0,0)
      }
      
      if (!allzero(base$exc)) {
        excCI = bootstrap.ci(base$exc,conf.level = conf, R = iter) # Bootstrap over all excess values.
      } else {
        excCI = c(0,0,0)
      }
      
      precLower = accCI[1]/(accCI[1]+excCI[3])*100 # excCI[3] to ensure larger possible denominator.
      precUpper = accCI[3]/(accCI[3]+excCI[1])*100 # excCI[1] to ensure smaller possible denominator.
      precMean = accCI[2]/(accCI[2]+excCI[2])*100
      
      
      recLower = accCI[1]/(accCI[1]+defCI[3])*100
      recUpper = accCI[3]/(accCI[3]+defCI[1])*100
      recMean = accCI[2]/(accCI[2]+defCI[2])*100
      
      bootAcc = bootAcc %>% add_row(Language = l,
                                    Concept = c,  
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
  }
  return(bootAcc)
}


#' print_Euler
#'
#' @param df A tibble with columns language, concept, acc, exc, def, with the latter three being the number of accurate, excessive and deficient ratings that are observed for concept in language. Typically the accuracy.per.concept output of the accuracy.per.concept function.
#' @param lang The language in which the concept conc is studied.
#' @param conc The concept in focus
#'
#' @return An Euler plot showing the overlap between authoritative and observed ratings.
#' @export
#'
#' @examples
print_Euler <- function(df,lang,conc) {
  df = acc.pc
  lang = "gmo"
  conc = "Actor"
  x = as.numeric(df %>% filter(concept == conc, language == lang) %>% select(exc,acc,def))
  names(x) = c("Rater","Auth.&Rater","Auth.")
  return( plot(euler(x), quantities = TRUE,lty=1:2, main = concept))
}


#' accuracy.per.concept
#'
#' @param observations A tibble containing at least columns: participant, item, language, class, option, response. Response is 1 if participant has rated item as option in language, and 0 otherwise. It is assumed that all combinations of items and options are listed for each language. 
#' @param authoritative A tibble containing at least columns: item, language, auth. The latter (auth) denotes the concept in which item must be normally classified. One row per item.
#' @param NAString How is the NA option denoted in the data. Rows with NA will be excluded from all calculations.
#'
#' @return A list with the following three data sets:
#' acc.data --> the observations with three added columns: acc, def, exc, each marking (with 1, otherwise 0) if the rating should be counted as accurate, deficient (response shoudl be 1 but was 0) or excessive (response should be 0 but was 1).
#' acc.per.concept --> a tibble with language, class, concept and acc, exc, def, representing the total ratings per concepts, precision and recall, directly calculated from acc, exc and def, as well as mean values (mean.acc, mean.exc, mean.def). Mean values are calculated as a fraction of total ratings, so normally add up to around 1.0.
#' acc.per.language --> as above, but aggregation is happening at the language level.
#' @export
#'
#' @examples
accuracy.per.concept <- function(observations, authoritative, NAString = "None"){
  
  df1 = observations %>% inner_join(authoritative, by = c("item","language"))
  
  acc.data = df1 %>% mutate(authoritative = ifelse((option == auth),1,0),
           acc = ifelse((response == 1) & (authoritative == 1),1,0), # Accurate response
           def = ifelse((response == 0) & (authoritative == 1),1,0), # Scope deficiency response
           exc = ifelse((response == 1) & (authoritative == 0),1,0), # Excessive response
    ) %>%
    filter((response != 0) | (authoritative != 0), option != NAString)  %>%  
    rename ("concept" = option) %>%
    select(participant,item, item_full, language, class, concept, acc, def, exc)
  
  acc.per.concept = acc.data  %>% 
      group_by(language,class,concept) %>%
      summarise(mean.acc = mean(acc),
                mean.exc = mean(exc),
                mean.def = mean(def),
                acc = sum(acc), # acc(.,.,v)
                exc = sum(exc), # exc(.,.,v)
                def = sum(def), # def(.,.,v)
                precision = ifelse(exc == 0,100,acc/(acc+exc)*100),
                recall = ifelse(def == 0,100,acc/(acc+def)*100)) %>% 
    ungroup()
  
  acc.per.language = acc.data  %>% 
    group_by(language) %>%
    summarise(mean.acc = mean(acc),
              mean.exc = mean(exc),
              mean.def = mean(def),
              acc = sum(acc), # acc(.,.,.)
              exc = sum(exc), # exc(.,.,.)
              def = sum(def), # def(.,.,.)
              precision = ifelse(exc == 0,100,acc/(acc+exc)*100),
              recall = ifelse(def == 0,100,acc/(acc+def)*100)) %>% 
    ungroup()
  
  out = list(acc.data = acc.data,
             acc.per.concept = acc.per.concept,
             acc.per.language = acc.per.language)
  print(acc.per.language)
  invisible(out)
}
  
#' accuracy.per.participant
#'
#' @param data A tibble with participant, item, language, class, concept, acc, def, exc. The last three indicate if rating is accurate, deficient or excessive. Typically the acc.data output of accuracy.per.concept.
#' @param concepts A list of concepts of interest. If empty, analysis is done for all concepts.
#'
#' @return A tibble with language, participant,class, concept, acc_pp, exc_pp, def_pp, precision_pp, recall_pp. Combinations of participant and concept are exhaustively included. Values acc_pp, ... are the per participant responses for each combination of participant and concept, i.e. how many times the participant has used a concept accurately (acc_pp) how many times excessively (exc_pp -- used it but they shouldn't), and how many times "negligently" (def_pp -- should have used it but they didn't). Precision_pp and recall_pp follow as per the normal calculation. 
#' @export
#'
#' @examples
accuracy.per.participant <- function(data, concepts = NULL) {
  if (is.null(concepts)) {
    result = data %>% group_by(language,participant,class,concept) %>% 
      summarise(acc_pp = sum(acc), #acc(p,.,v)
                exc_pp = sum(exc), #exc(p,.,v)
                def_pp = sum(def), #def(p,.,v)
                precision_pp = ifelse(exc_pp == 0, 1, acc_pp/(acc_pp+exc_pp)),
                recall_pp = ifelse(def_pp == 0, 1, acc_pp/(acc_pp+def_pp))
      )
  } else {
    result = data %>% filter(Concept %in% concepts) %>%
      group_by(language,participant,concept) %>% 
      summarise(acc_pp = sum(acc), #acc(p,.,v)
                def_pp = sum(def), #exc(p,.,v)
                exc_pp = sum(exc), #def(p,.,v)
                precision_pp = ifelse(exc_pp == 0, 1, acc_pp/(acc_pp+exc_pp)),
                recall_pp    = ifelse(def_pp == 0, 1, acc_pp/(acc_pp+def_pp))
      )
  }
  invisible(result)
}



# DEFICIT ----------------------

#' deficit
#'
#' @param observations A tibble containing at least columns: participant, item, language, class, option, response. Response is 1 if participant has rated item as option in language, and 0 otherwise. It is assumed that all combinations of items and options are listed for each language. 
#' @param NAString How is the NA option denoted in the data. Rows with NA will be excluded from all calculations.
#'
#' @return A list with the following tibbles:
#' def.item --> Columns: language, class, item, item_full, nas, totals, Deficit. nas: the number of ratings the item received under NA [n(.,s,v_NA) - represents a participant count], totals: the number of ratings the item has received under any concept [n(.,s,.)], deficit: the ratio of the two.
#' def.tab --> Columns: language, class, max, q95, q90, q75. Aggregate Deficit ratings from def.item above by language, class. Each combination of language, class is now associated with a list of Deficit ratings, one per item. Calculate various statistics (quantiles) of those ratings. Max or q95 are the most reliable descriptors of deficit.
#' def.parti --> Columns: language, class, participant, nas, totals, Deficit. nas: the number of ratings the participant gave under NA [n(p,.,v_NA) - represents an item count], totals: the number of ratings the participant has given under any concept [n(p,.,.)], deficit: the ratio of the two.
#' @export
#'
#' @examples
deficit <- function(observations, NAString = "None"){
  def.item = 
    # get the NA ratings --> n(.,s,v_NA)
    observations %>% mutate(resp_na = ifelse((option == NAString) & (response == 1),1,0)) %>% 
    group_by(language,class,item, item_full) %>% summarise (nas = sum(resp_na)) %>% # aggregate by item only
    # get all ratings as well --> n(.,s,.) 
    # alternatives inlcude: all items (not as in paper)
    inner_join(observations %>% group_by(language,class,item) %>% summarise (totals = sum(response)),  
               by = c("language"="language","class"="class","item"="item")) %>%
    mutate(Deficit = nas/totals) # find the ratio of NA ratings over total ratings.

  def.tab = def.item %>% group_by(language,class) %>% summarise(max = max(Deficit),
                                                                q95 = quantile(Deficit, 0.95),
                                                                q90 = quantile(Deficit, 0.90),
                                                                q75 = quantile(Deficit, 0.75)) 
  
  def.parti = 
    # get the NA ratings --> n(p,s,v_NA)
    observations %>% mutate(resp_na = ifelse((option == NAString) & (response == 1),1,0)) %>%
    group_by(language,class,participant) %>% summarise (nas = sum(resp_na)) %>% # aggregate per item and participant
    inner_join(
      # All the opportunities for NA (not as in paper)
      # observations %>% group_by(language,class,participant) %>% summarise (totals = sum(option == NAString))) %>%
      # All the ratings given by participant (as in paper)
      observations %>% group_by(language,class,participant) %>% summarise (totals = sum(response))) %>%
    mutate(Deficit = nas/totals)

  out = list(def.item = def.item, def.tab = def.tab, def.parti = def.parti)
  invisible(out)
}




# EXCESS ----------------------

#' excess
#'
#' @param observations A tibble containing at least columns: participant, item, language, class, option, response. Response is 1 if participant has rated item as option in language, and 0 otherwise. It is assumed that all combinations of items and options are listed for each language.
#' @param NAString How is the NA option denoted in the data. Rows with NA will be excluded from all calculations.
#'
#' @return A list with the following tibbles:
#' exc.data --> Columns: language, class, item, option, R, P, U. Where: R: the number of classifiactions of item under option (represents a participant count), P: the total number of raters rating the item, U: R/P
#' exc.descr --> Columns: language, class, option, exc_v_max, exc_v_95, exc_v_90. From exc.data, aggregate the set of U values for each language x option combination. With the resulting sets calculate exc_v_max = 1 - max(U), exc_v_95 = 1 - quantile(U,0.95), exc_v_90 = 1 - quantile(U,0.9)
#' exc.per.participant --> Columns: participant, language, class, option, n, S, exc_V_pp. Where, n: the total number of ratings a particiant gave to the option (a representation of item counts), S: the total number of such ratings (the number of items), exc_V_pp = n/S
#' @export 
#'
#' @examples
excess <- function(observations,NAString = "None") {
  exc.data = observations %>% filter(option != NAString) %>% 
    group_by(language,class,item,option) %>% 
    # R: number of raters rating item under option/concept. 
    # P: total raters rating the item.
    summarise(R = sum(response), P = n(), U = R/P) %>% ungroup()

  # Collect the U values per option/concept. Calculate statistics for them (max and high quantiles)
  exc.descr = exc.data %>% group_by(language,class,option) %>%
    summarise(exc_v_max = 1 - max(U), 
              exc_v_q95 = 1 - quantile(U,0.95),
              exc_v_q90 = 1 - quantile(U,0.90))
  
  # Collect the number or ratings of each participant to each option.
  exc.per.participant = observations %>% group_by(participant,language,class,option) %>% 
    filter(option != NAString) %>% 
    summarise(n = sum(response)) %>% ungroup() %>% 
    # What is the maximum number of such ratings? Ans: the number of items (I can rate all items to one concept).
    inner_join(observations %>% group_by(participant,item) %>% summarise(0) %>% 
    group_by(participant) %>% summarise(S = n())) %>%
    mutate(exc_V_pp = 1 - n/S)
  
  out = list(exc.data = exc.data, exc.descr = exc.descr, exc.per.participant = exc.per.participant)
  invisible(out)
}

# OVERLAPPING ----------------------


## Overlap functions  ----------------------


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
    warning(paste("ov2: Sum of individual ratings more than total: ", n_s_r1,"+", n_s_r2,">",n_s))
  }
  if (n_s<2) {
    return (0)
  } else {
    return( (n_s_r1*n_s_r2)/ (ceiling(n_s/2)*floor(n_s/2)) )
  }
}




## Overlap calculations  ----------------------


#' overlaps.per.item
#'
#' Calculates the per-item overlap given a set of ratings
#' 
#' @param observations A tibble containing at least columns: participant, item, language, class, option, response. Response is 1 if participant has rated item as option in language, and 0 otherwise. It is assumed that all combinations of items and options are listed for each language.
#' @param l The language for which to produce the analysis.
#' @param conceptList List of concepts for each pair of which to calculate the overlap. 
#' @param overlapFunc A function f(n_1,n_2,n_t) that calculates the overlap value between two concepts wrt. to an item, the items having been classified n_1 and n_2 times under each concept, respectively, and having received n_t ratings in total.  
#' @param threshold A percentage of total ratings to a concept exceeding which makes the concept relevant for redundancy calculations. It is relevant when n_1 >= (threshold)x(n_t) OR n_2 >= (threshold)x(n_t)
#'
#' @return Returns a list with two tibbles:
#' data --> A tibble with columns item, concept1, concept2, overlap, each row measuring the overlap between concept1 and concept2 with respect to item.
#' tile --> As above, formatted to allow for tile visualisation (see blackOvTile and blueOvTile).
#' stats --> considering the list of all overlaps between concept1 and concept2 over several items that are relevant according to the relevance threshold, contains a table with statistics of the list.
#' 
#' @export
#'
overlaps.per.item <- function(observations, l, conceptList, overlapFunc = ov2, threshold = 0.1) {
  
  result = tribble(~item, ~concept1, ~concept2, ~overlap)
  redresult = tribble(~item, ~concept1, ~concept2, ~overlap)

  ov_ratings = observations %>% filter(language == l) %>% group_by(language,class,item,option) %>%
    summarise(n = sum(response)) %>% # n(.,s,v): total ratings (raters) classifying s under v.
    rename("concept" = option)  %>%
     left_join(observations %>% group_by(language,item) %>% summarise(n_s = sum(response))) # n(.,s,.): total ratings that s has received.
  
  # ov_ratings = ds1_ratings %>% filter(type == "concept")
  # conceptList = lang[["gmo"]][["concepts"]]
  # overlapFunc = ov2
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
        assert("\n overlaps.per.item: mutliple ratings for a signle combination of item and concept; should be only one. Review your ov_ratings file.",length(n_s_r1)==1)
        # Inner concept ratings for the item in question
        n_s_r2 = ov_ratings %>% filter(item == it,concept == r2) %>% pull(n)
        assert("\n overlaps.per.item: mutliple ratings for a signle combination of item and concept; should be only one. Review your ov_ratings file.",length(n_s_r2)==1)
        # Total ratings  for the item in question
        n_s = ov_ratings %>% filter(item == it,concept == r1) %>% pull(n_s)
        assert("\n overlaps.per.item: mutliple ratings for a signle combination of item and concept; should be only one. Review your ov_ratings file.",length(n_s)==1)
        
        # Add overlap to a results table - for reference
        result = result %>% add_row(item = it, 
                                    concept1 = r1, 
                                    concept2 = r2, 
                                    overlap = overlapFunc(n_s_r1,n_s_r2,n_s))
        
        # For redundancy calculations, add the record only if the item is relevant to one of the 
        # concepts.
        if ((n_s_r1 >= threshold*n_s) | (n_s_r2 >= threshold*n_s)) {
          redresult = redresult %>% 
            add_row(item = it, 
                    concept1 = r1, 
                    concept2 = r2, 
                    overlap = overlapFunc(n_s_r1,n_s_r2,n_s))
        }
      }
    }
  }
 
  # Each pair has as many ratings as the items.
  # Calculate various statistics
  stats = redresult %>% group_by(concept1,concept2) %>% 
    summarise(mean = mean(overlap),
              max = max(overlap),
              q90 = quantile(overlap,0.9),
              q75 = quantile(overlap,0.75),
              median = quantile(overlap,0.5),
              q25 = quantile(overlap,0.25),
              q10 = quantile(overlap,0.1), 
              min = min(overlap))
  
  stats.tile = rbind(stats,
                     stats %>% rename(concept1 = concept2, concept2 = concept1)) %>% 
    mutate(concept1 = factor(concept1,levels = conceptList),
           concept2 = factor(concept2,levels = conceptList))
  
  out = list(stats = stats,tile = stats.tile, data = result)
  print(stats)
  invisible(out)
}



#' overlaps.per.participant
#'
#' Calculates the per-participant overlap given a set of ratings
#' 
#' @param observations A tibble containing at least columns: participant, item, language, class, option, response. Response is 1 if participant has rated item as option in language, and 0 otherwise. It is assumed that all combinations of items and options are listed for each language.
#' @param l The language for which to produce the analysis.
#' @param conceptList List of concepts for each pair of which to calculate the overlap. 
#' @param overlapFunc A function f(n_1,n_2,n_t) that calculates the overlap value between two concepts wrt. to an item, the items having been classified n_1 and n_2 times under each concept, respectively, and having received n_t ratings in total.  
#' @return A list of the following tibbles:
#' means --> Columns: participant, r1, r2, pair, Obs, mean, sum, totalNZ, total, max, median, min. For each participant and pair of concepts there is a set of per-item overlaps(often sporadic). Particularly: Obs = mean, sum: the sum of overlaps, totalNZ: number of non-zero overlaps, total: max number of possible non-zero overlaps.
#' retuls --> Columns: participant, item, r1, r2, ovelaps. The raw data for the above: the overlap value between r1 and r2, for item by participant.
#' @export
#'
overlaps.per.participant <- function(observations,l,conceptList,overlapFunc = ov2) {
  result = tribble(~participant, ~item, ~r1, ~r2, ~overlap)
  
  ov_ratings = observations %>% filter(language == l) %>% rename("concept" = option, "n" = response) %>% 
    left_join(observations %>% group_by(participant,item) %>% summarise(n_s = sum(response)))
  
  pb <- txtProgressBar(min = 0, 
                       max = length(unique(ov_ratings %>% pull(participant)))*length(unique(ov_ratings %>% pull(item))), 
                       style = 3)
  count = 0
  
  #  ov_ratings = ov_ratings_parti %>% filter(type == "concept")
  #  pt = "s.eb49544e-dd27-4c0a-85f7-cf9157a9b2c6.txt"
  #  it = "descr1_item1_relationships"
  #  r1 = "Goal"
  #  r2 = "Belief"
  #  ov_ratings %>% ungroup() %>% filter(participant == pt,item==it)
    
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
                                      overlap = overlapFunc(resp[[1,c("n")]],resp[[2,c("n")]],resp[[1,c("n_s")]]))
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
                         mean = Obs,
                         sum = sum(overlap),
                         totalNZ = sum(overlap != 0),
                         total = n(),
                         max = max(overlap),
                         median = median(overlap),
                         min = min(overlap)
               ),
             data = result)
  invisible(out)
}




#' blackOvTile Prepares an overlap chart based on a dataset of overlaps.
#'
#' @param df The tile dataset with overlaps per item. Coming out of overlaps.per.item.
#' @param statistic The statistic to be used for the visualization output. Must be a separate column of the tile data set.
#' @param title Title of the overlap chart.
#'
#' @return A tile diagram presenting overlaps.
#' @export
#'
#' @examples
#' 
blackOvTile <- function(df, statistic = mean, title = ""){
  return(ggplot(df, aes(x = concept1,y = concept2,fill = {{statistic}})) +
           geom_tile() +scale_fill_gradient2(low="white", high="black",limits = c(0,1)) +
           geom_label(aes(x = concept1,y = concept2,label = round({{statistic}},2)),alpha = 0.5, fill = "white", color="black", size=rel(4)) +
           scale_x_discrete(position = "top") + 
           xlab("Concept 2") + ylab("Concept 1") +
           scale_y_discrete(limits = rev(levels(factor(df$concept2)))) +
           labs(fill = "Overlap") +
           ggtitle(title))
}


redundancy <- function(data) {
  out = rbind(data,
        data %>% rename(conceptX = concept1, concept1 = concept2) %>% rename(concept2 = conceptX)) %>% 
    group_by(concept1) %>% summarise(min = max(min),
                                     q10 = max(q10),
                                     q25 = max(q25),
                                     median = max(median)
                                     ) %>% rename (Concept = concept1)
  print(out)
  invisible(out)
}



#' blueOvTile Prepares an overlap chart based on a dataset of overlaps.
#'
#' @param df The tile dataset with overlaps per item. Coming out of overlaps.per.item.
#' @param statistic The statistic to be used for the visualization output. Must be a separate column of the tile data set.
#' @param title Title of the overlap chart.
#'
#' @return A tile diagram presenting overlaps.
#' @export
#'
#' @examples
#' 
blueOvTile <- function(df,statistic = mean, title = ""){
  return(ggplot(df, aes(x = concept1,y = concept2,fill = {{statistic}})) +
           geom_tile(colour="gray90", size=1.5, stat="identity") + 
           geom_text(aes(x = concept1,y = concept2, label = round({{statistic}},4)), color="black", size=rel(4.5)) +
           scale_fill_gradient(low = "white", high = "dodgerblue", space = "Lab", 
                               na.value = "gray90", guide = "colourbar",limits = c(0,1)) +
           scale_x_discrete(expand = c(0, 0),position = "top") +
           scale_y_discrete(expand = c(0, 0),limits = rev(levels(factor(df$concept2)))) +
           xlab("Concept 2") + ylab("Concept 1") +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill="gray90"),
                 plot.background = element_rect(fill="gray90"),
                 legend.position = "none",         
                 axis.text = element_text(color="black", size=11),
                 axis.title=element_text(size=12,face="bold")
           ) + ggtitle(title))
}











# REPORTING --------------------

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




