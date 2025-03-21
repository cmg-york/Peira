# Libraries --------------------------------------------
source("PackageLoad.R")
source("Library.R")
source("DataLoader.R")


# Preliminary --------------------------------------------
print_preliminary <- function(langNum, 
                              entity_definition_sample, 
                              entity_examples_sample,
                              relationship_definition_sample,
                              relationship_example_sample){
  
  cat("scale: ",paste0(langNum,"-entities\n"))
  print_check_inventory_text(langNum,"entities")
  
  cat("scale: ",paste0(langNum,"-relationships\n"))
  print_check_inventory_text(langNum,"relationships")

  cat("scale: ",paste0(langNum,"-entities-test\n"))
  print_check_inventory_text(langNum,"entities",FALSE)
  
  cat("scale: ",paste0(langNum,"-relationships-test\n"))
  print_check_inventory_text(langNum,"relationships",FALSE)

  cat("l: intro_descr\n")
  cat("t: youtube\n")
  cat("q: <h2>Overview</h2> Thank you again for agreeing to participate. Please watch the following short video with an overview of this experiment. If embedded video does not load, you can  <a href='",getYoutubeURL(getConfig("intro")),"' target=\"_blank\">watch it directly on Youtube</a> \n")
  cat(paste0("- ",getConfig("intro"),"\n\n"))
  
  cat("l: vid1\n")
  cat("t: youtube\n")
  cat(paste0("q: <h2>",getLangName(langNum),"</h2>To start please watch the following video presentation about <b>",getLangName(langNum),"</b>. If the embedded video does not load, you can  <a href='",getYoutubeURL(getLanguageVideoCode(langNum)),"' target=\"_blank\">watch it directly on Youtube</a> \n"))
  cat(paste0("- ",getLanguageVideoCode(langNum),"\n\n"))
  
  cat("page: begin\n\n")
  cat("l: conceptUnderstandingInfo\n")
  cat("t: info\n")
  cat(paste0('q: <h2>Comprehension Questions</h2> Based on what you learned in the video, answer the following questions. You can always <a href=\"',getYoutubeURL(getLanguageVideoCode(langNum)),'" target=\"_blank\">watch the video again</a> (opens in new window) or refer to <a href=\"',getLangCheatSheetURL(langNum),'\" target=\"popup\" onclick=\"window.open(\'',getLangCheatSheetURL(langNum),'\',\'popup\',\'width=600,height=400\');return false;"> this short "cheat-sheet"</a>  (pops-up a window).\n\n'))
  
  
  cat("l: entityUnderstandingDefn\n")  
  cat("t: scale ",paste0(langNum,"-entities-test\n"))
  if (as.logical(getConfig("training_shuffle"))) {
    cat("o: random\n")  
  }
  cat("q: Please match each of the following definitions with the entity it defines.\n")
  for (t in getConceptDefinitions_Entities(langNum)[getTrainingSampleDefinitions_Entities(langNum)]) {
    cat(paste0("- \"", t,"\" \n"))
  }
  cat("\n")
  
  cat("l: entityUnderstandingExample\n")  
  cat("t: scale ",paste0(langNum,"-entities-test\n"))
  if (as.logical(getConfig("training_shuffle"))) {
    cat("o: random\n")  
  }
  cat("q: Please match each of the following examples with an entity it may exemplifies. There may be many correct answers. \n")
  for (t in getConceptExamples_Entities(langNum)[getTrainingSampleExamples_Entities(langNum)]) {
    cat(paste0("- <i>\"", t,"\"</i> \n"))
  }
  cat("\n")
  
  
  cat("l: relationshipUnderstandingDefn\n")  
  cat("t: scale ",paste0(langNum,"-relationships-test\n"))
  if (as.logical(getConfig("training_shuffle"))) {
    cat("o: random\n")  
  }
  cat("q: Please match each of the following definitions with the relationship it defines.\n")
  for (t in getConceptDefinitions_Relationships(langNum)[getTrainingSampleDefinitions_Relationships(langNum)]) {
    cat(paste0("- \"", t,"\" \n"))
  }
  cat("\n")
  

  cat("l: relationshipUnderstandingExample\n")  
  cat("t: scale ",paste0(langNum,"-relationships-test\n"))
  if (as.logical(getConfig("training_shuffle"))) {
    cat("o: random\n")  
  }
  cat("q: Please match each of the following examples with the relationship it exemplifies.\n")
  for (t in getConceptExamples_Relationships(langNum)[getTrainingSampleExamples_Relationships(langNum)]) {
    cat("- ", t," \n")
  }
  cat("\n")
  
  cat("page: end\n\n")
  
}

print_observational_check <- function(langID, caseID, key){
  
  cat("\n\n# # # # # # # # # # # # # # # # #\n")
  cat("#   O B S E R V A T I O N A L   # \n")
  cat("# # # # # # # # # # # # # # # # #\n\n")
  
  
  
  cat("page: begin\n\n")
  
  cat(paste0("l: directions_",caseID,"\n"))
  cat("t: youtube\n")
  cat(paste0("q: ", getClassificationHeader(caseID)," \n"))
  cat(paste0("- ",getCaseVideoCode(caseID),"\n\n"))
  
  cat(paste0("l: description_",caseID,"\n"))
  cat("t: info\n")
  cat("q: ",getCaseText(caseID),"\n\n")

  
  cat(paste0("l: entities_",caseID,"\n"))
  cat("t: info\n")
  cat(paste0('q: <h4>Entities</h4> Based on what you learned in the video and the information in the above passage, classify each of the following expressions to <b>one or more entities</b> that best describe it. You can always <a href=\"',getYoutubeURL(getLanguageVideoCode(langID)),'" target=\"_blank\">watch the vocabulary presentation video again</a> (opens in new window) or refer to <a href=\"',getLangCheatSheetURL_Entities(langID),'\" target=\"popup\" onclick=\"window.open(\'', getLangCheatSheetURL_Entities(langID),'\',\'popup\',\'width=600,height=400\');return false;"> this short "cheat-sheet"</a>  (pops-up a window). Please check <b>None of the above</b> if you think that none of the listed concepts describes the expression well. \n\n'))
  
  #cat("random: begin\n\n")
  
  
  for (i in (1:nrow(getCaseItems_Entities(caseID,langID)))) {
    cat(paste0("l: ",langID,"_ent_",caseID,"_",i,"\n"))
    cat(paste0("t: check\n"))
    cat(paste0("o: require 1\n"))
    cat(paste0('q: <i>"',getCaseItems_Entities(caseID,langID)[i,"item_text"], '"</i>\n'))
    #print_check_inventory(language,"concepts",lang)
    cList = print_check_inventory(caseID,i,langID,"entity",image = TRUE,trailing = TRUE)
    cat("")

    cset = c(cList %>% pull(conceptID),"None")
    for (j in 1:(length(cset))) {
      key = key %>% add_row(psy_item = paste0(langID,"_ent_",caseID,"_",i,"_",j),
                            item = paste0(caseID,"_item",i,"_entities"), 
                            item_full = getCaseItems_Entities(caseID,langID)[[i,"item_text"]],
                            description = caseID, 
                            language = langID, 
                            type = "entity",
                            psy_option = cset[j],
                            auth = getItemAuthClassification(caseID,i,langID,type))
    }
  }

  #cat("random: end\n\n")
  
  
  
  cat(paste0("l: relationships_",caseID,"\n"))
  cat("t: info\n")
  cat(paste0('q: <h4>Relationships</h4> Based on what you learned in the video and the information in the above passage, classify each of the following expressions to <b>one or more relationships</b> that best describe it. You can always <a href=\"',getYoutubeURL(getLanguageVideoCode(langID)),'" target=\"_blank\">watch the vocabulary presentation video again</a> (opens in new window) or refer to <a href=\"',getLangCheatSheetURL_Relationships(langID),'\" target=\"popup\" onclick=\"window.open(\'', getLangCheatSheetURL_Relationships(langID),'\',\'popup\',\'width=600,height=400\');return false;"> this short "cheat-sheet"</a>  (pops-up a window). Please check <b>None of the above</b> if you think that none of the listed relationships describes the expression well.\n\n'))
  
  #cat("random: begin\n\n")
  
  
  
  for (i in (1:nrow(getCaseItems_Relationships(caseID,langID)))) {
    cat(paste0("l: ",langID,"_rel_",caseID,"_",i,"\n"))
    cat(paste0("t: check\n"))
    cat(paste0("o: require 1\n"))
    cat(paste0('q: <i>"',getCaseItems_Relationships(caseID,langID)[i,"item_text"], '"</i>\n'))
    #print_check_inventory(language,"concepts",lang)
    cList = print_check_inventory(caseID,i,langID,"relationship",image = TRUE,trailing = TRUE)
    cat("")
    
    cset = c(cList %>% pull(conceptID),"None")
    for (j in 1:(length(cset))) {
      key = key %>% add_row(psy_item = paste0(langID,"_rel_",caseID,"_",i,"_",j),
                            item = paste0(caseID,"_item",i,"_relationships"), 
                            item_full = getCaseItems_Relationships(caseID,langID)[[i,"item_text"]],
                            description = caseID, 
                            language = langID, 
                            type = "relationship",
                            psy_option = cset[j],
                            auth = getItemAuthClassification(caseID,i,langID,type))
    }
  }
  
  
  #cat("random: end\n\n")
  cat("page: end\n\n")
  
  return(key)
}

print_overlap <- function(langNum, key_att){

  # Overlap ---------
  
  cat("page: begin\n\n")
  
#  example = paste0(
#    "(", paste(lang[[language]][["concepts"]][c(1,2)],collapse =","),"), ",
#    "(", paste(lang[[language]][["concepts"]][c(2,3)],collapse =","),"), ",
#    "(", paste(lang[[language]][["relationships"]][c(1,2)],collapse =","),")"
#  )
  
  cat("l: overlap_dir\n")
  cat("t: youtube\n")
  cat(paste0("q: <h2>Overlap</h2>Please watch the following video with directions about this page. If embedded video does not load, you can  <a href='",getURL(getConfig("overlap")),"' target=\"_blank\">watch it directly on Youtube</a>\n"))
  cat(paste0("- ",getConfig("overlap")),"\n\n")
    
  cat("l: conceptOverlap_ents\n")
  cat("t: range\n")
  cat('q: <h3>Overlap Assessment - Entities</h3> Consider <b>pairs</b> of the modeling <b>entities</b> discussed earlier: <b>',getLangExamples_Entities(langNum),'</b> etc. For each pair, rate how much you think the two entities in the pair <b>overlap</b>, i.e., refer to the same thing. Choose any level from 0 to 10, between <b>No Overlap</b> (0, the entities in the pair are completely distinct) and <b>Complete Overlap</b> (10, the entities in the pair refer to the same thing in different words).')
  cat('You can review the language again by referring to <a href=\"',getURL(getLanguageVideoCode(langNum)),'\" target=\"_blank\">the previous video</a> (opens in new window) or to <a href=\"',getLangCheatSheetURL_Entities(langNum),'\" target=\"popup\" onclick=\"window.open(\'',getLangCheatSheetURL_Entities(langNum),"','popup','width=600,height=400');return false;\"> the cheat-sheet</a> (pops-up a window).\n")
  
  k = 1
  c_list = getConceptList_Entities_Overlap(langNum)
  
  
  for (i in 1:(length(c_list)-1)) {
    for (j in (i+1):length(c_list)){
      cat("- {min=0,max=10,left=No Overlap,right=Complete Overlap,start=0} <u><b>",c_list[i],"</b></u> and <u><b>",c_list[j],"</b></u>\n")
      #cat(c_list[i],"-",c_list[j],"\n")
      key_att = key_att %>% add_row(psy_item = paste0("conceptOverlap_ents_",k),
                                    language = langNum,
                                    conceptID =  paste0(c_list[i]," -[vs]- ",c_list[j]),
                                    concept = paste0(c_list[i]," -[vs]- ",c_list[j]))
      k = k + 1
    }
  }
  
  
  k = 1
  cat("\n\n l: conceptOverlap_rels\n")
  cat("t: range\n")
  cat('q: <h3>Overlap Assessment - Relationships</h3> Consider <b>pairs</b> of the modeling <b>relationships</b> discussed earlier: <b>',getLangExamples_Relationships(langNum),'</b> etc. For each pair, rate how much you think the two relationships in the pair <b>overlap</b>, i.e., refer to the same thing. Choose any level from 0 to 10, between <b>No Overlap</b> (0, the relationships in the pair are completely distinct) and <b>Complete Overlap</b> (10, the relationships in the pair refer to the same thing in different words).')
  cat('You can review the language again by referring to <a href=\"',getURL(getLanguageVideoCode(langNum)),'\" target=\"_blank\">the previous video</a> (opens in new window) or to <a href=\"',getLangCheatSheetURL_Relationships(langNum),'\" target=\"popup\" onclick=\"window.open(\'',getLangCheatSheetURL_Relationships(langNum),"','popup','width=600,height=400');return false;\"> the cheat-sheet</a> (pops-up a window).\n")
  
  r_list = getConceptList_Relationships_Overlap(langNum)
  for (i in 1:(length(r_list)-1)) {
    for (j in (i+1):length(r_list)) {
      cat("- {min=0,max=10,left=No Overlap,right=Complete Overlap,start=0} <u><b>",r_list[i],"</b></u> and <u><b>",r_list[j],"</b></u>\n")
      #cat(r_list[i],"-",r_list[j],"\n")
      key_att = key_att %>% add_row(psy_item = paste0("conceptOverlap_rels_",k),
                                    language = langNum,
                                    conceptID = paste0(r_list[i]," -[vs]- ",r_list[j]),
                                    concept = paste0(r_list[i]," -[vs]- ",r_list[j]))
      k = k + 1
    }
  }
  
  cat("\n page: end\n\n")
  
  return(key_att)
}

print_self_reporting <- function(language, key_att){
  cat("page: begin\n\n")

# Relevance ---------
  
  cat("l: relevanceDescr\n")
  cat("t: youtube\n")
  cat("q: <h2>Relevance</h2>Please watch the following video with directions about this page.If embedded video does not load, you can  <a href='",getYoutubeURL(getConfig("relevance")),"' target=\"_blank\">watch it directly on Youtube</a>\n")
  cat(paste0("- ",getConfig("relevance"), "\n\n"))
  
  s_list = getCaseTitles()
  s_list_summary = paste0(s_list,collapse = ", ")
  casez = ifelse(length(s_list)==1, "case", paste0(english(length(s_list))," cases"))
  
  e_list = getConceptTable_Entities("iRL",concepts_df)$conceptDescr
  e_list_code = getConceptTable_Entities("iRL",concepts_df)$conceptID
  
  # e_list = getConceptList_Entities(langNum)
  # e_list_code = getConceptCodeList_Entities(langNum,concepts_df)
  e_list_summary = paste0(paste0(e_list[c(1,2,3)],collapse = ", "), ", etc.")
  
  cat("l: entityRelevance\n")
  cat("t: range\n")
  cat('q: <h3>Entity Relevance</h3> Consider the modeling <b>entities</b> discussed earlier (',e_list_summary,'). Consider also the ',casez,' you studied (',s_list_summary,'). Rate how <b>relevant</b> you find each entity to what is presented in the ',casez,'. You may review again the ',casez,' <a href=\"',getConfig("cases_url"),'\" target=\"_blank\">here</a> (opens in new window). You may review the vocabulary again by referring to <a href=\"',getYoutubeURL(getLanguageVideoCode(langNum)),'\" target=\"_blank\">the vocabulary presentation video</a> (opens in new window) or to <a href=\"',getLangCheatSheetURL_Entities(langNum),'\" target=\"popup\" onclick=\"window.open(\'',getLangCheatSheetURL_Entities(langNum),"','popup','width=600,height=400');return false;\"> the cheat-sheet</a> (pops-up a window).\n", sep="")
  
  for(i in 1:length(e_list)) {
    cat("- {min=0,max=10,left=Not at all relevant,right=Very relevant,start=5} Relevance of entity <u><b>",c_list[i],"</b></u> in the case(s)\n")
    key_att = key_att %>% add_row(psy_item = paste0("entityRelevance_",i), 
                                  language = language,
                                  conceptID = e_list_code[i],
                                  concept = e_list[i])
  }
  cat("\n")
  
  
  r_list = getConceptTable_Relationships("iRL",concepts_df)$conceptDescr
  r_list_code = getConceptTable_Relationships("iRL",concepts_df)$conceptID
  r_list_summary = paste0(paste0(r_list[c(1,2,3)],collapse = ", "), ", etc.")
  
  
  
  cat("l: relationshipRelevance\n")
  cat("t: range\n")
  cat('q: <h3>Relationship Relevance</h3>Consider now the modeling <b>relationships</b> discussed earlier (',r_list_summary,'). Consider also the ',casez,' you studied (',s_list_summary,'). Rate how <b>relevant</b> you find each of the relationships to what is presented in the ',casez,'. You may review again the ',casez,' <a href=\"',getConfig("cases_url"),'\" target=\"_blank\">here</a> (opens in new window). You may review the vocabulary again by referring to <a href=\"',getURL(getLanguageVideoCode(langNum)),'\" target=\"_blank\">the vocabulary presentation video</a> (opens in new window) or to <a href=\"',getLangCheatSheetURL_Relationships(langNum),'\" target=\"popup\" onclick=\"window.open(\'',getLangCheatSheetURL_Relationships(langNum),"','popup','width=600,height=400');return false;\"> the cheat-sheet</a> (pops-up a window).\n",sep="")
  for(i in 1:length(r_list)) {
    cat("- {min=0,max=10,left=Not at all relevant,right=Very relevant,start=5} Relevance of relationship <u><b>",r_list[i],"</b></u>\n")
    key_att = key_att %>% add_row(psy_item = paste0("relationshipRelevance_",i),
                                  language = language,
                                  conceptID = r_list_code[i],
                                  concept = r_list[i])
  }
  cat("\n")
  
  cat("page: end\n\n")
  
  # Completeness ---------
  cat("page: begin\n\n")
  
  cat("l: completenessDescr\n")
  cat("t: youtube\n")
  cat("q: <h2>Completeness</h2>Please watch the following video with directions about this page.If embedded video does not load, you can  <a href='",getYoutubeURL(getConfig("completeness")),"' target=\"_blank\">watch it directly on Youtube</a>\n")
  cat(paste0("- ",getConfig("completeness"), "\n\n"))
  
  cat("l: entityCompleteness\n")
  cat("t: range\n")
  cat('q: <h3>Concept Completeness</h3>Consider again the ',casez,' you studied (',s_list_summary,') and the items in the ',casez,' that you were asked to classify. Consider also the modeling <b>entites</b> that you used (',e_list_summary,'). Do you agree with the following statement: <BR>\n',sep="")
  cat('<p style="text-align:center"><i>The set of concepts included in the vocabulary (<b>',paste(c_list,collapse = ", "),'</b>) were <b>sufficient</b> for characterizing relevant parts of the described cases. <b>No more entities need to be added</b> to the set to make it more complete.</i></p>\n',sep="")
  cat("- {min=-7,max=+7,left=Strongly Disagree,right=Strongly Agree,start=0}\n")
  cat("\n")
  key_att = key_att %>% add_row(psy_item = "entityCompleteness_1", 
                                language = langNum,
                                conceptID = langNum,
                                concept = langNum)
  
  
  cat("l: relationshipCompleteness\n")
  cat("t: range\n")
  cat('q: <h3>Relationship Completeness</h3>Consider again the ',casez,' you studied (',s_list_summary,') and the items in the ',casez,' that you were asked to classify. Consider also the modeling <b>relationships</b> that you used (',r_list_summary,'). Do you agree with the following statement: <BR>\n',sep="")
  cat('<p style="text-align:center"><i>The set of relationships included in the vocabulary (<b>',paste(r_list,collapse = ", "),'</b>) were <b>sufficient</b> for characterizing relevant parts in the described cases.  <b>No more relationships need to be added</b> to the set to make it more complete.</i>\n',sep="")
  cat("- {min=-7,max=+7,left=Strongly Disagree,right=Strongly Agree,start=0}\n")
  cat("\n")
  key_att = key_att %>% add_row(psy_item = "relationshipCompleteness_1", 
                                language = langNum,
                                conceptID = langNum,
                                concept = langNum)
  
  
  cat("page: end\n\n")

  
  return(key_att)
}

demographics <- function(final) {
  
  cat("\n\n# # # # # # #\n")
  cat("# D E M O S #\n")
  cat("# # # # # # # \n\n")
  
  
  cat("page: begin\n\n")
  
  cat("l: closing\n")
  cat("t: info\n")
  if (final) {
    cat("q: <h2>Demographics</h2> Almost done! We only have a last few demographic type of questions.\n\n")
  } else {
    cat("q: <h2>Demographics</h2> Please tell us a few things about yourself.\n\n")
  }
  
  cat("l: Age\n")
  cat("t: textline\n")
  cat("q: <h3>Age</h3> What is your age?\n")
  cat("- {min=0,max=120} Type your age (must be numerical)\n\n")
  
  cat("l: Sex\n")
  cat("t: radio\n")
  cat("q: <h3>Sex</h3> Are you:\n")
  cat("- Female.\n")
  cat("- Male.\n")
  cat("- Other.\n")
  cat("- Rather not say.\n\n")
  
  
  cat("l: Fl\n")
  cat("t: check\n")
  cat("o: require 1 2\n")
  cat("q: <h3>Field</h3> Which of the following best describes your field in terms of formal education? Select the one (1) or at most two (2) most relevant.\n")
  cat("- Science, Technology and Engineering.\n")
  cat("- Business and Economics.\n")
  cat("- Health Sciences.\n")
  cat("- Social Sciences and Humanities (e.g. Sociology, English, History etc.).\n")
  cat("- Fine Arts.\n")
  cat("- Education.\n")
  cat("- Other/None.\n\n")
  
  
  cat("l: englishFirst\n")
  cat("t: radio\n")
  cat("q: <h3>Language Ability (1/2)</h3> Are you a native speaker of <b>English</b>:\n")
  cat("- Yes\n")
  cat("- No\n")
  cat("- Partially\n\n")
  
  cat("scale: english_scale\n")
  cat("- Fair\n")
  cat("- Good\n")
  cat("- Very Good\n")
  cat("- Excellent\n\n")
  
  cat("l: english\n")
  cat("t: scale english_scale\n")
  cat("q: <h3>Language Ability (2/2)</h3> Please rate your ability in <b>English</b> with respect to the following activities:\n")
  cat("- Reading\n")
  cat("- Writing\n")
  cat("- Listening\n")
  cat("- Speaking\n\n")
  
  cat("page: end\n\n")
  
}  

mturk <- function(count){
  cat("# # # # # # # # #\n")
  cat("# M - T U R K   # \n")
  cat("# # # # # # # # #\n\n")
  cat(paste0("l: mturkID",count,"\n"))
  cat("t: textline\n")
  if (count == 1) {
    cat("q: <h2>MTurk ID</h2>Thank you for agreeing to participate! What is your Mechanical Turk participant ID?\n")
  } else {
  cat("q: <h2>Done!</h2>Thank you for participating! Just in case, can you please enter your Mechanical Turk participant ID again?\n")
  }
  cat("- {require} Participant ID:\n\n") 
}

prolific <- function(count){
  cat("# # # # # # # # # # # #\n")
  cat("#   P R O L I F I C   # \n")
  cat("# # # # # # # # # # # #\n\n")
  cat(paste0("l: mturkID",count,"\n"))
  cat("t: textline\n")
  if (count == 1) {
    cat("q: <h2>Participant ID</h2>Thank you for agreeing to participate! What is your Prolific participant ID?\n")
  } else {
    cat("q: <h2>Done!</h2>Thank you for participating! Just in case, can you please enter your Prolific participant ID again?\n")
  }
  cat("- {require} Participant ID:\n\n") 
}

pilot <- function(final) {
  
  cat("\n\n# # # # # # #\n")
  cat("# P I L O T #\n")
  cat("# # # # # # # \n\n")
  
  cat("page: begin\n\n")
  
  cat("l: feedback\n")
  cat("t: info\n")
  if(final) {
    cat("q: <h2>Feedback</h2> Almost done! In order to help us make this instrument better, can you provide us with your opinion about it?\n\n")
  } else {
    cat("q: <h2>Feedback</h2> In order to help us make this instrument better, can you provide us with your opinion about it?\n\n")
  }
  
  cat("l: system\n")
  cat("t: radio\n")
  cat("q: <h3>Device Type</h3> What type of device are you using to complete this study? \n")
  cat("- A mobile phone\n")
  cat("- A tablet \n")
  cat("- A desktop or laptop computer\n")
  cat("- Other\n\n")
  
    
  cat("l: instrumentClarity\n")
  cat("t: radio\n")
  cat("q: <h3>Clarity</h3> How clear (easy to understand) was the material and questions you were presented?\n")
  cat("- Very clear\n")
  cat("- Unclear at places\n")
  cat("- Very unclear\n\n")
  

  cat("l: instrumentTime\n")
  cat("t: radio\n")
  cat("q: <h3>Time</h3> Does the time initially indicated reflects how long it took you to complete the tasks? \n")
  cat("- Yes, the time was actually more than enough.\n")
  cat("- Yes, the time is just about right. \n")
  cat("- No, it took me more time than I was expecting. \n\n")
  
  cat("l: instrumentIssues\n")
  cat("t: textbox\n")
  cat("q: <h3>Feedback</h3> Are there any parts of the tasks you performed that needs improvement in terms of, e.g., clarity, ease of reading and understanding, time?\n")
  cat("- {w=50,h=3} Comments:\n\n")
  
  cat("page: end\n\n")
  
}



