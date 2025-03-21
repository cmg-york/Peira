print_check_inventory_text <- function(langNum,type,trailing = TRUE){
  
  if (type == "entities") {
    c_list = getConceptList_Entities(langNum)
  } else {
    c_list = getConceptList_Relationships(langNum)
  }

  for (t in c_list) {
    cat(paste0("- ",t,"\n"))
  }
  if (trailing == TRUE){
    cat("- None of the above\n\n")
  } else {
    cat("\n\n")
  }
}


print_getImgTag <- function(langID, caseID, itemID, conceptID, conceptDescr, conceptType){
  typ = substr(conceptType, 1, 3)
  fileName = paste0(langID,"-",caseID,"-",typ,"-",itemID,"-",conceptID,".png")
  
  text = paste0('<img src="',getConfig("img_url"),fileName,'" style="height:100px" title="',conceptDescr,'">')
  return (text)
}


print_check_inventory <- function(caseNum,itemNum,langNum,type,image = FALSE,trailing = TRUE){
  conceptList = concepts_df %>% filter(langID == langNum, conceptType == type) %>% select(conceptID,conceptDescr)
  for (t in (1:nrow(conceptList))){
    if (image) {
      text = print_getImgTag(langNum,caseNum,itemNum,conceptList[t,"conceptID"],conceptList[t,"conceptDescr"],type)
      #
      # F I X  T H I S !!!!
      #
      #text = print_getImgTag(langNum,caseNum,1,conceptList[t,"conceptID"],conceptList[t,"conceptDescr"])
      #                                 !!! ^ !!!
      #
      #
      cat(paste0("- ",text,"\n"))
    } else {
      cat(paste0("- ",conceptList[t,"conceptDescr"],"\n"))  
    }
  }
  if (trailing == TRUE){
    cat("- None of the above\n\n")
  } else {
    cat("\n\n")
  }
  return (conceptList)
}




#
#
# C O N F I G U R A T I O N
#
#


getConfig <- function(item){
  config %>% filter(name==item) %>% pull(value)
}

getYoutubeURL <- function(code){
  return(paste0("https://www.youtube.com/watch?v=",code))
}


#
# Case-related functions
#

getCaseHeader <- function(caseNum){
  str = cases_df %>% filter(caseID==caseNum) %>% pull(caseHeader)
  return(eval(parse(text = str)))
}

getCaseText <- function(caseNum){
  return(cases_df %>% filter(caseID==caseNum) %>% pull(caseText))
}

getCaseTitle <- function(caseNum){
  return(cases_df %>% filter(caseID==caseNum) %>% pull(caseTitle))
}

getCaseTitles <- function(includedOnly = TRUE){
  if (includedOnly) {
    return(cases_df %>% filter(include == TRUE) %>% pull(caseTitle))
  } else {
    return(cases_df %>% pull(caseTitle))
  }
}

getCaseVideoCode <- function(caseNum){
  return(cases_df %>% filter(caseID==caseNum) %>% pull(vCode))
}

getClassificationHeader <- function(caseNum){
  str = cases_df %>% filter(caseID==caseNum) %>% pull(classifHeader)
  return(eval(parse(text = str)))
}

getCaseItems_Entities <- function(caseNum,langNum){
  return(items_df %>% filter(caseID==caseNum, type == "entity", include == TRUE) %>% select(itemID,order,
                                                                    item_text,
                                                                    paste0(langNum,"_auth")))
}
getCaseItems_Relationships <- function(caseNum,langNum){
  return(items_df %>% filter(caseID==caseNum, type == "relationship", include == TRUE) %>% select(itemID,order,
                                                                           item_text,
                                                                           paste0(langNum,"_auth")))
}

getItemAuthClassification <- function(caseNum,itemNum,langNum,cType) {
  return(items_df %>% filter(caseID == caseNum,itemID == itemNum,type == cType) %>% pull(paste0(langNum,"_auth")))
}

#
# Language- and Concept-related functions
#


getLanguageVideoCode <- function(langNum){
  return(lang_df %>% filter(langID==langNum) %>% pull(vCode))
}

getLangName <- function(langID){
  lang_names = setNames(unique(lang_df[c("langID", "langDescr")])$langDescr, 
                        unique(lang_df[c("langID", "langDescr")])$langID)
  return(lang_names[[langID]])
}

getLangCodeList <- function(){
  return (unique(lang_df$langID))
}

getLangCheatSheetURL <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(urlAll))
}

getLangCheatSheetURL_Entities <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(urlEntities))
}

getLangCheatSheetURL_Relationships <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(urlRelationships))
}

getLangExamples <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(examples))
}

getLangExamples_Entities <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(ent_examples))
}

getLangExamples_Relationships <- function(langNum) {
  return(lang_df %>% filter(langID==langNum) %>% pull(rel_examples))
}

getConceptList_Entities <- function(langNum){
  return(concepts_df %>% filter(langID == langNum,conceptType == "entity") %>% pull(conceptDescr))
}

getConceptList_Relationships <- function(langNum){
  return(concepts_df %>% filter(langID == langNum,conceptType == "relationship") %>% pull(conceptDescr))
}

getConceptCodeList_Entities <- function(langNum, concepts_df){
  return(concepts_df %>% filter(langID == langNum,conceptType == "entity") %>% pull(conceptID))
}

getConceptCodeList_Relationships <- function(langNum, concepts_df){
  return(concepts_df %>% filter(langID == langNum,conceptType == "relationship") %>% pull(conceptID))
}

getConceptTable_Entities  <- function(langNum, concepts_df){
  return(concepts_df %>% filter(langID == langNum,conceptType == "entity") %>% select(conceptID,conceptDescr))
}

getConceptTable_Relationships  <- function(langNum, concepts_df){
  return(concepts_df %>% filter(langID == langNum,conceptType == "relationship") %>% select(conceptID,conceptDescr))
}



getConceptList_Entities_Overlap <- function(langNum){
  return(concepts_df %>% filter(langID == langNum,conceptType == "entity-overlap") %>% pull(conceptDescr))
}

getConceptList_Relationships_Overlap <- function(langNum){
  return(concepts_df %>% filter(langID == langNum,conceptType == "relationship-overlap") %>% pull(conceptDescr))
}




# For training

getTrainingSampleDefinitions_Entities <- function(langNum) {
  vec1 <- as.numeric(unlist(strsplit(lang_df$ent_training_defn, ";")))
  vec2 = vec1[!is.na(vec1)]
  return(vec2)
}

getTrainingSampleDefinitions_Relationships <- function(langNum) {
  vec1 <- as.numeric(unlist(strsplit(lang_df$rel_training_defn, ";")))
  vec2 = vec1[!is.na(vec1)]
  return(vec2)
}

getTrainingSampleExamples_Entities <- function(langNum) {
  vec1 <- as.numeric(unlist(strsplit(lang_df$ent_training_examples, ";")))
  vec2 = vec1[!is.na(vec1)]
  return(vec2)
}

getTrainingSampleExamples_Relationships <- function(langNum) {
  vec1 <- as.numeric(unlist(strsplit(lang_df$rel_training_examples, ";")))
  vec2 = vec1[!is.na(vec1)]
  return(vec2)
}





getConceptDefinitions_Entities <- function(langNum){
  return(concepts_df %>% filter(langID == langNum, conceptType == "entity") %>% pull(definition))
}

getConceptExamples_Entities <- function(langNum){
  #return(concepts_df %>% filter(langID == langNum, conceptType == "entity") %>% pull(trainingExample))
  
  return( paste0("<i>",concepts_df %>% 
            filter(langID == langNum, conceptType == "entity") %>% 
            select(trainingExample,exampleIsImage,conceptDescr) %>% 
            mutate(trainingExample = if_else(exampleIsImage,paste0('<img src="',getConfig("trainingImg_url"),ex_df$trainingExample,'" style="height:100px" title="',ex_df$conceptDescr,'">'),trainingExample)) %>% pull(trainingExample),"</i>") )
  
  
}

getConceptDefinitions_Relationships <- function(langNum){
  return(concepts_df %>% filter(langID == langNum, conceptType == "relationship") %>% pull(definition))
}

getConceptExamples_Relationships <- function(langNum){
  #return(concepts_df %>% filter(langID == langNum, conceptType == "relationship") %>% pull(trainingExample))
  
  return( concepts_df %>% 
    filter(langID == langNum, conceptType == "relationship") %>% 
    select(trainingExample,exampleIsImage,conceptDescr) %>% 
    mutate(trainingExample = if_else(exampleIsImage,paste0('<img src="',getConfig("trainingImg_url"),ex_df$trainingExample,'" style="height:100px" title="',ex_df$conceptDescr,'">'),paste0("<i>",trainingExample,"</i>"))) %>% pull(trainingExample))
  
}




#
# Construct Language lists and structures
#


#entity_names = NULL
#relationship_names = NULL
#summary = NULL

#for (l in getLangCodeList()) {

    #entity_names[[l]] = concepts_df %>% filter(langID == l,conceptType == "entity") %>% pull(conceptDescr)
  
    #relationship_names[[l]] = concepts_df %>% filter(langID == l,conceptType == "relationship") %>% pull(conceptDescr)
  
    #summary[[l]] = paste0("(",paste0(entity_names[[l]][c(1,2)], collapse=", "),", ",relationship_names[[l]][c(1)],", etc.)")

#}


#
# NOT SURE WHAT THESE ARE
#


# concept_elements_desc_1 = items %>% filter(type == "concept",description ==1) %>% arrange(order)
# concept_elements_1 = concept_elements_desc_1 %>% pull(item_text)
# concept_elements_auth_1 = list(gmo = concept_elements_desc_1 %>% pull(gmo_auth_descr),
#                                gme = concept_elements_desc_1 %>% pull(gme_auth_descr))
# 
# relationship_elements_desc_1 = items %>% filter(type == "relationship",description ==1) %>% arrange(order)
# relationship_elements_1 = relationship_elements_desc_1 %>% pull(item_text)
# relationship_elements_auth_1 = list(gmo = relationship_elements_desc_1 %>% pull(gmo_auth_descr),
#                                     gme = relationship_elements_desc_1 %>% pull(gme_auth_descr))
# 
# 
# concept_elements_desc_2 = items %>% filter(type == "concept",description == 2) %>% arrange(order)
# concept_elements_2 = concept_elements_desc_2 %>% pull(item_text)
# concept_elements_auth_2 = list(gmo = concept_elements_desc_2 %>% pull(gmo_auth_descr),
#                                gme = concept_elements_desc_2 %>% pull(gme_auth_descr))
# 
# relationship_elements_desc_2 = items %>% filter(type == "relationship",description ==2) %>% arrange(order)
# relationship_elements_2 = relationship_elements_desc_2 %>% pull(item_text)
# relationship_elements_auth_2 = list(gmo = relationship_elements_desc_2 %>% pull(gmo_auth_descr),
#                                     gme = relationship_elements_desc_2 %>% pull(gme_auth_descr))
# 
# 
# 
# concept_elements = list(concept_elements_1,concept_elements_2)
# relationship_elements = list(relationship_elements_1,relationship_elements_2)
# 
# concept_elements_auth = list(concept_elements_auth_1,concept_elements_auth_2)
# relationship_elements_auth = list(relationship_elements_auth_1,relationship_elements_auth_2)
