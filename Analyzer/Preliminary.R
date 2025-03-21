items <- read_excel("../Instrument/InstrumentData/items.xlsx", sheet = "Items")
names <- read_excel("../Instrument/InstrumentData/items.xlsx", sheet = "Key")

gmo_c = names %>% filter(Language == "gmo",Type == "concept") %>% pull(Descr)
gmo_r = names %>% filter(Language == "gmo",Type == "relationship") %>% pull(Descr)

gme_c = names %>% filter(Language == "gme",Type == "concept") %>% pull(Descr)
gme_r = names %>% filter(Language == "gme",Type == "relationship") %>% pull(Descr)

lang_names = c(gmo = "Goal Models", gme = "Intention Models")

lang = list(
  gmo = list(concepts = gmo_c, relationships = gmo_r),
  gme = list(concepts = gme_c, relationships = gme_r)
)

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
