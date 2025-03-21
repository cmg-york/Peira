#
# Loading Language and Concepts
#
getConfigData <- function(dataPath,configID) {
  configFile = paste0(dataPath, configID,".xlsx")
  demosFile = paste0(dataPath, "demos.csv")
  
  cdf <- read_excel(configFile, sheet = "Concepts")
  ldf <- read_excel(configFile, sheet = "Languages")
  c <- read_excel(configFile, sheet = "Config")
  adf <- read_excel(configFile, sheet = "Cases")
  idf <- read_excel(configFile, sheet = "Items")  
  
  return ( list(concepts_df = cdf, lang_df = ldf, config = c, cases_df = adf, items_df = idf) )
}

