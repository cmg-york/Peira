source("Generator.R")
# MAIN: Export Modeling Language ---------------------
outDir = "../Projects/iRL/Data/"
dataPath = "../Projects/iRL/Parameters/"
configID = "config"
generateFiles = TRUE

confData = getConfigData(dataPath,configID)

concepts_df = confData$concepts_df
lang_df = confData$lang_df
config = confData$config
cases_df = confData$cases_df
items_df = confData$items_df


targetDir = paste0(configID," - ",format(Sys.time(), format = "%y-%m-%d %H-%M-%S"))
dir.create(file.path(outDir, targetDir), showWarnings = TRUE)
outPath = paste0(outDir,targetDir,"/")
file.copy(configFile,outPath)
file.copy(demosFile,outPath)


key_obs_ = tribble(~psy_item, ~item, ~item_full, 
                   ~description, ~language,~type,
                   ~psy_option, ~auth)
key_att_ = tribble(~psy_item, ~language, ~conceptID, ~concept)

for (langID in c("iRL")){
  outFile = paste0(outPath,langID,"-checked.txt")
  if (generateFiles) {
    sink(outFile, append=FALSE)
  }
  prolific(1)
  
  
  
  print_preliminary(langID,
                    sample(1:5, size = 3, replace = FALSE),
                    sample(1:5, size = 3, replace = FALSE),
                    sample(1:5, size = 3, replace = FALSE),
                    sample(1:5, size = 3, replace = FALSE)
                    )
  key_att_ = print_overlap(langID, key_att_)
  for (caseID in cases_df$caseID){
    key_obs_ = print_observational_check(langID,caseID,key = key_obs_)
  }
  key_att_ = print_self_reporting(langID, key_att_)
  demographics(FALSE)
  pilot(TRUE)
  prolific(2)
  if (generateFiles) {
    sink()
  }
}

if (generateFiles){
  write.csv(x = key_obs_, file=paste0(outPath,"key.csv"),row.names = FALSE)
  write.csv(x = key_att_, file=paste0(outPath,"key-attitudinal.csv"),row.names = FALSE)
}
