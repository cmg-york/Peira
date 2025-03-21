
allIDs = c(gmo_data[[4]]$unclean$PROLIFIC_PID,gmo_data[[4]]$clean$PROLIFIC_PID)
retest = gmo_data[[5]]$PROLIFIC_PID
disqualify = gmo_data[[4]]$unclean$PROLIFIC_PID


reinvite = setdiff(gmo_data[[4]]$clean$PROLIFIC_PID,retest)
ban = setdiff(disqualify,retest)

cat(paste0(reinvite,sep = ","))
cat(paste0(ban,sep = ","))

