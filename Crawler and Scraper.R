drive_pattern <- paste(away_team,"[[:digit:]]{1,2}" ,home_team, sep = '')

dlist <- which(str_detect(drvs, drive_pattern)) # index of drive summaries only

st_list <- which(str_detect(drvs, "kickoff[[:print:]]{10,}return") == TRUE) # index of special teams plays only

drvs[-st_list]

plays <- drvs[-dlist]
plays
oss <- drvs[dlist]

drvs[dlist] <- paste("======", drvs[dlist], "=============", sep='')

drvs
oss
write(oss, file = paste("Game", game_id,  away_team, home_team, "oss.txt", sep = "_"))

unlist(convert_os(oss[7]))[3]

#=========================== Label Drives in Drives and Plays List =============================
time_pattern <- "\\(+.+\\d..\\)"
timed_plays <- which(str_detect(drvs, time_pattern) == TRUE)

unlist(str_extract(drvs[timed_plays], time_pattern))



