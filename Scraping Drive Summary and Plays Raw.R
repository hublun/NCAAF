#==================================== Input Parameters ================================
game_id = '400876570'
#===================================== Read  in Whole Page=================================================
web_page_text <- getURLContent(paste('http://www.espn.com/college-football/playbyplay?gameId', game_id, sep = '='))
print(attributes(web_page_text))
tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
print(attributes(tree))
print(tree)
#=============== read team names ===============
teams <- readHTMLTable(tree)
str(teams)
line.scores <- teams[1]

team.names <- line.scores$linescore

away_team <- team.names$ESPN[1]
home_team <- team.names$ESPN[2]

print(home_team)
#===================================== Read in All Drives AND PLAYS ===============================================


#=========== read all drive and plays info stored in <li>s =====
game <- readHTMLList(tree) # READ IN ALL LISTS <LI>
dps <- game [10]
dps <- str_split(unlist(dps), "\\n")
dps <- unlist(dps)
 #--------------------------------------------
drive_pattern <- paste(away_team,"[[:digit:]]{1,2}" ,home_team, sep = '')
drive_index <- which(str_detect(dps, drive_pattern))
#dps[drive_index]

rps_index <- which(str_detect(dps, "\\t{7}\\(")==TRUE) # regular plays not drives
#dps[rps_index]

pos_index <- which(str_detect(dps, "\\t{5}\\d")==TRUE) # field positions
#dps[pos_index]

koff_index <- which(str_detect(dps, "kickoff")==TRUE)
punt_index <- which(str_detect(dps, "punt for")==TRUE)
onsk_index <- which(str_detect(dps, "on-side")==TRUE)
stp_index <- sort(c(koff_index, punt_index, onsk_index))


dprp_index <- sort(c(drive_index, pos_index, rps_index))

drives_only <- dps[drive_index]
#============== form the drives dataframe ++++++================================================
drive.frame <- data.frame(outcome=character(), num_plays = integer(), yards = integer(), drive_time = integer(),
                          away_score = integer(), home_score = integer(), stringsAsFactors = FALSE)

for (i in 1:length(fos)){
  drive.frame <-rbind(drive.frame, convert_os(drives_only[i]))
}
#-----------------------------------------------------------------
drive.frame <- cbind(drive.frame, Game_ID = rep(game_id, nrow(drive.frame)), Away_Team = rep(away_team, nrow(drive.frame)), Home_Team = rep(home_team, nrow(drive.frame)))
#============================= save to file ===============================================
cwd <- getwd()

if (str_detect(cwd, "/Data")== FALSE){
  setwd(paste(cwd,"/Data", sep=""))
}

write(dps[dprp_index], file = paste("Game", game_id,  away_team, home_team, ".txt", sep = "_"))

write.csv(drive.frame, file = paste("Game", game_id,  away_team, home_team, ".csv", sep = "_"))
#============================ Retrieve All Drives only   =======================================