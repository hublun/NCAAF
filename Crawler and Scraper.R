#==================================== Input Parameters ================================
game_id = '400876570'
away_team <- 'CLEM'
home_team <- 'ALA'
#===================================== Read  in Whole Page=================================================
web_page_text <- getURLContent(paste('http://www.espn.com/college-football/playbyplay?gameId', game_id, sep = '='))
print(attributes(web_page_text))
tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
print(attributes(tree))
print(tree)
#===================================== Read in All Drives AND PLAYS ===============================================
game <- readHTMLList(tree) # READ IN ALL LISTS <LI>
drives <- game [10]
drives
drvs <- str_split(unlist(drives), "\\n")
drvs
drvs <- str_replace_all(unlist(drvs), "\\t", "")
str(drvs)
drvs
drvs <- drvs[which(str_length(drvs) != 0)] 
drvs

cwd <- getwd()
cwd
setwd(paste(cwd,"/Data", sep=""))
write(drvs, file = paste("Game", game_id,  away_team, home_team, ".txt", sep = "_"))
#============================ Retrieve All Drives only   =======================================

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



