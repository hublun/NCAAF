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
dps <- game [10]
dps <- str_split(unlist(dps), "\\n")
dps <- unlist(dps)
dps
 #--------------------------------------------
rps_index <- which(str_detect(dps, "\\t{7}\\(")==TRUE) # regular plays not drives
dps[rps_index]
pos_index <- which(str_detect(dps, "\\t{5}\\d")==TRUE) # field positions
dps[pos_index]

pos_index
rps_index

length(dps)
length(rps_index)
length(pos_index)


str(drvs)
drvs
drvs <- drvs[which(str_length(drvs) != 0)] 
drvs

cwd <- getwd()
cwd
setwd(paste(cwd,"/Data", sep=""))
write(drvs, file = paste("Game", game_id,  away_team, home_team, ".txt", sep = "_"))
#============================ Retrieve All Drives only   =======================================