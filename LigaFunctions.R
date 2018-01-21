library(RCurl)
library(XML)
library(stringr)
#=========================attendance H3s ========================================
is.AttendanceNode <- function(att) {
  return (str_detect(xmlValue(att[[1]]), "Attendance"))
}
#======================================
is.StreakNode <- function (stt){
  return (str_detect(xmlValue(stt[[1]]),"Streak"))
}
#======================================
getAllAttendanceNodes <-  function(h3s){
  index <- c()
  for (i in 1:length(h3s)){
    if (is.AttendanceNode(h3s[i]))
      index <- c(index, i)
  }
  return (index)
} #===========end of attendance index function====
getAllGoalNodes <-  function(h3s){
  index <- c()
  for (i in 1:length(h3s)){
    if (!is.AttendanceNode(h3s[i]) & !is.StreakNode(h3s[i]))
      index <- c(index, i)
  }
  return (index)
} #===== end of goal index ====
#====================================================
getAllStreakNodes <-  function(h3s){
  index <- c()
  for (i in 1:length(h3s)){
    if (is.StreakNode(h3s[i]))
      index <- c(index, i)
  }
  return (index)
} #============== end of streak index function ======
#====================================================
getAttendance <- function(att){
  var.name = paste(str_replace_all(str_replace_all(xmlValue(att[[1]]), "\\W+", ""), " ", "_"), sep=".")
  var.name
  att.parent <- xmlParent(att[[1]]) # find the parent node of largets attendance
  #names(club.latt.parent)
  #names(xmlChildren(club.latt.parent))
  gg <- xmlElementsByTagName(att.parent, "p", recursive = TRUE)
  #length(gg)
  var.value <- as.integer(str_replace_all(xmlValue(gg[[length(gg)]]), "\\W+", ""))
  att.frame <- data.frame(var.value)
  names(att.frame) <- var.name # dynamically assign column names
  return (att.frame)
  #return (data.frame(var.name, var.value))
}
#==================

#==================================end of attendance extraction ========================================

getScoreResult <- function(h3p.node){
  
  p.var.name <- paste(str_replace_all(str_replace_all(xmlValue(h3p.node[[1]]), "\\W+", ""), " ", "_"), sep=".")
  p.var.name # get performance variable name
  
  h3p.parent.node <- xmlParent(h3p.node[[1]]) # get parent node of the current h3 node
  h3p.parent.node
  
  #game.nodes <- xmlElementsByTagName(h3p.parent.node, "span", recursive = TRUE)
  
  goal <- xmlChildren(xmlChildren(xmlChildren(xmlChildren(h3p.parent.node)$div)$div)$div)[4]
  scores <- xmlElementsByTagName(goal[[1]],"span", recursive = TRUE) # get two score nodes
  v1 <- as.integer(xmlValue(scores[[1]])) 
  v2 <- as.integer(xmlValue(scores[[2]]))
  #=============================== return result =================================
  if (str_detect(p.var.name, "Goal")){
    if (str_detect(p.var.name, "Home"))
      result = v1
    else
      result = v2
  }
  else
    result = abs(v1-v2)
  #=====return result data.frame ================
  p.fr <- data.frame(result)
  names(p.fr) <- p.var.name
  return (p.fr)
} # =========== end of the function =============

#===========get Streak Results =======================
getStreakResult <- function(h3p.node){
  
  p.var.name <- paste(str_replace_all(str_replace_all(xmlValue(h3p.node[[1]]), "\\W+", ""), " ", "_"), sep=".")
  p.var.name # get performance variable name
  
  h3p.parent.node <- xmlParent(h3p.node[[1]]) # get parent node of the current h3 node
  h3p.parent.node
  
  #game.nodes <- xmlElementsByTagName(h3p.parent.node, "span", recursive = TRUE)
  
  scores <- xmlElementsByTagName(h3p.parent.node,"span", recursive = TRUE) # get two score nodes
  streak <- xmlValue(scores[[1]])
  streak
  v1 <- as.integer(str_extract(streak, "\\d{1,2}")) 
  v1
  #=============================== return result =================================
  
  #=====return result data.frame ================
  p.fr <- data.frame(v1)
  names(p.fr) <- p.var.name
  p.fr
  return (p.fr)
} # end of streak result function =====
#======================================================================
getStats <- function(club.id, club.season){
  #============================================================================================
  url.1 =paste("http://www.espnfc.us/club/chelsea/", club.id, 
               "/statistics/performance?leagueId=23&season=", club.season, sep="")
  
  web_page_text <-getURLContent(url.1)
  #web_page_text <- getURLContent("http://www.espn.com/college-football/team/stats/_/id/333/year/2007")
  #print(attributes(web_page_text))
  club.tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
  
  #====================================
  #print(attributes(club.tree))
  #print(club.tree)
  #============== XPath =====select nodes anywhere in the document for the <div> tags==========
  
  club.divs <- xpathSApply(club.tree, "//div[@class='stat-score']") #, saveXML) #save node as strings
  club.h3s <- xpathSApply(club.tree, "//h3") # get all h3 var names of interest
  
  #=========================attendance H3s ========================================
  #===================== form all sttendance data.frame =====================
  club.frame = data.frame(club.season)
  names(club.frame) <- "Season"
  club.frame
  goal.index <- getAllGoalNodes(club.h3s)
  for (i in goal.index)
    club.frame <- cbind.data.frame(club.frame, getScoreResult(club.h3s[i]))
  
  streak.index <- getAllStreakNodes(club.h3s)
  for (i in streak.index)
    club.frame <- cbind.data.frame(club.frame, getStreakResult(club.h3s[i]))  
  
  att.index <- getAllAttendanceNodes(club.h3s)
  for (i in att.index)
    club.frame <- cbind.data.frame(club.frame, getAttendance(club.h3s[i]))
  club.frame
  #==================================form all Goal data.frame ========================================
  return (club.frame)
}
#=================================All seasons Data ==================================================
getClubStats <- function(club.id){
  
  club.stats <- data.frame()
  
  for (i in club.season.most.past:club.season.most.recent){ 
    #===========================================================
    statt <- getStats(club.id, i)
    
    if (dim(statt)[2] == 13) {
      club.stats <- rbind.data.frame(club.stats, statt)
    } 
    
  } #=========================================================
  return (club.stats)
} #=========================================================
