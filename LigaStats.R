club.id = 83
club.season = 2015
#============================================================================================
url.1 ="http://www.espnfc.us/club/barcelona/83/statistics/performance?leagueId=all&season=2015"
web_page_text <-getURLContent(url.1)
#web_page_text <- getURLContent("http://www.espn.com/college-football/team/stats/_/id/333/year/2007")
#print(attributes(web_page_text))
club.tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
#print(attributes(tree))
print(club.tree)
#============== XPath =====select nodes anywhere in the document for the <div> tags==========

club.divs <- xpathSApply(club.tree, "//div[@class='stat-score']") #, saveXML) #save node as strings
club.h3s <- xpathSApply(club.tree, "//h3") # get all h3 var names of interest

#=========================attendance H3s ========================================
#===================== form all sttendance data.frame =====================
  club.frame = data.frame(club.season)
  names(club.frame) <- "Season"
  club.frame
  att.index <- getAllAttendanceNodes(club.h3s)
  for (i in att.index)
    club.frame <- cbind.data.frame(club.frame, getAttendance(club.h3s[i]))
  club.frame
 #==================================form all Goal data.frame ========================================



