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
write(club.tree, file="ss.txt")
#============== XPath =====select nodes anywhere in the document for the <div> tags==========
club.divs <- xpathSApply(club.tree, "//div[@class='stat-score']")

mhg <- club.divs[[1]]
mhg

scores <- xpathSApply(mhg, "//div[@class='team-scores']")
scores
