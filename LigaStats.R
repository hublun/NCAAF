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

club.h3s

club.latt <- club.h3s[1]

xmlParent(club.latt[[1]]) # find the parent node of largets attendance


mhg <- club.divs[[1]]
str(mhg)
 
xmlName(mhg)
names(mhg)  # child nodes of this node
xmlAttrs(mhg) # all attributes


xmlChildren(mhg)[4]
xmlParent(mhg)[4]
xmlValue(mhg)

child.content <- xmlSApply(mhg, xmlValue) # loop over the nodes inside mhg and get the content as s string

 
