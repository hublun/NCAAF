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

att <- club.h3s[7]
is.AttendanceNode <- function(att) {
  return (str_detect(xmlValue(att[[1]]), "Attendance"))
}

is.AttendanceNode(att)

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

getAttendance(club.h3s[7])

getAllAttendance <- function(h3s){
 at.fr <- data.frame()
 for (i in 1:length(h3s)){
   if (is.AttendanceNode(h3s[i])){
     at.fr <- cbind(at.fr, getAttendance(h3s[i]))
   }
 }
 return (at.fr)
}

getAllAttendance(club.h3s)

#mhg <- club.divs[[1]]
#str(mhg)
#xmlName(mhg)
#names(mhg)  # child nodes of this node
#xmlAttrs(mhg) # all attributes
#child.content <- xmlSApply(mhg, xmlValue) # loop over the nodes inside mhg and get the content as s string

 
