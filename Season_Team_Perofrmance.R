web_page_text <- getURLContent("http://www.espn.com/college-football/team/stats/_/id/333/year/2007")
print(attributes(web_page_text))
tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
print(attributes(tree))
print(tree)

stats.tables.all <- readHTMLTable(tree)
print(stats.tables.all)

#====================== schedules and results ======================
web_page.schedule <- getURLContent("http://www.espn.com/college-football/team/schedule/_/id/333/year/2016")
trees.schedule <- htmlTreeParse(web_page.schedule, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
schedules.tables <- readHTMLTable(trees.schedule)
schedules.tables <- schedules.tables[[1]]
schedules.tables$V3 # all scores with W or L

