team.id <- 166

#team.year <- 2007
#==============================extract season stats =======================================================
stats_by_team_year <- function (team.id=333, team.year=2007) {

  url1 <- paste("http://www.espn.com/college-football/team/stats/_/id/",team.id,"/year/",team.year, sep="")
url2 <- paste("http://www.espn.com/college-football/team/schedule/_/id/",team.id,"/year/", team.year, sep="")
#======================================================================
web_page_text <-getURLContent(url1)
#web_page_text <- getURLContent("http://www.espn.com/college-football/team/stats/_/id/333/year/2007")
#print(attributes(web_page_text))
tree <- htmlTreeParse(web_page_text, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
#print(attributes(tree))
#print(tree)

stats.tables.all <- readHTMLTable(tree)
team.passing <- stats.tables.all[[1]]
team.rushing <- stats.tables.all[[2]]
team.receiving <- stats.tables.all[[3]]
#print(team.passing)
#==================== receiving stats ===================
rec.recs <- team.receiving[nrow(team.receiving),2]
team.c.recs <- as.numeric(levels(rec.recs)[rec.recs])
#team.c.recs
rec.yards <- team.receiving[nrow(team.receiving),3]
team.c.yards <- as.numeric(levels(rec.yards)[rec.yards])
#team.c.yards
rec.longest <- team.receiving[nrow(team.receiving),5]
team.c.longest <- as.numeric(levels(rec.longest)[rec.longest])
#team.c.longest
rec.tds <- team.receiving[nrow(team.receiving),6]
team.c.tds <- as.numeric(levels(rec.tds)[rec.tds])
#team.c.tds
#==================== passing stats    == ===============
pass.completions <- team.passing[nrow(team.passing),2]
pass.attempts <- team.passing[nrow(team.passing),3]
pass.yards <- team.passing[nrow(team.passing),4]
pass.completion_percentage <- team.passing[nrow(team.passing),5]
pass.avg_yards <- team.passing[nrow(team.passing),6]
pass.touchdowns <- team.passing[nrow(team.passing),7]
pass.interceptions <- team.passing[nrow(team.passing),8]
pass.QB_rating <- team.passing[nrow(team.passing),9]

team.p.att <- as.numeric(levels(pass.attempts)[pass.attempts])
team.p.cmp <- as.numeric(levels(pass.completions)[pass.completions])
team.p.yards <- as.numeric(levels(pass.yards)[pass.yards])
team.p.tds <- as.numeric(levels(pass.touchdowns)[pass.touchdowns])
team.p.ints <- as.numeric(levels(pass.interceptions)[pass.interceptions])
team.qb.rating <- as.numeric(levels(pass.QB_rating)[pass.QB_rating])
#c(team.p.att, team.p.cmp, team.p.yards, team.p.ints, team.p.tds, team.qb.rating)
#==================rushing stats ==============================================
russ.carrys <- team.rushing[nrow(team.rushing),2]
team.r.carrys <- as.numeric(levels(russ.carrys)[russ.carrys])
#team.r.carrys
russ.yards <- team.rushing[nrow(team.rushing),3]
team.r.yards <- as.numeric(levels(russ.yards)[russ.yards])
#team.r.yards
russ.longest <- team.rushing[nrow(team.rushing),5]
team.r.longest <- as.numeric(levels(russ.longest)[russ.longest])
#team.r.longest
russ.tds <- team.rushing[nrow(team.rushing),6]
team.r.tds <- as.numeric(levels(russ.tds)[russ.tds])
#team.r.tds
#====================== schedules and results ======================
web_page.schedule <- getURLContent(url2)
#web_page.schedule <- getURLContent("http://www.espn.com/college-football/team/schedule/_/id/333/year/2016")
trees.schedule <- htmlTreeParse(web_page.schedule, useInternalNodes = TRUE, asText = TRUE, isHTML = TRUE)
schedules.tables <- readHTMLTable(trees.schedule)
schedules.tables <- schedules.tables[[1]]
schedules.tables$V3 # all scores with W or L
nrow(schedules.tables)
#=============== =========Total Points Scored and Total Points Allowed =============

game.results <- unlist(schedules.tables$V3)

team.tps <- 0
team.tpa <- 0

for (i in 1:nrow(schedules.tables)){
  #print(game.results[i])
  ss <- convert_game_result(game.results[i])
  team.tps <- team.tps + ss[1]
  team.tpa <- team.tpa + ss[2]
}

#print(c(team.tps, team.tpa))
#--------------W L Ratio-------------------
team.record.year_end <- schedules.tables$V4[nrow(schedules.tables)]
#print(team.record.year_end)

wlwl <- as.integer(unlist(str_extract_all(team.record.year_end, "\\d{1,2}")))
team.wlratio.overall <- wlwl[1] / (wlwl[2]+0.1)
team.wlratio.conference <- wlwl[3] / (wlwl[4]+0.1)
#====================== overall win-loss ratio and conference win-loss ratio ======
team.wlratio.overall
team.wlratio.conference
#==================================================================+++++++++++
return(data.frame(team= team.id, season= team.year, tpcmp = team.p.cmp,tpatt=team.p.att,tpyds=team.p.yards, 
                  tpint = team.p.ints, tptds = team.p.tds, tpqbrat = team.qb.rating,
                  trcary = team.r.carrys, tryds = team.r.yards, trlong = team.r.longest,
                  trtds = team.r.tds, tcrecs = team.c.recs, tcyds = team.c.yards,
                  tclong = team.c.longest, tctds = team.c.tds, tps = team.tps, tpa = team.tpa,
                  wlrall = team.wlratio.overall, wlrconf = team.wlratio.conference,
                  stringsAsFactors = FALSE))
}
#==========================================================================================================


team.frame = data.frame()

for (i in 2004:2016)
 team.frame <- rbind(team.frame, stats_by_team_year(team.id,i))

team.frame
write.csv(team.frame, file = paste("Team", team.id, ".csv", sep = "_"))
