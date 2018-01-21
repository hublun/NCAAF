# Barcelona 83
# Real Mardrid 86
# AC Milan 103

club.id = 363

club.season.most.recent = 2016
club.season.most.past = 2001

getStats(club.id, 2016)

write.csv(getClubStats(club.id), file = paste("EPL_", club.id, "_2000_2016.csv", sep = ""))

