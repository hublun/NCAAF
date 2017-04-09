# Barcelona 83
# Real Mardrid 86
# AC Milan 103

club.id = 137

#club.season.most.recent = 2016
#club.season.most.past = 2001


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



write.csv(getClubStats(club.id), file = paste("Bundesliga_", club.id, "_2001_2016.csv", sep = ""))

