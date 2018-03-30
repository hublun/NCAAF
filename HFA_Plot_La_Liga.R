library("rstan")
#============= Change Param Names to Team Names =================
names(fit) # listing all papam names in the stanfit object

names(fit)[501]
names(fit)[501] <- "Chelsea"
names(fit)[502]
names(fit)[502] <- "Tottenham_Hotspur"
names(fit)[503]
names(fit)[503] <- "Manchester_City"
names(fit)[504]
names(fit)[504] <- "Liverpool"
names(fit)[505]
names(fit)[505] <- "Arsenal"

names(fit)[506]
names(fit)[506] <- "Manchester_United"
names(fit)[507]
names(fit)[507] <- "Everton"
names(fit)[508]
names(fit)[508] <- "Southampton"
names(fit)[509]
names(fit)[509] <- "AFC_Bournemouth"
names(fit)[510]
names(fit)[510] <- "West_Bromwich_Albion"

names(fit)[511]
names(fit)[511] <- "West_Ham_United"
names(fit)[512]
names(fit)[512] <- "Leicester_City"
names(fit)[513]
names(fit)[513] <- "Stoke_City"
names(fit)[514]
names(fit)[514] <- "Crystal_Palace"
names(fit)[515]
names(fit)[515] <- "Swansea_City"

names(fit)[516]
names(fit)[516] <- "Burnley"
names(fit)[517]
names(fit)[517] <- "Watford"
names(fit)[518]
names(fit)[518] <- "Hull_City"
names(fit)[519]
names(fit)[519] <- "Middlesbrough"
names(fit)[520]
names(fit)[520] <- "Sunderland"

#==================== Plot the La_Liga Caterpillar Chart ========

plot(fit, ci_level = 0.90, point_est ="mean", est_color = "#126622", show_outer_line = TRUE, outer_level = 0.95,

pars=c("Chelsea","Tottenham_Hotspur", "Manchester_City","Liverpool", "Arsenal", "Manchester_United", 
       "Everton", "Southampton", "AFC_Bournemouth",
       "West_Bromwich_Albion", "West_Ham_United", "Leicester_City", "Stoke_City", "Crystal_Palace", "Swansea_City", 
       "Burnley", "Watford", "Hull_City", "Middlesbrough", "Sunderland"), 
     
     show_density=FALSE, fill_color="#123489") + geom_vline(xintercept = 0, linetype=2) +
  
  xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Team") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1, 1.5, 0.2), limits=c(-0.4, 1.8)) + theme_light()#theme_Posterior
