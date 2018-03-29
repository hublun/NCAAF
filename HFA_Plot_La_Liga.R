library("rstan")
#============= Change Param Names to Team Names =================
names(fit) # listing all papam names in the stanfit object

names(fit)[483]
names(fit)[483] <- "Bayern_Munich"
names(fit)[484]
names(fit)[484] <- "RB_Leipzig"
names(fit)[485]
names(fit)[485] <- "Borussia_Dortmund"
names(fit)[486]
names(fit)[486] <- "TSG_Hoffenheim"
names(fit)[487]
names(fit)[487] <- "FC_Cologne"

names(fit)[488]
names(fit)[488] <- "Hertha_Berlin"
names(fit)[489]
names(fit)[489] <- "SC_Freiburg"
names(fit)[490]
names(fit)[490] <- "Werder_Bremen"
names(fit)[491]
names(fit)[491] <- "Borussia_Monchengladbach"
names(fit)[492]
names(fit)[492] <- "Schalke_04"

names(fit)[493]
names(fit)[493] <- "Eintracht_Frankfurt"
names(fit)[494]
names(fit)[494] <- "Bayer_Leverkusen"
names(fit)[495]
names(fit)[495] <- "FC_Augsburg"
names(fit)[496]
names(fit)[496] <- "Hamburg_SV"
names(fit)[497]
names(fit)[497] <- "Mainz"

names(fit)[498]
names(fit)[498] <- "Vfl_Wolfsburg"
names(fit)[499]
names(fit)[499] <- "FC_Ingolstadt_04"
names(fit)[500]
names(fit)[500] <- "SV_Darmstadt_98"
names(fit)[481]
names(fit)[481] <- "AS_Nancy_Lorraine"
names(fit)[482]
names(fit)[482] <- "Bastia"

#==================== Plot the La_Liga Caterpillar Chart ========

plot(fit, ci_level = 0.90, point_est ="mean", est_color = "#126622", show_outer_line = TRUE, outer_level = 0.95,

pars=c("Bayern_Munich","RB_Leipzig", "Borussia_Dortmund","TSG_Hoffenheim", "FC_Cologne", "Hertha_Berlin", 
       "SC_Freiburg", "Werder_Bremen", "Borussia_Monchengladbach",
       "Schalke_04", "Eintracht_Frankfurt", "Bayer_Leverkusen", "FC_Augsburg", "Hamburg_SV", "Mainz", 
       "Vfl_Wolfsburg", "FC_Ingolstadt_04", "SV_Darmstadt_98"), 
     
     show_density=FALSE, fill_color="#123489") + geom_vline(xintercept = 0, linetype=2) +
  
  xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Team") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1, 1.5, 0.2), limits=c(-0.4, 1.8)) + theme_light()#theme_Posterior
