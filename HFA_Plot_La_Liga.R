library("rstan")
#============= Change Param Names to Team Names =================
names(fit) # listing all papam names in the stanfit object

names(fit)[443]
names(fit)[443] <- "Juventus"
names(fit)[444]
names(fit)[444] <- "AS_Roma"
names(fit)[445]
names(fit)[445] <- "Napoli"
names(fit)[446]
names(fit)[446] <- "Atalanta"
names(fit)[447]
names(fit)[447] <- "Lazio"

names(fit)[448]
names(fit)[448] <- "AC_Milan"
names(fit)[449]
names(fit)[449] <- "Internazionale"
names(fit)[450]
names(fit)[450] <- "Fiorentina"
names(fit)[451]
names(fit)[451] <- "Torino"
names(fit)[452]
names(fit)[452] <- "Sampdoria"

names(fit)[453]
names(fit)[453] <- "Cagliari"
names(fit)[454]
names(fit)[454] <- "Sassuolo"
names(fit)[455]
names(fit)[455] <- "Udinese"
names(fit)[456]
names(fit)[456] <- "Chievo_Verona"
names(fit)[457]
names(fit)[457] <- "Bologna"

names(fit)[458]
names(fit)[458] <- "Genoa"
names(fit)[459]
names(fit)[459] <- "Crotone"
names(fit)[460]
names(fit)[460] <- "Empoli"
names(fit)[461]
names(fit)[461] <- "Palermo"
names(fit)[462]
names(fit)[462] <- "US_Pescara"

#==================== Plot the La_Liga Caterpillar Chart ========

plot(fit, ci_level = 0.90, point_est ="mean", est_color = "#126622",
     
     show_outer_line = TRUE, outer_level = 0.95,

     pars=(),     

#     pars=c("Juventus","AS_Roma", "Napoli","Atalanta", "Lazio", "AC_Milan", "Internazionale", "Fiorentina", "Torino",
            
#           "Sampdoria", "Cagliari", "Sassuolo", "Udinese", "Chievo_Verona", "Bologna", "Genoa", "Crotone", "Empoli",
            
#          "Palermo", "US_Pescara"), 
     
     show_density=FALSE, fill_color="#123489") + geom_vline(xintercept = 0, linetype=2) +
  
  xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Team") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1, 1.5, 0.2), limits=c(-0.4, 1.6)) +
  
  theme_light()#theme_Posterior
