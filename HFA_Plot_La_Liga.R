library("rstan")
#============= Change Param Names to Team Names =================
names(fit) # listing all papam names in the stanfit object

names(fit)[463]
names(fit)[463] <- "AS_Monaco"
names(fit)[464]
names(fit)[464] <- "Paris_Saint_Germain"
names(fit)[465]
names(fit)[465] <- "Nice"
names(fit)[466]
names(fit)[466] <- "Lyon"
names(fit)[467]
names(fit)[467] <- "Marseille"

names(fit)[468]
names(fit)[468] <- "Bordeaux"
names(fit)[469]
names(fit)[469] <- "Nantes"
names(fit)[470]
names(fit)[470] <- "St_Etienne"
names(fit)[471]
names(fit)[471] <- "Stade_Rennes"
names(fit)[472]
names(fit)[472] <- "Guingamp"

names(fit)[473]
names(fit)[473] <- "Lille"
names(fit)[474]
names(fit)[474] <- "Angers"
names(fit)[475]
names(fit)[475] <- "Toulouse"
names(fit)[476]
names(fit)[476] <- "Metz"
names(fit)[477]
names(fit)[477] <- "Montpellier"

names(fit)[478]
names(fit)[478] <- "Dijon_FCO"
names(fit)[479]
names(fit)[479] <- "Caen"
names(fit)[480]
names(fit)[480] <- "Lorient"
names(fit)[481]
names(fit)[481] <- "AS_Nancy_Lorraine"
names(fit)[482]
names(fit)[482] <- "Bastia"

#==================== Plot the La_Liga Caterpillar Chart ========

plot(fit, ci_level = 0.90, point_est ="mean", est_color = "#126622",
     
     show_outer_line = TRUE, outer_level = 0.95,

          

     pars=c("AS_Monaco","Paris_Saint_Germain", "Nice","Lyon", "Marseille", "Bordeaux", "Nantes", "St_Etienne", "Stade_Rennes",
            
           "Guingamp", "Lille", "Angers", "Toulouse", "Metz", "Montpellier", "Dijon_FCO", "Caen", "Lorient",
            
          "AS_Nancy_Lorraine", "Bastia"), 
     
     show_density=FALSE, fill_color="#123489") + geom_vline(xintercept = 0, linetype=2) +
  
  xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Team") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1, 1.5, 0.2), limits=c(-0.4, 1.8)) +
  
  theme_light()#theme_Posterior
