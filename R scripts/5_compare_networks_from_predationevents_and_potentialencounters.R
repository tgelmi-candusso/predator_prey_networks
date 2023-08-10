######### Code for generating bipartite network comparing datasets from  ##########
#             predation events observed and potential encounter events 
#
# Author: Tiziana A. Gelmi Candusso 
# Publication: Gelmi-Candusso, et al. (2023) Building urban predator-prey networks using camera traps. Food webs
#
# note: code uses dataset from scripts 3 and 4, available at repo tgelmi-candusso/predator_prey_networks
#       code creates bipartite networks comparing datasets, uses data written in script 3 and 4, also available at repo
#
################################################################################

#install.packages("bipartite")
library(bipartite)
library(dplyr)
library(ggplot2)
library(bipartiteD3)

## follows with objects obtained in script 4 and script 3
# or
#load objects
out_pp2 <- read.csv("data/PEE_interactions.csv")
pred2 <- read.csv("data/OBS_interactions.csv")

#### Predation events vs potential encounters networks ####
#plot both in one bipartite network to compare predator-prey interactions observed with two different datasets
#all in one web both predation events and potential encounters
out_ppe <- out_pp2 %>% filter(!(bird_feeder =='yes')) %>% select(-bird_feeder)
pred2obs <- pred2 %>% select(-site_name)
all<-rbind(pred2obs, out_ppe)
fox_comp <- all %>% filter(predator %in% c('fox (Obs)', 'fox (PEE)'))
coy_comp <- all %>% filter(predator %in% c('coyotes (Obs)', 'coyote (PEE)'))
nrow(fox_comp %>% filter((predator=='fox (Obs)' & prey == 'rabbit')))
par(mfrow=c(1,1))
web10<- frame2webs(fox_comp[,c(1:3)], varnames = c( "prey", "predator","site_s"), type.out = "list", emptylist = TRUE)
plotweb(web10$site_s, arrow='down.center')
web10<- frame2webs(coy_comp[,c(1:3)], varnames = c( "prey", "predator","site_s"), type.out = "list", emptylist = TRUE)
plotweb(web10$site_s, arrow='down.center')
#interactive bipartite network
bipartite_D3(web10$site_s)
#modules
AAmod <- computeModules(web10$site_s, method="Beckett", deep = FALSE, deleteOriginalFiles = FALSE, 
                        steps = 1000000, tolerance = 1e-10, experimental = FALSE, forceLPA=FALSE)
#plotModuleWeb(AAmod)



