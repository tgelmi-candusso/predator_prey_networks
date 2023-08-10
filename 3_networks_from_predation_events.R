######### Code for generating bipartite networks from observed predation events ######### 
#
# Author: Tiziana A. Gelmi Candusso 
# Publication: Gelmi-Candusso, et al. (2023) Building urban predator-prey networks using camera traps. Food webs
#
# note: code uses dataset from publication available at repo tgelmi-candusso/predator_prey_networks
#       code creates summary table for predation events observed 
#
################################################################################

#install.packages("bipartite")
library(bipartite)
library(dplyr)
library(ggplot2)
library(bipartiteD3)


## NETWORK AND DETECTION GRAPH with predation observations#####

###items detected within predator's jaw or in the same frame, but weren't predation events:
nope <- c("squirrel (hunting)",
          "unknown (fish or trash)" , 
          "raccoon (interaction)", 
          #"same as before (squirrel)", 
          "bird (not interaction)", 
          "anthropogenic")

#load data
pred_aa <- read.csv("predation_events_pub.csv") 

#summarize observations of preyed items per predator
summ<- pred_aa %>% group_by(prey, predator) %>% dplyr::count()
f<- summ %>% filter(predator=='fox')
c <-summ %>% filter(predator=='coyote')
fc<-left_join(f,c, by='prey')
fc$n.y[is.na(fc$n.y)] <- 0
fc<-fc %>% select(prey, n.x, n.y)
colnames(fc)<- c('prey', 'fox', 'coyote')

#write.csv(fc, "observed_predation_events_table_summary.csv")

## bar graph with predation events observed (Figure 2a)
pred <- pred_aa

pred$prey <- factor(pred$prey, 
                                levels= c("small mammal", "medium size animal", "cat", "rabbit", "raccoon", "squirrel"),  
                                labels = c("Rodent-like", "Rabbit-like", "cat", "rabbit", "raccoon", "squirrel"))
pred <- na.omit(pred)
#plot counts per prey
ggplot(pred, aes(x = prey)) +
  geom_bar(aes(fill=predator),
           colour="black", 
           position=position_dodge()) + 
  #facet_wrap(~predator, nrow =1) +
  theme_classic() +
  scale_x_discrete(name="preyed species") +
  scale_y_continuous(name="observed events", breaks = seq(1,14, 1), limits=c(0,14)) + 
  scale_fill_brewer(name="predator",palette = "Set2")

##bipartite network from predation events dataset
##generate bipartite network with predation events ##
pred2 <- pred %>% select(predator, prey, site_name)
pred2$site<- "site_s"
colnames(pred2)<- c("predator","prey", "site_name", "site_s")
pred2$predator <- factor(pred2$predator, levels = c('fox', 'coyote'), labels=c('fox (Obs)', 'coyotes (Obs)'))
#write.csv(pred2, "OBS_interactions.csv", row.names=FALSE)

##generate bipartite network
web4<- frame2webs(pred2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web4$site_s)
#html plot with percentages (not included in publication)
bipartite_D3(web4$site_s)

#modularity - not included in publication
AAmod <- computeModules(web4$site_s, method="Beckett", deep = FALSE, deleteOriginalFiles = FALSE, 
                        steps = 1000000, tolerance = 1e-10, experimental = FALSE, forceLPA=FALSE)
#plotModuleWeb(AAmod)

#estimate nested contribution, not included in publication
nestedcontribution(web4$site_s, nsimul = 99)

