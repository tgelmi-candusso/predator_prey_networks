######### Code for generating bipartite networks from potential encounter events ######### 
#
# Author: Tiziana A. Gelmi Candusso 
# Publication: Gelmi-Candusso, et al. (2023) Building urban predator-prey networks using camera traps. Food webs
#
# note: code uses dataset from publication available at repo tgelmi-candusso/predator_prey_networks
#       code creates bipartite networks from waiting times output from script 1 in repo but can also
#       use publication waiting times dataset available at repo
#
################################################################################



#### POTENTIAL ENCOUNTER EVENTS #####

#install.packages("bipartite")
library(bipartite)
library(dplyr)
library(ggplot2)
library(bipartiteD3)

#load data from publication
output_allsites <- read.csv("data/output_allsites_pub.csv")


#Species 1 is the firest species detected, and Species 2 is the following species detected
# units for time_interval are in decimal hours, multiply by 60 to get minutes, divide minutes by 60 to get hours in decimals.

## define objects for focal predators and focal prey, and other combinations we might need
species <- levels(output_allsites$Species_1)
predators <- c("fox", "coyote")
actual_Potprey <- c("dog", "cat", "deer", "rabbit", "raccoon", "skunk", "squirrel", "oppossum")
fox_coy_dogs <- c("fox", "coyote", "dog")


##### add information to table to identify whether first or second species detected is predator or prey ####

out<-output %>% 
  mutate(species1 = ifelse((Species_1 %in% predators), 'predator',
                           ifelse((Species_1 %in% actual_Potprey), 'prey','NA')))%>%
  mutate(species2 = ifelse((Species_2 %in% predators), 'predator',
                           (ifelse((Species_2 %in% actual_Potprey), 'prey','NA'))))

# out<-out %>% 
#   mutate(fox_coy_dogs = ifelse(((Species_1 %in% fox_coy_dogs)&(Species_2 %in% fox_coy_dogs)), 'yes', 'no'))

#filter out instances where first and second species are the same
out2<-out %>% filter(species1!=species2)

#separate instances where first species is a predator and when it is a prey, to re-format
#in a more accesible way for later analysis, with a column for predator species and a column for prey species
out_a<- out2 %>% filter (species1 == 'predator') %>%
  mutate(predator = Species_1) %>%
  mutate(prey = Species_2)

out_b<- out2 %>% filter (species1 == 'prey') %>%
  mutate(predator = Species_2) %>%
  mutate(prey = Species_1)

out_pp<- rbind(out_a, out_b)

#define the site with the bird-feeder as yes bird feeder, and restrict time_interval to ~5 minutes
out_pp1 <- out_pp %>%
  mutate(bird_feeder = ifelse(site_name == 'site 10', 'yes', 'no')) %>%
  filter(time_interval <= 0.08666667) #%>% 
#filter(prey!= "dog")

#define factor
out_pp1$predator <- factor(out_pp1$predator, levels =c("fox","coyote", "dog", "cat", 'squirrel', 'rabbit', 'raccoon', 'skunk', 'oppossum', 'deer', "small rodent","mink","not listed","groundhog", "horse","empty", "chipmunk","bird"))
out_pp1$prey <- factor(out_pp1$prey, levels =c("dog", "cat", 'squirrel', 'rabbit', 'raccoon', 'skunk', 'oppossum', 'deer', "small rodent","mink","not listed","groundhog", "horse","empty", "chipmunk","bird", "coyote","fox"))

#generate plot with only potential encounter events between coyotes and foxes
#only dogs, foxes and coyotes
out_c<- out %>% 
  mutate(pred_pred = ifelse(species1=='predator' & species2=='predator', 'yes', 'no'))
out_fcd<- out_c %>% filter(species1=='predator' & species2=='predator')
out_fcd<- out_fcd %>%
  mutate(predator = "fox")

out_fcd2 <- out_fcd %>% 
  mutate(bird_feeder = ifelse(site_name == 'site 10', 'yes', 'no')) %>%
  filter(time_interval <= 0.08666667) 


supp.labs <- c("coyote")
names(supp.labs) <- c("predator")

#figure 2b right box
ggplot(out_fcd2, aes(x=predator, y=time_interval, shape=Species_1)) + 
  geom_jitter(aes(fill= bird_feeder), width = 0.20) + 
  facet_wrap(~species1, nrow =1, labeller=labeller(species1 = supp.labs)) +
  #theme(panel.grid.major.y = element_line(colour = "grey",linetype="dashed",size=0.1))+
  scale_x_discrete(name = "species")+
  scale_y_continuous(name = "waiting times", limits = c(-0.000001,0.08666667), breaks = c(seq(0.016666667,0.08333334, by=0.016666667)), labels = c('1 min', '2 min', '3 min', '4 min', '5 min'))+
  scale_shape_manual(name = 'First species',
                     values=c(24, 21))+
  scale_fill_manual(name = 'Bird feeder', values = c("black", "white"))+
  ggtitle('Potential encounter events')+
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey", linetype="dashed"), 
        panel.grid.minor.y = element_blank())

##plot potential encounter events between two focal predators and their potential prey.
#Figure 2b
##coyote and fox with their prey separately
ggplot(out_pp1, aes(x=prey, y=time_interval,  shape=species1)) + 
  geom_jitter(aes(fill= bird_feeder), width = 0.20) + 
  facet_wrap(~predator, nrow =2) +
  scale_x_discrete(name = "species")+
  scale_y_continuous(name = "waiting times",breaks = c(0.016666667,0.0333333, 0.05, 0.066666667, 0.08333334), labels = c('1 min', '2 min', '3 min', '4 min', '5 min'))+
  scale_shape_manual(name = 'First species',
                     values=c("predator"=24,"prey"=21))+
  scale_fill_manual(name = 'Bird feeder', values = c("black", "white"))+
  ggtitle('Potential encounter events')+
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey", linetype="dashed"), 
        panel.grid.minor.y = element_blank())




##N numbers of detections across different combinations of predator and with/without dogs, with/without bird feeder
# numbers given in results section of publication
##how many of out_pp1 are foxes, coyotes etc - summary numbers for paper.
nrow(out_pp1 %>% filter(predator=='fox'))
nrow(out_pp1 %>% filter(predator=='coyote'))

nrow(out_pp1 %>% filter(predator=='fox'& prey=='dog'))
nrow(out_pp1 %>% filter(predator=='coyote'& prey=='dog'))

nrow(out_pp1 %>% filter(species1=='predator'& prey!='dog'))
nrow(out_pp1 %>% filter(species1=='prey'& prey!='dog'))

nrow(out_pp1 %>% filter((species1=='predator' & predator =='fox' & prey!='dog')))
nrow(out_pp1 %>% filter((species1=='predator' & predator =='coyote' & prey!='dog')))
nrow(out_pp1 %>% filter((species1=='prey' & predator =='fox' & prey!='dog')))
nrow(out_pp1 %>% filter((species1=='prey' & predator =='coyote' & prey!='dog')))

nrow(out_pp1 %>% filter((predator=='fox' & site_name =="site 10")))
nrow(out_pp1 %>% filter((predator=='coyote' & site_name =="site 10")))
nrow(out_pp1 %>% filter((predator=='fox' & site_name !="site 10")))
nrow(out_pp1 %>% filter((predator=='coyote' & site_name !="site 10")))

###count those excluding dogs and excluding site with bird feeder
nrow(out_pp1 %>% filter((predator=='fox' & site_name !="site 10"& prey!='dog')))
nrow(out_pp1 %>% filter((predator=='coyote' & site_name !="site 10"& prey!='dog')))

nrow(out_pp1 %>% filter((species1=='predator' & prey!='dog')))
nrow(out_pp1 %>% filter((species1=='prey' & prey!='dog')))

nrow(out_pp1 %>% filter((species1=='predator' & predator=='fox' & site_name !="site 10"& prey!='dog')))
nrow(out_pp1 %>% filter((species1=='prey' & predator=='fox' & site_name !="site 10"& prey!='dog')))
nrow(out_pp1 %>% filter((species1=='predator' & predator=='coyote' & site_name !="site 10"& prey!='dog')))
nrow(out_pp1 %>% filter((species1=='prey' & predator=='coyote' & site_name !="site 10"& prey!='dog')))


nrow(out_pp1)
nrow(out_pp1 %>% filter((prey=='dog')))
nrow(out_pp1 %>% filter((site_name !="site 10"& predator=='fox'& prey!='dog')))
nrow(out_pp1 %>% filter((site_name !="site 10"& prey!='dog')))

#interactions between dogs and foxes
nrow(out_fcd2)


####IF considering unidirectional detections, that is only when predator or prey comes first, this is the code.
# we did not consider them separately for logistics reasons, but if needed here it is. 

### considering unidirectional detections, either predator first only or prey first only ###
##when considering predator after prey

out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) %>%
  filter(species2 == 'predator') #only consider when predator comes second (after prey)
out_pp2 <- out_pp2 %>% select(prey, predator, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("prey", "predator", "bird_feeder", "site_s")

##generate bipartite network
web5<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web5$site_s, arrow='down')
bipartite_D3(web5$site_s)
Bmod_pred <- computeModules(web5$site_s)
Bmodpred_prey<- plotModuleWeb(Bmod_pred)
networklevel(web5$site_s)
nestedcontribution(web5$site_s, nsimul = 99)

###when considering prey after predator
out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) %>%
  filter(species1 == 'predator') #only considering when predator comes before prey
out_pp2 <- out_pp2 %>% select(predator, prey, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("predator","prey", "bird_feeder", "site_s")
##create file for web
web6<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web6$site_s)
bipartite_D3(web6$site_s)
Bmod_prey <- computeModules(web6$site_s)
#Bmodprey_pred<- plotModuleWeb(Bmod_prey)
#stats we did not include
networklevel(web6$site_s)
nestedcontribution(web6$site_s, nsimul = 99)

##what we included in the manuscript, the bidirectional method, when instances where predator is first or second are pooled 
### bidirectional predator prey detection, network, disregarding of who came first ####
out_pp2 <- out_pp1 %>% select(predator, prey, bird_feeder, species1, species2)%>%
  filter(prey != 'dog')%>%
  filter(!(bird_feeder == 'yes')) 
out_pp2 <- out_pp2 %>% select(predator, prey, bird_feeder)
out_pp2$site<- "site_s"
colnames(out_pp2)<- c("predator","prey", "bird_feeder", "site_s")
out_pp2$predator <- factor(out_pp2$predator, labels = c('fox (PEE)', 'coyote (PEE)'))
write.csv(out_pp2, "data/PEE_interactions.csv", row.names=FALSE)

##generate bipartite network
web7<- frame2webs(out_pp2[,c(1:2,4)], varnames = c("prey", "predator", "site_s"), type.out = "list", emptylist = TRUE)
plotweb(web7$site_s)
bipartite_D3(web7$site_s)
Bmod <- computeModules(web7$site_s)
#bipartite::plotModuleWeb(Bmod) #recently not working, this function would plot the modules
#stats we did not include in the publication
networklevel(web7$site_s)
nestedcontribution(web7$site_s, nsimul = 99)

## plot all graphs together: modules #not working since update :( )
# par(mfrow=c(2,2))
# plotModuleWeb(AAmod)
# title(main = "Predation observations", line = -1,
#       cex.main = 1,   font.main= 2, col.main= "black")
# plotModuleWeb(Bmod)
# title(main = "Bidirectional waiting time", line = -1,
#       cex.main = 1,   font.main= 2, col.main= "black")
# plotModuleWeb(Bmod_prey)
# title(main = "Unidirectional:Predator after prey", line = -1,
#       cex.main = 1,   font.main= 2, col.main= "black")
# plotModuleWeb(Bmod_pred)
# title(main = "Unidirectional:Prey after predator", line = -1,
#       cex.main = 1,   font.main= 2, col.main= "black")

## plot all graphs together: bipartite networks
par(mfrow=c(2,2))
plotweb(web4$site_s)
title(main = "Predation observations", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web7$site_s)
title(main = "Bidirectional waiting time", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web5$site_s)
title(main = "Unidirectional:Predator after prey", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
plotweb(web6$site_s)
title(main = "Unidirectional:Prey after predator", line = 0,
      cex.main = 1,   font.main= 2, col.main= "black")
par(new=T)
###there is a difference if we consider prey or predator first or second when we comput modularity!! 


