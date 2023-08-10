######### Code for extracting waiting times between pairs of detections######### 
#
# Author: Tiziana A. Gelmi Candusso 
# Publication: Gelmi-Candusso, et al. (2023) Building urban predator-prey networks using camera traps. Food webs
#
# note: code uses demo data from repo tgelmi-candusso/predator_prey_networks
################################################################################

###libraries ###
library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyverse)


### read demo data ####
#the demo data consists of one site in Toronto, on a vacant lot next to a ravine, where
#the camera trap ran for 364 continuous days.

data <-  read.csv("demo_data.csv")

#set date + species name format
data$DateTime <- as.POSIXlt(data$DateTime, format="%Y-%m-%d %H:%M:%S") 
data$site_name <- factor(data$site_name)
data$common_name <- factor(data$Species)


##create list of predators and prey
preys <- c("dog", "cat", "chipmunk" , "mink", "deer","groundhog","oppossum","rabbit","raccoon","skunk","small rodent","squirrel","fox", "coyote")
predators <- c("coyote", "fox")
prey_predator <- c(preys,predators)

#create object to loop through 
sites <- levels(factor(data$site_name))

#create dataframe for loop
output_allsites <- data.frame()

#loop through predators and prey estimating waiting times between each other at each site.
for (predator in predators){
  for (prey in preys){
    species <- c(predator,prey)
    data1 <- data %>%
      filter(common_name %in% species)
    for (site in sites){
      data2 <- data1 %>%
        filter(site_name == site) %>% 
        dplyr::arrange(DateTime)
      if(length(unique(data2$common_name))>1){
        #keep species names
        combn(data2$common_name, 2, simplify=TRUE) -> m              
        m[1,] -> Species_1
        m[2,] -> Species_2
        rm(m)
        # ##keep station name (referred to as site_name in this .csv)
        combn(as.character(data2$DateTime),2, simplify=TRUE) -> p   
        p[1,] -> Date_1
        p[2,] -> Date_2
        rm(p)
        interval(Date_1,Date_2)->c                                            
        as.duration(c)->d
        as.numeric(d)/3600-> time_interval    #time interval in hours
        rm(c)
        rm(d)
        combn(as.character(data2$File),2, simplify=TRUE) -> p    
        p[1,] -> Filename_1
        p[2,] -> Filename_2
        rm(p)
        combn(as.character(data2$site_name),2, simplify=TRUE) -> p    
        p[1,] -> site_1
        p[2,] -> site_2
        rm(p)
        output_dataset_loc <- data.frame(Species_1, Species_2, Date_1, Date_2, time_interval, Filename_1, Filename_2, site_1, site_2)
        output_dataset_loc$site_name <- site
        ##filter for events where species 1 and 2 are dissimilar and time interval is within 3 hours
        output_dataset_loc <- output_dataset_loc %>% 
          filter(Species_1 != Species_2) %>%
          filter(time_interval<12)
        if(nrow(output_dataset_loc)>=1){
          output_allsites <- rbind(output_allsites, output_dataset_loc)
          }
        print(Sys.time())
      }
    }
  }
}

#save RDS for later use in case of crash
saveRDS(output_allsites, 'preypredatorcombnoutput_allsites_Ttfix_clean.rds')
#output_allsites <- readRDS('preypredatorcombnoutput_allsites_Ttfix_clean.rds')


