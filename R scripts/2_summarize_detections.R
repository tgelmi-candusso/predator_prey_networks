######### Code for generating summary tables with detections per site ######### 
#
# Author: Tiziana A. Gelmi Candusso 
# Publication: Gelmi-Candusso, et al. (2023) Building urban predator-prey networks using camera traps. Food webs
#
# note: code uses demo data from repo tgelmi-candusso/predator_prey_networks
#       code to summarize data at each site, it will output a table with a row per site, in 
#       case of the demo_Data, there will be only one code.
################################################################################


## libraries ###
library(lubridate)
library(dplyr)

#load data
data <-  read.csv("data/demo_data.csv")

#set datetime in the right format
as.POSIXlt(data$DateTime, format="%Y-%m-%d %H:%M:%S") -> data$DateTime
as.POSIXlt(data$date_start, format="%Y-%m-%d %H:%M:%S") -> data$date_start
as.POSIXlt(data$date_end, format="%Y-%m-%d %H:%M:%S") -> data$date_end
site_time <- data %>% dplyr::select(site_name, date_start, date_end)
site_time <- unique(site_time)
#in case unique doesnt work
#site_time[!duplicated(site_time$site_name), ]

#site_time %>% interval(date_start, date_end)
trap_night <- site_time$date_end - site_time$date_start
site_time$trap_night <- as.data.frame(trap_night)

##this will divide presence summary for each species in more rows if there were
#multiple detections
data_s <- data %>% select(site_name, Species, Nspecies) %>% 
  filter(Species !="")
data_s %>% dplyr::count(Species, Nspecies)

##number of frame detections per sites (includes the possibility of more detections per day)
data_s <- data %>% select(site_name, Species, Nspecies) %>% 
  filter(Species !="")
data_s_sp <- data_s %>% dplyr::count(site_name, Species)
list_sites <- split(data_s_sp, f=factor(data_s_sp$site_name), drop = TRUE)
list_sites <- lapply(list_sites, function(x) x[2:3])
list_sites1 <- lapply(list_sites, function(x) data.frame(t(x)))
list_sites1 <- list_sites1 %>%map(~ remove_rownames(.))
list_sites1 <- lapply(list_sites1, function(x) {names(x)<-as.character(x[1,]);x})
list_sites1 <- lapply(list_sites1, function(x) x[2,])
df <- bind_rows(list_sites1, .id="id")
#dataset2 <-df %>% dplyr::select(-"empty", -"not listed", -"horse") 
dataset2 <-  df
dataset2[is.na(dataset2)] <- 0
dataset2<- dataset2 %>% select("id", "coyote", "fox", "dog", "cat", "squirrel", "rabbit", "raccoon", "skunk", "oppossum", "deer")
colnames(dataset2)<- c("site_name", "coyote", "fox", "dog", "cat", "squirrel", "rabbit", "raccoon", "skunk", "opossum", "deer")

##number of days with detections per site:
data_s <- data %>% select(site_name, Species, DateTime) %>% 
  filter(Species !="")
data_s$date<- date(data_s$DateTime) 
data_s <- data_s %>% select(-DateTime)
data_s <-  unique(data_s)
data_s_sp <- data_s %>% dplyr::count(site_name, Species)
list_sites <- split(data_s_sp, f=factor(data_s_sp$site_name), drop = TRUE)
list_sites <- lapply(list_sites, function(x) x[2:3])
list_sites1 <- lapply(list_sites, function(x) data.frame(t(x)))
list_sites1 <- list_sites1 %>%map(~ remove_rownames(.))
list_sites1 <- lapply(list_sites1, function(x) {names(x)<-as.character(x[1,]);x})
list_sites1 <- lapply(list_sites1, function(x) x[2,])
df <- bind_rows(list_sites1, .id="id")
#dataset <-df %>% dplyr::select(-"empty", -"not listed", -"horse") 
dataset <- df
dataset[is.na(dataset)] <- 0
dataset<- dataset %>% select("id", "coyote", "fox", "dog", "cat", "squirrel", "rabbit", "raccoon", "skunk", "oppossum", "deer")
colnames(dataset)<- c("site_name", "coyote", "fox", "dog", "cat", "squirrel", "rabbit", "raccoon", "skunk", "opossum", "deer")

#clean deployment table and add XY for visualization in arcgis if available (not available in demo code)
site_time$start <- date(site_time$date_start)
site_time$end <- date(site_time$date_end)
site_time <- site_time %>% select(-"date_start", -"date_end")
#add XY data if available:
# sites_xy <- read.csv("sites_XY2.csv")
# site_time <- left_join(site_time, sites_xy, by="site_name")

#join detections and total deployment info
summ_table <- left_join(site_time, dataset, by="site_name")
summ_table2 <- left_join(site_time, dataset2, by="site_name")

#save summary tables
write.csv(summ_table, "data/summary_table_days_xy.csv")
write.csv(summ_table2, "data/summary_table_detections_xy.csv")

