##############
### setup
##############

library(tidyverse)

##############
### read in raw 
##############

dat <- read_csv("G:/My Drive/UCSB/Sierra_2019_data/YSI/YSIraw.csv")

##############
### clean up for visualization
##############

dat1 <- dat %>% 
  transmute(lake = as.factor(Lake),
         lat = Lat,
         lon = Long,
         date = Date,
         time = Time,
         z = Depth,
         temp = `Temperature C`,
         pctDO = `Percent DO`,
         mgDO = `DO (mg/L)`) 


##############
### visualization
##############

plot1 <- ggplot(data = dat1)+
  geom_point(aes(x = z, y = pctDO))+
  facet_wrap(~lake + date, scales = "free")

plot1 #this shows roughly 20 profiles taken over the course of the season plus several other spot measurements

##############
### select just the profiles, investigate further
##############

dat2 <- dat1 %>% 
  group_by(date + lake) %>% #group into profiles
  slice() #select only profiles that have at least 5 measurements
  
  