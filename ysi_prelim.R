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

sum(is.na(dat1$lake)) #2 entries have NA for lake

dat1[374,1] <- "H1A"
dat1[375,1] <- "H1A" #fill in the correct lake by referencing GPS locations, times
dat1[38:43,2] <- "37 16.043" #separate profiles by lat

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
  group_by(date, lake, lat) %>% #group into profiles
  filter(n()>2) #select only profiles that have at least 3 measurements
  #and if you want to use only final 6 lakes # %>% filter(lake != "Knob")

plot2 <- ggplot(data= dat2)+
  geom_point(aes(x=z, y=pctDO))+
  facet_wrap(~lake + date, scales = "free")+
  theme_classic()

plot2 #percent DO vs. depth for each profile 

plot3 <- ggplot(data = dat2)+
  geom_point(aes(x=z, y=mgDO))+
  facet_wrap(~lake + date, scales = "free")+
  theme_classic()

plot3 #mg/L DO vs. depth for each profile

##############
### playing with just one sample date / lake - untangling patterns
##############

dat3 <- dat2 %>% 
  filter(lake == "H2P", date == "3-Aug-19")

plot4 <- ggplot(data = dat3)+
  geom_point(aes(x=z, y=pctDO))+
  facet_grid(rows=vars(lat), cols = NULL)+
  theme_classic()

plot4


plot5 <- ggplot(data= dat2)+
  geom_point(aes(x=z, y=pctDO))+
  facet_wrap(~lake + lat + date, scales = "free", drop = TRUE, 
             labeller = labeller(~lake + date, .multi_line = FALSE))+
  theme_void()

plot5
