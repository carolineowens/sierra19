#################################################
### Emerging Insect Data ########################
#################################################

###########################################################################
### Install packages / setup ###############
###########################################################################

#Required packages

library(tidyverse)
library(lubridate)
library(calecopal)
library(stats)
library(janitor)

#Import data from CSV file.
View(rawdat)
rawdat <- read_csv("G:/My Drive/UCSB/Sierra_2019_data/Emergence_traps/f19_emerg_counts.csv") %>% 
  clean_names() %>% 
  select(lake, sample_date, trap, notes, total, contents = "rough_estimate_of_contents") %>% 
  separate(contents, into = c("a", "b", "c", "d", "e", "f", "g", "h"), sep = "([,])") %>% 
  rownames_to_column(var="sampleID") %>% 
  filter(is.na(total)==FALSE)

#get count and type separate for each column

each <- c("a", "b", "c", "d", "e", "f", "g", "h")
classes <- NULL

for (i in 1:length(each)){
  temp <- each[i]
  
  dat_a <- rawdat %>% 
  select(sampleID, temp) %>% 
  rename(strings = temp) %>% 
  mutate(strings = trimws(strings, which="left")) %>% 
  separate(strings, into = c("count", "class"), extra = "merge")
  
  classes <- rbind(classes, dat_a)
}

#sometimes number and class are reversed - how to reorder? *CO edited CSV file rows 121-123 to fix this

#checking whether it is safe to remove all NA values - TRUE and FALSE in each position
# classes <- classes %>% 
#   filter(is.na(count) == TRUE & is.na(class) == FALSE)

classes <- na.omit(classes) 
rm(dat_a, temp, i)
#########

dat <- rawdat %>% 
  select(sampleID, total) %>% 
  mutate(mosq = NA, chiron = NA, dipt = NA, colep = NA, hemip = NA, hym = NA, other = NA)


# Daniel's code: partial string matching
classes$tidyclass <- NA
tcls <- c("mosq", "chir", "midge", "dip", "cole", "hemip", "hym")

for (x in 1:length(tcls)){
  y = tcls[x]
for (i in 1:nrow(classes)){
  if(grepl(y, classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- y
  } else {}
  if(grepl("culic", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "mosq"
  }else {}
  if(grepl("ephem", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "mayfly"
  }else {}
  if(grepl("mayf", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "mayfly"
  }else {}
  if(grepl("sim", classes$class, ignore.case = TRUE)[i] == TRUE) {
    classes$tidyclass[i] <- "simul"
  }else {}
  if(grepl("cadd", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "trichop"
  }else {}
  if(grepl("trich", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "trichop"
  }else {}
  if(grepl("beetle", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "cole"
  }else {}
  if(grepl("spider", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "arach"
  }else {}
  if(grepl("aran", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "arach"
  }else {}
    if(grepl("blackfly", classes$class)[i] == TRUE) {
      classes$tidyclass[i] <- "sim"
  }else {}
  if(grepl("crane", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "dip"
  }else {}
  if(grepl("tach", classes$class)[i] == TRUE) {
    classes$tidyclass[i] <- "dip"
  }else {}
  if(classes$class[i] == "fly" |classes$class[i] == "fly " |classes$class[i] == "flies" 
     |classes$class[i] == "flies ") {
    classes$tidyclass[i] <- "sim"
  }else{}
  if(isTRUE(classes$class[i] == "flies")) {
    classes$tidyclass[i] <- "sim"
  }
}}

#now want to spread data

classwide <- classes %>% 
  filter(is.na(tidyclass)==FALSE) %>% 
  select(-class) %>% 
  group_by(sampleID)

classwide$count <- as.integer(classwide$count)
classwide$tidyclass <- as.factor(classwide$tidyclass)
classwide$sampleID <- as.factor(classwide$sampleID)

classwide <- classwide %>% 
  pivot_wider(names_from = tidyclass, values_from = count, values_fn = list(count=sum))
#This is working!!!! but counting exuvia as live... fix this upstream  