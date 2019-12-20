#################################################
### Water Column Nutrient Analyses ##############
### Data from MSI 12 Dec 2019      ##############
#################################################

###########################################################################
### Install packages / setup ###############
###########################################################################

#Required packages

library(tidyverse)
library(lubridate)
library(calecopal)
library(stats)

#Import data from CSV file. Concentrations given are in micromolar.

MSIdat <- read_csv("G:/My Drive/UCSB/Sierra_2019_data/sierra19/NutrientData_12Dec19.csv") %>% 
  na.omit() %>% #remove a blank row
  rename(SampleID = `Sample ID`, NN = NitriteNitrate) %>% #tidy column names
  mutate(SampleID = as.character(SampleID)) #must be character to join with other table


#Import metadata from analytical lab submission.

metadat <- read_csv("G:/My Drive/UCSB/Sierra_2019_data/sierra19/nutrient_metadata_2019.csv")  %>% 
  select(-Lake_1, -Date_1) %>% 
  mutate(Label = as.character(Label), Date = dmy(Date))


#Link tables by sample number

dat <- MSIdat %>% 
  left_join(metadat, by= c("SampleID" = "Label")) %>% 
  select(Lake, SampleID, Phosphate, Nitrite, NN, Date
         #, Time, Depth, Substrate
         ) %>% 
  mutate(Fish = fct_recode(as.factor(Lake %>% substr(3,3)), `0` = "A", `1` = "P"), 
         Block = as.factor(Lake %>% substr(2,2)))

##################################################################################
### Plot nutrients
##################################################################################
rm(lks, plotdat, phosplot)
# facet colors from stackoverflow https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid
plotdat <- dat %>% 
  na.omit(Lake)

lks <- unique(plotdat[,"Lake"]) 
lks$Date <- rep(plotdat$Date[1],6)
lks$Phosphate <- lks$Nitrite <- lks$NN <- lks$Fish <- 0



phosplot <- ggplot(plotdat, aes(x = Date, y = Phosphate))+
 geom_rect(data=lks, aes(fill = Lake), xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = 0.3)+
#arrange colors so that warmer colors are warmer lakes and fish are darker than no fish - colorhex.com
  scale_fill_manual(values = c("#feecc4","#feecc4","#a5d0d7","#a5d0d7","#9cadaf","#9cadaf" ),
                    name = "Climate", labels = c("Cold", "Intermediate", "Warm"), breaks = c("H1A", "H2A", "H3A"))+
  geom_point(data = plotdat, aes(shape = Fish))+
  scale_shape_manual(values = c(1, 19), name = "Fish  status", labels = c("Absent", "Present"))+
  facet_wrap(~Lake, nrow = 3, scales = "free")+
  theme(strip.background = element_blank(), legend.key = element_blank())+ 
  ggtitle(label = "2019 phosphate concentrations (micromolar)")+
  labs(xlab = "Phosphate concentrations (uM)")
  

phosplot

nitrplot <- dat %>% 
  na.omit(Lake) %>% 
  ggplot(aes(x = Date, y = Nitrite))+
#adjust asthetics of geom_point to have diff markers for fish/fishless
  geom_point()+
  geom_rect(data=lks, aes(fill = Lake), xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = 0.3)+
#arrange colors so that warmer colors are warmer lakes and fish are darker than no fish - colorhex.com
  scale_fill_manual(values = c("#feecc4", "#FDD989", "#a5d0d7", "#4CA2B0", "#9cadaf", "#395B5F"))+
  facet_wrap(~Lake, nrow = 3, scales = "free")+
  theme(legend.position = "none")+
  #theme_classic()+
  ggtitle(label = "Seasonal nitrite concentrations (micromolar)")

nitrplot

nnplot <- dat %>% 
  na.omit(Lake) %>% 
  ggplot(aes(x = Date, y = NN, col = Block))+
  geom_point()+
  facet_wrap(~Lake, nrow = 3)+
  theme(#panel.background = element_blank(), #(fill = cal_palette("lake")[3:5]),
        #strip.background = element_rect(fill = cal_palette("lake")[4]), #want to color separately by block (factor) 
        legend.position = "none")+
  ggtitle(label = "Seasonal nitrite + nitrate concentrations (micromolar)")

nnplot

h2plot <- dat %>%
    na.omit(Lake) %>%
    filter(Block == 2) %>% 
    pivot_longer(c(Phosphate, Nitrite, NN), names_to = "Nutrient", values_to = "conc_uM") %>% 
  ggplot(aes(x = Date, y= conc_uM, color = Fish))+
    geom_point()+
    facet_wrap(~Nutrient, nrow = 3, scales = "free")+
    #legend(x = "left", y = 2.25, title = "Fish Status", legend = c("Absent","Present"), trace = TRUE)+
  #ERROR: Don't know how to add o to a plot
    ggtitle(label = "H2 pair: seasonal nutrient concentrations")
 
h2plot 


# theme(strip.background = element_rect(fill = "white", color = "grey50"),
  #     legend.title = element_text("Fish Status"),
  #     legend.text = element_text(c("Absent", "Present")),
  #     axis.title.y = element_text("Concentration (uM)"))+
  


#######################################################################################
### Conduct preliminary statistics
#######################################################################################


### Is the mean nutrient level different

## across Fish/No Fish?

## across blocks (1, 2, 3)?

## over time during the season?
##    if you take fish status or block into account?

lakdat <- dat %>% 
  select(Fish, Block, Date, Phosphate, Nitrite, NN) %>% 
  filter(!is.na(Date)) %>% 
  group_by(Block)

ar1 <- ar(lakdat$NN)

pairwise.t.test(lakdat$NN, lakdat$Fish)
