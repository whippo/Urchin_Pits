###################################################################################
#                                                                                ##
# Tagged Urchin Initial Movement Response                                        ##
# Data are current as of 2018-11-29                                              ##
# Data source: Ross Whippo PhD - OIMB/UO                                         ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2018-11-29                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY: Analysis of initial movement response by purple sea urchins tagged with
# passive integrated transponders (PIT) tags and those without. Data derived from
# manaual point tracking analysis in ImageJ software.


# Required Files (check that script is loading latest version):
# 201811_Urchin_PIT_tagging_lab_movement_pilot.csv

# Associated Scripts:
# none

# TO DO

###################################################################################
# TABLE OF CONTENTS                                                               #
#                                                                                 #
# RECENT CHANGES TO SCRIPT                                                        #
# LOAD PACKAGES                                                                   #
# READ IN AND PREPARE DATA                                                        #
# MANIPULATE DATA                                                                 #   
#                                                                                 #
###################################################################################

###################################################################################
# RECENT CHANGES TO SCRIPT                                                        #
###################################################################################

# 2018-11-29 Script created

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

library(tidyverse)
library(lubridate)
library(viridis)


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

Urchin_tag_move <- read.csv("~/Git/Urchin_Pits/Data/Lab_Movement/Initial_tagging/201811_Urchin_PIT_tagging_lab_movement_pilot.csv")

str(Urchin_tag_move)

# make bin and urchin numbers into factors
Urchin_tag_move$Bin <- as.factor(as.character(Urchin_tag_move$Bin))
Urchin_tag_move$Urchin <- as.factor(as.character(Urchin_tag_move$Urchin))

# remove negative values
Urchin_tag_move_pos <- Urchin_tag_move %>%
  subset(Distance > -0.0001)

# sum frames w/ movement
Urchin_tag_move_pos$movement <- Urchin_tag_move_pos$Distance %>%
  mutate()

###################################################################################
# PLOTS                                                                           #
###################################################################################

# patterns of movement through time
ggplot(Urchin_tag_move_pos, aes(x = Slice.n., y = Distance, color = Tag, fill = Tag)) +
  geom_col() +
  #facet_wrap(~Tag)+
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "B") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "B") +
  theme_minimal() +
  xlab("minutes") +
  ylab("distance (cm)") +
  theme(text = element_text(size=20))

ggsave("20181129_tagmove_initial_timeseries", device = "png", width = 8, height = 7, units = 'in', dpi = 500)

# mean movement
ggplot(Urchin_tag_move_pos, aes(x = Tag, y = Distance, color = Tag)) +
  geom_boxplot() +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "B") + 
  ylab("distance (cm)") +
  xlab(NULL) +
  theme_minimal() +
  theme(text = element_text(size=20)) 
  
ggsave("20181129_tagmove_initial_medians", device = "png", width = 8, height = 7, units = 'in', dpi = 500)

# time spent moving




############### SUBSECTION HERE

#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#