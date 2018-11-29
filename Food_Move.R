###################################################################################
#                                                                                ##
# Tagged Urchin Response to Food                                                 ##
# Data are current as of 2018-11-05                                              ##
# Data source: Ross Whippo PhD - OIMB/UO                                         ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2018-11-08                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY: Analysis of response to food cue by purple sea urchins tagged with
# passive integrated transponders (PIT) tags and without. 


# Required Files (check that script is loading latest version):
# 2018-11-05_urchin_food_response.csv

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

# 2018-11-08 Script created

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

library(tidyverse)
library(lubridate)


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

Urchin_food_move <- read.csv("~/Git/Urchin_Pits/Data/Lab_Movement/Food_trials/2018-11-05_urchin_food_response.csv")

str(Urchin_food_move)

Urchin_food_move$duration <- as.numeric(as.period(ms(Urchin_food_move$duration), unit = "sec"))

###################################################################################
# PLOTS                                                                           #
###################################################################################

ggplot(Urchin_food_move, aes(x = tagged, y = duration)) +
  geom_boxplot()

# time w/ diameter
ggplot(Urchin_food_move, aes( x = diameter, y = duration, color = tagged)) +
  geom_point() +
  theme_minimal()



############### SUBSECTION HERE

#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#