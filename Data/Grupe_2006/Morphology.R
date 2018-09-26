###################################################################################
#                                                                                ##
# Grupe Data Tidy - Morphology                                                   ##
# Data are current as of 2018-09-26                                              ##
# Data source: University of Oregon - Oregon Institute of Marine Biology         ##
# R code prepared by Ross Whippo (rosspitality@gmail.com)                        ##
# Last updated 2018-09-26                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY:

# Cleaning up and formatting data extracted from Benjamin Grupe's 2006 Masters 
# Thesis. Raw data extracted from unpublished thesis on 2018-09-21 by Ross Whippo 
# using the PDF table extraction tool Tabula 1.2.1 (https://tabula.technology/) 

# Required Files (check that script is loading latest version):
# /home/ross/Dropbox/Datasets/Tidepool_Urchins/Grupe_data/Morphology_Grupe_2006_Thesis.csv

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

# 2018-09-26 Script created from R data template

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

library(tidyverse)

###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

morpho_raw <- read_csv("~/Dropbox/Datasets/Tidepool_Urchins/Grupe_data/Morphology_Grupe_2006_Thesis.csv")

###################################################################################
# MANIPULATE DATA                                                                 #
###################################################################################

# Delete "Pd" column

morpho1 <- morpho_raw %>% 
  select(-one_of("Pd"))

# Separate "Di He" column into three columns
morpho_2 <- morpho_raw %>%
  separate(col = "Di He",
           into = c("Di", "He", "Pd"),
           sep = (" "))

# Separate "S2 S3" into two columns
morpho_3 <- morpho_2 %>%
  separate(col = "S2 S3",
           into = c("S2", "S3"),
           sep = (" "))

# Separate "Sp Ma Cs" into three columns
morpho_4 <- morpho_3 %>%
  separate(col = "Sp Ma Cs",
           into = c("Sp", "Ma", "Cs"),
           sep = (" "),
           extra = "merge")

# Separate "Cs" column into three column
morpho_5 <- morpho_4 %>%
  separate(col = "Cs",
           into = c("Cs", "Go2", "Gu2"),
           sep = (" "))

# Separate X9 column into two columns
morpho_6 <- morpho_5 %>%
  separate(col = "X9",
           into = c("X9", "X10"),
           sep = (" "))

# Unite X9 values w/ Cs column
morpho_7 <- morpho_6 %>%
  replace_na(list(Cs = "", X9 = "")) %>% 
  unite("Cs", c("Cs", "X9"), sep = "")

# Separate Go into Gu3
morpho_8 <- morpho_7 %>%
  separate(col = "Go",
           into = c("Go", "Gu3"),
           sep = (" "))

# Unite Go, Go2, and X10
morpho_9 <- morpho_8 %>%
  replace_na(list(Go2 = "", X10 = "", Go = "")) %>%
  unite("Go", c("Go2", "X10", "Go"), sep = "")

############### SUBSECTION HERE

#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#