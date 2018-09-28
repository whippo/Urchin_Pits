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
# CLEAN UP DATA                                                                   #   
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
library(magrittr)
library(psych)


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

morpho_raw <- read_csv("~/Dropbox/Datasets/Tidepool_Urchins/Grupe_data/Morphology_Grupe_2006_Thesis.csv")

###################################################################################
# CLEAN UP DATA                                                                   #
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

# Unite Gu, Gu2, Gu3, and X12
morpho_10 <- morpho_9 %>%
  replace_na(list(Gu = "", Gu2 = "", Gu3 = "", X12 = "")) %>%
  unite("Gu", c("Gu", "Gu2", "Gu3", "X12"), sep = "")

# Separate La Ja into four columns
morpho_11 <- morpho_10 %>%
  separate(col = "La Ja",
           into = c("La", "Ja", "Sk2", "Te2"),
           sep = (" "))

# Unite Sk and Sk2
morpho_12 <- morpho_11 %>%
  replace_na(list(Sk = "", Sk2 = "")) %>%
  unite("Sk", c("Sk", "Sk2"), sep = "")

# Unite Te and Te2
morpho_13 <- morpho_12 %>%
  replace_na(list(Te = "", Te2 = "")) %>%
  unite("Te", c("Te", "Te2"), sep = "")

# make numbers numeric and the rest factors
str(morpho_13)
morpho_13[4:18] <- as.numeric(as.matrix(morpho_13[4:18]))
morpho_13$Si %<>% factor
morpho_13$Ti %<>% factor
morpho_13$Mi %<>% factor

Morpho_clean <- morpho_13 

# Si – Site
# Ti – Tidepool
# Mi - Microhabitat
# Di – Test diameter (cm)
# He – Test height (cm)
# Pd – Peristomial diameter (cm)
# S1 – 1 st Spine (cm)
# S2 – 2 nd Spine (cm)
# S3 – 3 rd Spine (cm)
# Sp – Average spine length (cm)
# Ma – Mass (g)
# Cs – Compression strength
# Go – Gonad mass
# Gu – Gut mass
# La – Lantern mass
# Ja – Jaw Length
# Sk – Skeletal mass
# Te – Test thickness

###################################################################################
# URCHIN VOLUME                                                                   #
###################################################################################

# Q: Is there a difference in urchin volume between pit and non-pit urchins?
# A: NO

Volume <- Morpho_clean

# Calculate volume column
Volume$Vo <- (4/3)*(pi*(((Volume$Di)/2)^3))

# Plot volume vs. habitat
ggplot(Volume, aes(Mi, Vo)) +
  geom_boxplot()

t.test(Vo ~ Mi, data = Volume)
# Welch Two Sample t-test
# data:  Vo by Mi
# t = 1.7991, df = 166.86, p-value = 0.07381

ggplot(Volume, aes(Mi, Vo)) +
  geom_boxplot() +
  facet_grid(~Si)

###################################################################################
# MORPHOLOGICAL CHARACTERS                                                        #
###################################################################################

# Q: How are morphological charaters of all urchins pooled related?
# A: Mass and length covary, jaw size seem asymptotic with mass.

# Pairs plot for all metrics
pairs.panels(Morpho_clean[4:18], 
             scale = TRUE, pch=21, ellipses = FALSE)

###################################################################################
# MORPHOLOGICAL CHARACTERS                                                        #
###################################################################################

# Q: Do any of the measured characters predict pit-dwelling?
# A: 

Morpho_binom <- Morpho_clean

# Make pit/non-pit into binomials (0,1)
Morpho_binom$Mi <- Morpho_binom$Mi %>%
  recode("P" = "1", "NP" = "0")
Morpho_binom$Mi <- as.numeric(as.character(Morpho_binom$Mi))

# Create null model for comparison
morph_mod_null <- glm(Mi ~ 1, data = Morpho_binom, family = binomial(link = "logit"))

# Full model
morph_mod_full <- glm(Mi ~., data = Morpho_binom, family = binomial(link = "logit"))

# Stepwise comparison
step(morph_mod_null,
     scope = list(upper = morph_mod_full),
     direction = "both",
     test = "Chisq",
     data = Morpho_binom)

# 'Best' model fit
morph_mod_fin <- glm(Mi ~ S2 + Ja + Sk + Ma + Gu + Pd, data = Morpho_binom, family = binomial(link = "logit"))


#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#