###################################################################################
#                                                                                ##
# Grupe Data Tidy - Growth                                                       ##
# Data are current as of 2018-10-15                                              ##
# Data source: University of Oregon - Oregon Institute of Marine Biology         ##
# R code prepared by Ross Whippo (rosspitality@gmail.com)                        ##
# Last updated 2018-10-15                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY:

# Cleaning up and formatting data extracted from Benjamin Grupe's 2006 Masters 
# Thesis. Raw data extracted from unpublished thesis on 2018-09-21 by Ross Whippo 
# using the PDF table extraction tool Tabula 1.2.1 (https://tabula.technology/) 

# Required Files (check that script is loading latest version):
# /home/ross/Dropbox/Datasets/Tidepool_Urchins/Grupe_data/Growth_Grupe_2006_Thesis.csv

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

# 2018-10-15 Script created from R data template

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

library(tidyverse)
library(magrittr)
library(psych)


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

growth_raw <- read_csv("~/Dropbox/Datasets/Tidepool_Urchins/Grupe_data/Growth_Grupe_2006_Thesis.csv")

###################################################################################
# CLEAN UP DATA                                                                   #
###################################################################################

levels(growth_raw$X11)

# Delete "X11" column
growth_1 <- growth_raw %>%
  select(-one_of("X11"))

# Separate "Ti" column into two columns
growth_2 <- growth_1 %>%
  separate(col = "Ti",
           into = c("Ti", "Mi2"),
           sep = (" "))

# Copy Mi column rows 1-16 to Mi2 column rows 1-16
growth_3 <- growth_2
growth_3[1:16, 3] <- growth_2[1:16, 4]

# Rename X12 to 'Sex'
growth_4 <- growth_3
names(growth_4)[names(growth_4)=="X12"] <- "Sex"

# duplicate column # so the original can be overwritten
growth_5 <- growth_4 
growth_5$'#1' <- growth_5$'#'

#### Skip ahead for sex ratio analysis
#### BELOW WAS PREVIOUS ATTEMPT

# Separate 'EsGr LaGr' values and rename 'Years Sex' as 'Years'
growth_5 <- growth_4 %>%
  separate(col = "EsGr LaGr",
           into = c("EsGr", "LaGr", "Years1"), 
           sep = (" "))
names(growth_5)[names(growth_5)=="Years Sex"] <- "Years"

# Unite Years1 values w/ Years column
growth_6 <- growth_5 %>%
  replace_na(list(Years1 = "", Years = "")) %>% 
  unite("Years", c("Years1", "Years"), sep = "")

# Separate Jaw0 TotGr into 5 columns
growth_7 <- growth_6 %>%
  separate(col = "Jaw0 TotGr",
           into = c("Jaw0", "TotGr", "EsGr1", "LaGr1", "Years1"),
           sep = (" "))

# Unite new Years1 values w/ Years column
growth_8 <- growth_7 %>%
  replace_na(list(Years1 = "")) %>% 
  unite("Years", c("Years1", "Years"), sep = "")

# Separate Jaw1 into 5 columns
growth_7 <- growth_6 %>%
  separate(col = "Jaw0 TotGr",
           into = c("Jaw1", "Jaw0", "TotGr", "EsGr1", "LaGr1", "Years1"),
           sep = (" "))

##### BELOW THIS IS MORPHO CODE TO BE CANNIBALIZED

# more code!!!!!

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
# Mi – Microhabitat (P or NP)
# Di – Diameter
# Jaw1 – Jaw size at collection
# TotGr – Total growth
# LaGr – Labial growth
# Sex – M(ale) or F(emale)
# Ti – Tidepool
# # – ID Number
# He – Height
# Jaw0 – Jaw size at tagging
# EsGr – Esophageal Growth
# Ye – Years of growth

###################################################################################
# SEX RATIOS                                                                      #
###################################################################################

# Q: Is there a difference in urchin sex-ratio between pit and non-pit urchins?
# A: 

sexratio <- growth_5 %>%
  select(Si, Ti, Mi2, Sex)

sexratio[is.na(sexratio)] <- "U"

# remove header rows
sexratio <- sexratio %>%
  subset(Sex != "Sex")

# Plot sex vs. habitat
ggplot(sexratio, aes(Sex)) +
  geom_bar() +
  facet_grid(Si ~ Mi2)

x[is.na(x)] <- 0# make males 0, females 1
sexratio$SexNum <- sexratio$Sex %>%
  recode("F" = "1", "M" = "0")
sexratio$SexNum <- as.numeric(sexratio$SexNum)

model <- glm(SexNum ~ Mi2 + Si, family=binomial, data = sexratio)
summary(model)
# Call:
# glm(formula = SexNum ~ Mi2 + Si, family = binomial, data = sexratio)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.1647  -0.9702  -0.9468   1.2163   1.4270  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -0.50901    0.15894  -3.203  0.00136 **
#   Mi2P         0.47895    0.14671   3.265  0.00110 **
#   SiMC        -0.06088    0.18873  -0.323  0.74701   
# SiSC        -0.03718    0.19698  -0.189  0.85030   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)



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

#SCRATCH PAD

# mean test diameter by site and type of urchin
Morpho_clean %>%
  group_by(Si, Mi) %>%
  summarize(mean(Di))

# mean test height by site and type of urchin
Morpho_clean %>%
  group_by(Si, Mi) %>%
  summarize(mean(He))

# mean spine lengths
Morpho_clean %>%
  group_by(Si, Mi) %>%
  summarize(mean(S1:S3))
