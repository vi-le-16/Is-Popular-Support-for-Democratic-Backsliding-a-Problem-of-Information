library(viridis)
library(hrbrthemes)
library(HH)
library(Rmpfr)
library(reshape)
library(tidyr)
library(dplyr) 
library(data.table)
library(srvyr)
library(survey)
library(cli)
library(gtsummary)
library(ggplot2)
library(stargazer)
library(margins)
library(sjPlot)
library(sjmisc)
library(modelsummary)
library(svyVGAM)
library(ggeffects)
library(nnet)
library(MASS) 
library(ggpubr)
library(psych)
library(scales)
library(stringr)
library(gridExtra)
library(grid)
library(readxl)
library(tableone)
library(formattable)
library(htmlwidgets)
library(FSA)

## set working space ## 

setwd("~/Documents/For Laura/HU Media Survey Project/Data")

##read xlsx files in##

hu_survey <- read_excel("MediaSurvery_2024_EN.xlsx")

#converting 98's and 99's to NAs#

hu_survey <- hu_survey %>%
  mutate(across(everything(), ~ ifelse(. %in% c(98, 99), NA, .)))

##### setting up variables for future modeling ##### 

hu_survey$DEM1 <- as.numeric(hu_survey$DEM1)
hu_survey$DEM2 <- as.numeric(hu_survey$DEM2)
hu_survey$DEM3 <- as.numeric(hu_survey$DEM3)
hu_survey$DEM4 <- as.numeric(hu_survey$DEM4)
hu_survey$DEM5 <- as.numeric(hu_survey$DEM5)
hu_survey$HHD <- as.numeric(hu_survey$HHD)
hu_survey$POL1 <- as.numeric(hu_survey$POL1)
hu_survey$INK <- as.numeric(hu_survey$INK)
hu_survey$IND <- as.numeric(hu_survey$IND)
hu_survey$DEM6 <- as.numeric(hu_survey$DEM6)

hu_survey <- hu_survey %>%
  mutate(fidesz_or_not = ifelse(POL2 == 1, 1, 0)) # 1 = Pro Fidesz; 0 = non-Fidesz 

hu_survey <- hu_survey %>% 
  mutate(gender = 
           dplyr::case_when(
             DEM1 <= 1 ~ 1,  # male 
             DEM1 > 1 ~ 0.   # female 
           )
  )

hu_survey <- hu_survey %>% 
  mutate(education_level = 
           dplyr::case_when(
             DEM4 >= 3 ~ 1,  # high education
             DEM4 < 3 ~ 0.   # low education 
           )
  )

## set up political leaning tripartite variable ## 

summary(hu_survey$POL1)
table(hu_survey$POL1)

hu_survey <- hu_survey %>% 
  mutate(pol_lean = 
           dplyr::case_when(
             POL1 > 5 ~ 2,  # right leaning
             POL1 > 4 ~ 1,  # centrist 
             POL1 > 0 ~ 0.  # left leaning  
           )
  )

table(hu_survey$pol_lean) #0 == 476; 1 == 570; 2 == 857 

##fidesz overlap with each political leaning ## 

hu_survey %>%
  filter(fidesz_or_not == 1, pol_lean == 2) %>%
  nrow() #540

hu_survey %>%
  filter(fidesz_or_not == 1, pol_lean == 1) %>%
  nrow() #60

hu_survey %>%
  filter(fidesz_or_not == 1, pol_lean == 0) %>%
  nrow() #12 

## set up pre-treatment responses ## 

hu_survey <- hu_survey %>% 
  mutate(TRU_1 = 
           dplyr::case_when(
             TRU1 >= 5 ~ 1,   #a lot of confidence in Fidesz
             TRU1 > 3 ~ 0.75,
             TRU1 > 2 ~ 0.50,
             TRU1 > 1 ~ 0.25, 
             TRU1 > 0 ~ 0.    #no confidence at all in Fidesz
           ))

### RECALIBRATE DOSAGE GROUPS ###

#RAND1# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND1 = 
           dplyr::case_when(
             RAND1 >= 2 ~ 0, #more information, dosage 2
             RAND1 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND1 = case_when(
      dosage_group_RAND1 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND1 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND1 = factor(dosage_group_RAND1, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND2# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND2 = 
           dplyr::case_when(
             RAND2 >= 2 ~ 0, #more information, dosage 2
             RAND2 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND2 = case_when(
      dosage_group_RAND2 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND2 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND2 = factor(dosage_group_RAND2, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND3# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND3 = 
           dplyr::case_when(
             RAND3 >= 2 ~ 0, #more information, dosage 2
             RAND3 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND3 = case_when(
      dosage_group_RAND3 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND3 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND3 = factor(dosage_group_RAND3, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND4# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND4 = 
           dplyr::case_when(
             RAND4 >= 2 ~ 0, #more information, dosage 2
             RAND4 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND4 = case_when(
      dosage_group_RAND4 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND4 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND4 = factor(dosage_group_RAND4, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND5# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND5 = 
           dplyr::case_when(
             RAND5 >= 2 ~ 0, #more information, dosage 2
             RAND5 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND5 = case_when(
      dosage_group_RAND5 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND5 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND5 = factor(dosage_group_RAND5, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND6# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND6 = 
           dplyr::case_when(
             RAND6 >= 2 ~ 0, #more information, dosage 2
             RAND6 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND6 = case_when(
      dosage_group_RAND6 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND6 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND6 = factor(dosage_group_RAND6, levels = c("Dosage 1", "Dosage 2"))
  )

#RAND7# 

hu_survey <- hu_survey %>% 
  mutate(dosage_group_RAND7 = 
           dplyr::case_when(
             RAND7 >= 2 ~ 0, #more information, dosage 2
             RAND7 > 0 ~ 1.  #less information, dosage 1
           ))

hu_survey <- hu_survey %>%
  mutate(
    dosage_group_RAND7 = case_when(
      dosage_group_RAND7 %in% c(1) ~ "Dosage 1",
      dosage_group_RAND7 %in% c(0) ~ "Dosage 2",
      TRUE ~ NA_character_
    ),
    dosage_group_RAND7 = factor(dosage_group_RAND7, levels = c("Dosage 1", "Dosage 2"))
  )

#### RECALIBRATE DEPENDENT VARIABLES #####

#RAND1_1# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_1 =
           dplyr::case_when(
             TRE1_1 >= 4 ~ 1,
             TRE1_1 > 2 ~ 0.75,
             TRE1_1 > 1 ~ 0.5, 
             TRE1_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_2 = 
           dplyr::case_when(
             TRE1_2 >= 5 ~ 1,
             TRE1_2 > 3 ~ 0.75,
             TRE1_2 > 2 ~ 0.5,
             TRE1_2 > 1 ~ 0.25, 
             TRE1_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_3 = 
           dplyr::case_when(
             TRE1_3 >= 5 ~ 1,
             TRE1_3 > 3 ~ 0.75,
             TRE1_3 > 2 ~ 0.5,
             TRE1_3 > 1 ~ 0.25, 
             TRE1_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_4 = 
           dplyr::case_when(
             TRE1_4 >= 5 ~ 1,
             TRE1_4 > 3 ~ 0.75,
             TRE1_4 > 2 ~ 0.5,
             TRE1_4 > 1 ~ 0.25, 
             TRE1_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_5 = 
           dplyr::case_when(
             TRE1_5 >= 2 ~ 0, 
             TRE1_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_1_6 = 
           dplyr::case_when(
             TRE1_6 >= 5 ~ 0,
             TRE1_6 > 3 ~ 0.25,
             TRE1_6 > 2 ~ 0.5,
             TRE1_6 > 1 ~ 0.75, 
             TRE1_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_1 =
           dplyr::case_when(
             TRE2_1 >= 4 ~ 1,
             TRE2_1 > 2 ~ 0.75,
             TRE2_1 > 1 ~ 0.5, 
             TRE2_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_2 = 
           dplyr::case_when(
             TRE2_2 >= 5 ~ 1,
             TRE2_2 > 3 ~ 0.75,
             TRE2_2 > 2 ~ 0.5,
             TRE2_2 > 1 ~ 0.25, 
             TRE2_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_3 = 
           dplyr::case_when(
             TRE2_3 >= 5 ~ 1,
             TRE2_3 > 3 ~ 0.75,
             TRE2_3 > 2 ~ 0.5,
             TRE2_3 > 1 ~ 0.25, 
             TRE2_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_4 = 
           dplyr::case_when(
             TRE2_4 >= 5 ~ 1,
             TRE2_4 > 3 ~ 0.75,
             TRE2_4 > 2 ~ 0.5,
             TRE2_4 > 1 ~ 0.25, 
             TRE2_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_5 = 
           dplyr::case_when(
             TRE2_5 >= 2 ~ 0, 
             TRE2_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND1_2_6 = 
           dplyr::case_when(
             TRE2_6 >= 5 ~ 0,
             TRE2_6 > 3 ~ 0.25,
             TRE2_6 > 2 ~ 0.5,
             TRE2_6 > 1 ~ 0.75, 
             TRE2_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND1_Q1_combined = coalesce(RAND1_1_1, RAND1_2_1))

hu_survey <- hu_survey %>%
  mutate(RAND1_Q2_combined = coalesce(RAND1_1_2, RAND1_2_2))

hu_survey <- hu_survey %>%
  mutate(RAND1_Q3_combined = coalesce(RAND1_1_3, RAND1_2_3))

hu_survey <- hu_survey %>%
  mutate(RAND1_Q4_combined = coalesce(RAND1_1_4, RAND1_2_4))

hu_survey <- hu_survey %>%
  mutate(RAND1_Q5_combined = coalesce(RAND1_1_5, RAND1_2_5))

hu_survey <- hu_survey %>%
  mutate(RAND1_Q6_combined = coalesce(RAND1_1_6, RAND1_2_6))

#RAND2# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_1 =
           dplyr::case_when(
             TRE3_1 >= 4 ~ 1,
             TRE3_1 > 2 ~ 0.75,
             TRE3_1 > 1 ~ 0.5, 
             TRE3_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_2 = 
           dplyr::case_when(
             TRE3_2 >= 5 ~ 1,
             TRE3_2 > 3 ~ 0.75,
             TRE3_2 > 2 ~ 0.5,
             TRE3_2 > 1 ~ 0.25, 
             TRE3_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_3 = 
           dplyr::case_when(
             TRE3_3 >= 5 ~ 1,
             TRE3_3 > 3 ~ 0.75,
             TRE3_3 > 2 ~ 0.5,
             TRE3_3 > 1 ~ 0.25, 
             TRE3_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_4 = 
           dplyr::case_when(
             TRE3_4 >= 5 ~ 1,
             TRE3_4 > 3 ~ 0.75,
             TRE3_4 > 2 ~ 0.5,
             TRE3_4 > 1 ~ 0.25, 
             TRE3_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_5 = 
           dplyr::case_when(
             TRE3_5 >= 2 ~ 0, 
             TRE3_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_3_6 = 
           dplyr::case_when(
             TRE3_6 >= 5 ~ 0,
             TRE3_6 > 3 ~ 0.25,
             TRE3_6 > 2 ~ 0.5,
             TRE3_6 > 1 ~ 0.75, 
             TRE3_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_1 =
           dplyr::case_when(
             TRE4_1 >= 4 ~ 1,
             TRE4_1 > 2 ~ 0.75,
             TRE4_1 > 1 ~ 0.5, 
             TRE4_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_2 = 
           dplyr::case_when(
             TRE4_2 >= 5 ~ 1,
             TRE4_2 > 3 ~ 0.75,
             TRE4_2 > 2 ~ 0.5,
             TRE4_2 > 1 ~ 0.25, 
             TRE4_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_3 = 
           dplyr::case_when(
             TRE4_3 >= 5 ~ 1,
             TRE4_3 > 3 ~ 0.75,
             TRE4_3 > 2 ~ 0.5,
             TRE4_3 > 1 ~ 0.25, 
             TRE4_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_4 = 
           dplyr::case_when(
             TRE4_4 >= 5 ~ 1,
             TRE4_4 > 3 ~ 0.75,
             TRE4_4 > 2 ~ 0.5,
             TRE4_4 > 1 ~ 0.25, 
             TRE4_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_5 = 
           dplyr::case_when(
             TRE4_5 >= 2 ~ 0, 
             TRE4_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND2_4_6 = 
           dplyr::case_when(
             TRE4_6 >= 5 ~ 0,
             TRE4_6 > 3 ~ 0.25,
             TRE4_6 > 2 ~ 0.5,
             TRE4_6 > 1 ~ 0.75, 
             TRE4_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND2_Q1_combined = coalesce(RAND2_3_1, RAND2_4_1))

hu_survey <- hu_survey %>%
  mutate(RAND2_Q2_combined = coalesce(RAND2_3_2, RAND2_4_2))

hu_survey <- hu_survey %>%
  mutate(RAND2_Q3_combined = coalesce(RAND2_3_3, RAND2_4_3))

hu_survey <- hu_survey %>%
  mutate(RAND2_Q4_combined = coalesce(RAND2_3_4, RAND2_4_4))

hu_survey <- hu_survey %>%
  mutate(RAND2_Q5_combined = coalesce(RAND2_3_5, RAND2_4_5))

hu_survey <- hu_survey %>%
  mutate(RAND2_Q6_combined = coalesce(RAND2_3_6, RAND2_4_6))

#RAND3# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_1 =
           dplyr::case_when(
             TRE5_1 >= 4 ~ 1,
             TRE5_1 > 2 ~ 0.75,
             TRE5_1 > 1 ~ 0.5, 
             TRE5_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_2 = 
           dplyr::case_when(
             TRE5_2 >= 5 ~ 1,
             TRE5_2 > 3 ~ 0.75,
             TRE5_2 > 2 ~ 0.5,
             TRE5_2 > 1 ~ 0.25, 
             TRE5_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_3 = 
           dplyr::case_when(
             TRE5_3 >= 5 ~ 1,
             TRE5_3 > 3 ~ 0.75,
             TRE5_3 > 2 ~ 0.5,
             TRE5_3 > 1 ~ 0.25, 
             TRE5_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_4 = 
           dplyr::case_when(
             TRE5_4 >= 5 ~ 1,
             TRE5_4 > 3 ~ 0.75,
             TRE5_4 > 2 ~ 0.5,
             TRE5_4 > 1 ~ 0.25, 
             TRE5_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_5 = 
           dplyr::case_when(
             TRE5_5 >= 2 ~ 0, 
             TRE5_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_5_6 = 
           dplyr::case_when(
             TRE5_6 >= 5 ~ 0,
             TRE5_6 > 3 ~ 0.25,
             TRE5_6 > 2 ~ 0.5,
             TRE5_6 > 1 ~ 0.75, 
             TRE5_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_1 =
           dplyr::case_when(
             TRE6_1 >= 4 ~ 1,
             TRE6_1 > 2 ~ 0.75,
             TRE6_1 > 1 ~ 0.5, 
             TRE6_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_2 = 
           dplyr::case_when(
             TRE6_2 >= 5 ~ 1,
             TRE6_2 > 3 ~ 0.75,
             TRE6_2 > 2 ~ 0.5,
             TRE6_2 > 1 ~ 0.25, 
             TRE6_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_3 = 
           dplyr::case_when(
             TRE6_3 >= 5 ~ 1,
             TRE6_3 > 3 ~ 0.75,
             TRE6_3 > 2 ~ 0.5,
             TRE6_3 > 1 ~ 0.25, 
             TRE6_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_4 = 
           dplyr::case_when(
             TRE6_4 >= 5 ~ 1,
             TRE6_4 > 3 ~ 0.75,
             TRE6_4 > 2 ~ 0.5,
             TRE6_4 > 1 ~ 0.25, 
             TRE6_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_5 = 
           dplyr::case_when(
             TRE6_5 >= 2 ~ 0, 
             TRE6_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND3_6_6 = 
           dplyr::case_when(
             TRE6_6 >= 5 ~ 0,
             TRE6_6 > 3 ~ 0.25,
             TRE6_6 > 2 ~ 0.5,
             TRE6_6 > 1 ~ 0.75, 
             TRE6_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND3_Q1_combined = coalesce(RAND3_5_1, RAND3_6_1))

hu_survey <- hu_survey %>%
  mutate(RAND3_Q2_combined = coalesce(RAND3_5_2, RAND3_6_2))

hu_survey <- hu_survey %>%
  mutate(RAND3_Q3_combined = coalesce(RAND3_5_3, RAND3_6_3))

hu_survey <- hu_survey %>%
  mutate(RAND3_Q4_combined = coalesce(RAND3_5_4, RAND3_6_4))

hu_survey <- hu_survey %>%
  mutate(RAND3_Q5_combined = coalesce(RAND3_5_5, RAND3_6_5))

hu_survey <- hu_survey %>%
  mutate(RAND3_Q6_combined = coalesce(RAND3_5_6, RAND3_6_6))

#RAND4# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_1 =
           dplyr::case_when(
             TRE7_1 >= 4 ~ 1,
             TRE7_1 > 2 ~ 0.75,
             TRE7_1 > 1 ~ 0.5, 
             TRE7_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_2 = 
           dplyr::case_when(
             TRE7_2 >= 5 ~ 1,
             TRE7_2 > 3 ~ 0.75,
             TRE7_2 > 2 ~ 0.5,
             TRE7_2 > 1 ~ 0.25, 
             TRE7_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_3 = 
           dplyr::case_when(
             TRE7_3 >= 5 ~ 1,
             TRE7_3 > 3 ~ 0.75,
             TRE7_3 > 2 ~ 0.5,
             TRE7_3 > 1 ~ 0.25, 
             TRE7_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_4 = 
           dplyr::case_when(
             TRE7_4 >= 5 ~ 1,
             TRE7_4 > 3 ~ 0.75,
             TRE7_4 > 2 ~ 0.5,
             TRE7_4 > 1 ~ 0.25, 
             TRE7_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_5 = 
           dplyr::case_when(
             TRE7_5 >= 2 ~ 0, 
             TRE7_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_7_6 = 
           dplyr::case_when(
             TRE7_6 >= 5 ~ 0,
             TRE7_6 > 3 ~ 0.25,
             TRE7_6 > 2 ~ 0.5,
             TRE7_6 > 1 ~ 0.75, 
             TRE7_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_1 =
           dplyr::case_when(
             TRE8_1 >= 4 ~ 1,
             TRE8_1 > 2 ~ 0.75,
             TRE8_1 > 1 ~ 0.5, 
             TRE8_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_2 = 
           dplyr::case_when(
             TRE8_2 >= 5 ~ 1,
             TRE8_2 > 3 ~ 0.75,
             TRE8_2 > 2 ~ 0.5,
             TRE8_2 > 1 ~ 0.25, 
             TRE8_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_3 = 
           dplyr::case_when(
             TRE8_3 >= 5 ~ 1,
             TRE8_3 > 3 ~ 0.75,
             TRE8_3 > 2 ~ 0.5,
             TRE8_3 > 1 ~ 0.25, 
             TRE8_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_4 = 
           dplyr::case_when(
             TRE8_4 >= 5 ~ 1,
             TRE8_4 > 3 ~ 0.75,
             TRE8_4 > 2 ~ 0.5,
             TRE8_4 > 1 ~ 0.25, 
             TRE8_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_5 = 
           dplyr::case_when(
             TRE8_5 >= 2 ~ 0, 
             TRE8_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND4_8_6 = 
           dplyr::case_when(
             TRE8_6 >= 5 ~ 0,
             TRE8_6 > 3 ~ 0.25,
             TRE8_6 > 2 ~ 0.5,
             TRE8_6 > 1 ~ 0.75, 
             TRE8_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND4_Q1_combined = coalesce(RAND4_7_1, RAND4_8_1))

hu_survey <- hu_survey %>%
  mutate(RAND4_Q2_combined = coalesce(RAND4_7_2, RAND4_8_2))

hu_survey <- hu_survey %>%
  mutate(RAND4_Q3_combined = coalesce(RAND4_7_3, RAND4_8_3))

hu_survey <- hu_survey %>%
  mutate(RAND4_Q4_combined = coalesce(RAND4_7_4, RAND4_8_4))

hu_survey <- hu_survey %>%
  mutate(RAND4_Q5_combined = coalesce(RAND4_7_5, RAND4_8_5))

hu_survey <- hu_survey %>%
  mutate(RAND4_Q6_combined = coalesce(RAND4_7_6, RAND4_8_6))

#RAND5# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_1 =
           dplyr::case_when(
             TRE9_1 >= 4 ~ 1,
             TRE9_1 > 2 ~ 0.75,
             TRE9_1 > 1 ~ 0.5, 
             TRE9_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_2 = 
           dplyr::case_when(
             TRE9_2 >= 5 ~ 1,
             TRE9_2 > 3 ~ 0.75,
             TRE9_2 > 2 ~ 0.5,
             TRE9_2 > 1 ~ 0.25, 
             TRE9_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_3 = 
           dplyr::case_when(
             TRE9_3 >= 5 ~ 1,
             TRE9_3 > 3 ~ 0.75,
             TRE9_3 > 2 ~ 0.5,
             TRE9_3 > 1 ~ 0.25, 
             TRE9_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_4 = 
           dplyr::case_when(
             TRE9_4 >= 5 ~ 1,
             TRE9_4 > 3 ~ 0.75,
             TRE9_4 > 2 ~ 0.5,
             TRE9_4 > 1 ~ 0.25, 
             TRE9_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_5 = 
           dplyr::case_when(
             TRE9_5 >= 2 ~ 0, 
             TRE9_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_9_6 = 
           dplyr::case_when(
             TRE9_6 >= 5 ~ 0,
             TRE9_6 > 3 ~ 0.25,
             TRE9_6 > 2 ~ 0.5,
             TRE9_6 > 1 ~ 0.75, 
             TRE9_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_1 =
           dplyr::case_when(
             TRE10_1 >= 4 ~ 1,
             TRE10_1 > 2 ~ 0.75,
             TRE10_1 > 1 ~ 0.5, 
             TRE10_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_2 = 
           dplyr::case_when(
             TRE10_2 >= 5 ~ 1,
             TRE10_2 > 3 ~ 0.75,
             TRE10_2 > 2 ~ 0.5,
             TRE10_2 > 1 ~ 0.25, 
             TRE10_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_3 = 
           dplyr::case_when(
             TRE10_3 >= 5 ~ 1,
             TRE10_3 > 3 ~ 0.75,
             TRE10_3 > 2 ~ 0.5,
             TRE10_3 > 1 ~ 0.25, 
             TRE10_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_4 = 
           dplyr::case_when(
             TRE10_4 >= 5 ~ 1,
             TRE10_4 > 3 ~ 0.75,
             TRE10_4 > 2 ~ 0.5,
             TRE10_4 > 1 ~ 0.25, 
             TRE10_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_5 = 
           dplyr::case_when(
             TRE10_5 >= 2 ~ 0, 
             TRE10_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND5_10_6 = 
           dplyr::case_when(
             TRE10_6 >= 5 ~ 0,
             TRE10_6 > 3 ~ 0.25,
             TRE10_6 > 2 ~ 0.5,
             TRE10_6 > 1 ~ 0.75, 
             TRE10_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND5_Q1_combined = coalesce(RAND5_9_1, RAND5_10_1))

hu_survey <- hu_survey %>%
  mutate(RAND5_Q2_combined = coalesce(RAND5_9_2, RAND5_10_2))

hu_survey <- hu_survey %>%
  mutate(RAND5_Q3_combined = coalesce(RAND5_9_3, RAND5_10_3))

hu_survey <- hu_survey %>%
  mutate(RAND5_Q4_combined = coalesce(RAND5_9_4, RAND5_10_4))

hu_survey <- hu_survey %>%
  mutate(RAND5_Q5_combined = coalesce(RAND5_9_5, RAND5_10_5))

hu_survey <- hu_survey %>%
  mutate(RAND5_Q6_combined = coalesce(RAND5_9_6, RAND5_10_6))

#RAND6# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_1 =
           dplyr::case_when(
             TRE11_1 >= 4 ~ 1,
             TRE11_1 > 2 ~ 0.75,
             TRE11_1 > 1 ~ 0.5, 
             TRE11_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_2 = 
           dplyr::case_when(
             TRE11_2 >= 5 ~ 1,
             TRE11_2 > 3 ~ 0.75,
             TRE11_2 > 2 ~ 0.5,
             TRE11_2 > 1 ~ 0.25, 
             TRE11_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_3 = 
           dplyr::case_when(
             TRE11_3 >= 5 ~ 1,
             TRE11_3 > 3 ~ 0.75,
             TRE11_3 > 2 ~ 0.5,
             TRE11_3 > 1 ~ 0.25, 
             TRE11_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_4 = 
           dplyr::case_when(
             TRE11_4 >= 5 ~ 1,
             TRE11_4 > 3 ~ 0.75,
             TRE11_4 > 2 ~ 0.5,
             TRE11_4 > 1 ~ 0.25, 
             TRE11_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_5 = 
           dplyr::case_when(
             TRE11_5 >= 2 ~ 0, 
             TRE11_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_11_6 = 
           dplyr::case_when(
             TRE11_6 >= 5 ~ 0,
             TRE11_6 > 3 ~ 0.25,
             TRE11_6 > 2 ~ 0.5,
             TRE11_6 > 1 ~ 0.75, 
             TRE11_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_1 =
           dplyr::case_when(
             TRE12_1 >= 4 ~ 1,
             TRE12_1 > 2 ~ 0.75,
             TRE12_1 > 1 ~ 0.5, 
             TRE12_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_2 = 
           dplyr::case_when(
             TRE12_2 >= 5 ~ 1,
             TRE12_2 > 3 ~ 0.75,
             TRE12_2 > 2 ~ 0.5,
             TRE12_2 > 1 ~ 0.25, 
             TRE12_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_3 = 
           dplyr::case_when(
             TRE12_3 >= 5 ~ 1,
             TRE12_3 > 3 ~ 0.75,
             TRE12_3 > 2 ~ 0.5,
             TRE12_3 > 1 ~ 0.25, 
             TRE12_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_4 = 
           dplyr::case_when(
             TRE12_4 >= 5 ~ 1,
             TRE12_4 > 3 ~ 0.75,
             TRE12_4 > 2 ~ 0.5,
             TRE12_4 > 1 ~ 0.25, 
             TRE12_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_5 = 
           dplyr::case_when(
             TRE12_5 >= 2 ~ 0, 
             TRE12_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND6_12_6 = 
           dplyr::case_when(
             TRE12_6 >= 5 ~ 0,
             TRE12_6 > 3 ~ 0.25,
             TRE12_6 > 2 ~ 0.5,
             TRE12_6 > 1 ~ 0.75, 
             TRE12_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND6_Q1_combined = coalesce(RAND6_11_1, RAND6_12_1))

hu_survey <- hu_survey %>%
  mutate(RAND6_Q2_combined = coalesce(RAND6_11_2, RAND6_12_2))

hu_survey <- hu_survey %>%
  mutate(RAND6_Q3_combined = coalesce(RAND6_11_3, RAND6_12_3))

hu_survey <- hu_survey %>%
  mutate(RAND6_Q4_combined = coalesce(RAND6_11_4, RAND6_12_4))

hu_survey <- hu_survey %>%
  mutate(RAND6_Q5_combined = coalesce(RAND6_11_5, RAND6_12_5))

hu_survey <- hu_survey %>%
  mutate(RAND6_Q6_combined = coalesce(RAND6_11_6, RAND6_12_6))

#RAND7# 

#dosage 1# 

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_1 =
           dplyr::case_when(
             TRE13_1 >= 4 ~ 1,
             TRE13_1 > 2 ~ 0.75,
             TRE13_1 > 1 ~ 0.5, 
             TRE13_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_2 = 
           dplyr::case_when(
             TRE13_2 >= 5 ~ 1,
             TRE13_2 > 3 ~ 0.75,
             TRE13_2 > 2 ~ 0.5,
             TRE13_2 > 1 ~ 0.25, 
             TRE13_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_3 = 
           dplyr::case_when(
             TRE13_3 >= 5 ~ 1,
             TRE13_3 > 3 ~ 0.75,
             TRE13_3 > 2 ~ 0.5,
             TRE13_3 > 1 ~ 0.25, 
             TRE13_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_4 = 
           dplyr::case_when(
             TRE13_4 >= 5 ~ 1,
             TRE13_4 > 3 ~ 0.75,
             TRE13_4 > 2 ~ 0.5,
             TRE13_4 > 1 ~ 0.25, 
             TRE13_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_5 = 
           dplyr::case_when(
             TRE13_5 >= 2 ~ 0, 
             TRE13_5 > 0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_13_6 = 
           dplyr::case_when(
             TRE13_6 >= 5 ~ 0,
             TRE13_6 > 3 ~ 0.25,
             TRE13_6 > 2 ~ 0.5,
             TRE13_6 > 1 ~ 0.75, 
             TRE13_6 > 0 ~ 1
           ))

#dosage 2# 

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_1 =
           dplyr::case_when(
             TRE14_1 >= 4 ~ 1,
             TRE14_1 > 2 ~ 0.75,
             TRE14_1 > 1 ~ 0.5, 
             TRE14_1 > 0 ~ 0.25
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_2 = 
           dplyr::case_when(
             TRE14_2 >= 5 ~ 1,
             TRE14_2 > 3 ~ 0.75,
             TRE14_2 > 2 ~ 0.5,
             TRE14_2 > 1 ~ 0.25, 
             TRE14_2 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_3 = 
           dplyr::case_when(
             TRE14_3 >= 5 ~ 1,
             TRE14_3 > 3 ~ 0.75,
             TRE14_3 > 2 ~ 0.5,
             TRE14_3 > 1 ~ 0.25, 
             TRE14_3 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_4 = 
           dplyr::case_when(
             TRE14_4 >= 5 ~ 1,
             TRE14_4 > 3 ~ 0.75,
             TRE14_4 > 2 ~ 0.5,
             TRE14_4 > 1 ~ 0.25, 
             TRE14_4 > 0 ~ 0
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_5 = 
           dplyr::case_when(
             TRE14_5 >= 2 ~ 0, 
             TRE14_5 >0 ~ 1
           ))

hu_survey <- hu_survey %>% 
  mutate(RAND7_14_6 = 
           dplyr::case_when(
             TRE14_6 >= 5 ~ 0,
             TRE14_6 > 3 ~ 0.25,
             TRE14_6 > 2 ~ 0.5,
             TRE14_6 > 1 ~ 0.75, 
             TRE14_6 > 0 ~ 1
           ))

#combined columns# 

hu_survey <- hu_survey %>%
  mutate(RAND7_Q1_combined = coalesce(RAND7_13_1, RAND7_14_1))

hu_survey <- hu_survey %>%
  mutate(RAND7_Q2_combined = coalesce(RAND7_13_2, RAND7_14_2))

hu_survey <- hu_survey %>%
  mutate(RAND7_Q3_combined = coalesce(RAND7_13_3, RAND7_14_3))

hu_survey <- hu_survey %>%
  mutate(RAND7_Q4_combined = coalesce(RAND7_13_4, RAND7_14_4))

hu_survey <- hu_survey %>%
  mutate(RAND7_Q5_combined = coalesce(RAND7_13_5, RAND7_14_5))

hu_survey <- hu_survey %>%
  mutate(RAND7_Q6_combined = coalesce(RAND7_13_6, RAND7_14_6))

##### creating embeddedness score for each question MED1 - MED5 #####

### Primary Categories (MED2, MED3, MED4) - Raw scoring, no log scaling ###

hu_survey <- hu_survey %>%
  rowwise() %>%
  mutate(
    # MED2
    med2_binary = sum(c_across(c(MED2_1, MED2_9)) == 1, na.rm = TRUE) + ifelse(MED2_2 == 1, 0.5, 0),
    med2_text = sum(
      str_detect(str_to_lower(coalesce(MED2_97_E, "")), "portfolio") * 0,
      str_detect(str_to_lower(MED2_97_E), "penzcentrum") * 0,
      str_detect(str_to_lower(MED2_97_E), "metropol") * 1,
      str_detect(str_to_lower(MED2_97_E), "ripost") * 1,
      na.rm = TRUE
    ),
    MED2_score = ifelse(MED2_95 == 1, 0.05, med2_binary + med2_text),
    
    # MED3
    med3_binary = sum((MED3_1 == 1) + (MED3_2 == 1) + (MED3_4 == 1) + (MED3_5 == 1) +
                        ((MED3_3 == 1) * 0.5) + ((MED3_6 == 1) * 0.5), na.rm = TRUE),
    med3_text = ifelse(
      str_detect(coalesce(MED3_97_E, ""), "\\b1\\b") & str_detect(MED3_97_E, "\\b88\\b"), 1, 0
    ),
    MED3_score = ifelse(MED3_95 == 1, 0.05, med3_binary + med3_text),
    
    # MED4
    med4_binary = sum(c_across(c(MED4_2, MED4_3, MED4_4, MED4_5)) == 1, na.rm = TRUE),
    med4_text = sum(
      str_detect(str_to_lower(coalesce(MED4_97_E, "")), "moziverzum") * 1,
      str_detect(str_to_lower(MED4_97_E), "mozi") * 1,
      str_detect(str_to_lower(MED4_97_E), "spektrum") * 0,
      str_detect(str_to_lower(MED4_97_E), "sorozat") * 0,
      str_detect(str_to_lower(MED4_97_E), "viasat") * 0,
      na.rm = TRUE
    ),
    MED4_score = ifelse(MED4_95 == 1, 0.05, med4_binary + med4_text)
  ) %>%
  ungroup() %>%
  select(-med2_binary, -med2_text, -med3_binary, -med3_text, -med4_binary, -med4_text)

### Bonus Categories (MED1 and MED5) ###

#+0.1 to the base_score for each government affiliated media source a respondent consumed from MED1 and MED5 
#-0.1 to the base_score for each non-government affiliated media source a respondnet consumed from MED1 and MED5 

hu_survey <- hu_survey %>%
  rowwise() %>%
  mutate(
    # MED1
    med1_positive = sum(c_across(c(MED1_1, MED1_2, MED1_6, MED1_7)) == 1, na.rm = TRUE) * 0.1 +
      sum(
        str_detect(str_to_lower(coalesce(MED1_97_E, "")), "bors") * 0.1,
        str_detect(str_to_lower(MED1_97_E), "blikk") * 0.05,
        str_detect(str_to_lower(MED1_97_E), "naplo") * 0.1,
        na.rm = TRUE
      ),
    med1_negative = sum(c_across(c(MED1_3, MED1_4, MED1_5)) == 1, na.rm = TRUE) * 0.1 +
      sum(
        str_detect(str_to_lower(MED1_97_E), "nok lap") * 0.1,
        na.rm = TRUE
      ),
    med1_bonus_score = med1_positive - med1_negative,
    
    # MED5
    med5_positive = sum(c_across(c(MED5_1, MED5_2, MED5_3, MED5_7, MED5_10)) == 1, na.rm = TRUE) * 0.1 +
      sum(
        str_detect(str_to_lower(coalesce(MED5_97_E, "")), "zsolt") * 0.1,
        str_detect(str_to_lower(MED5_97_E), "somogyi") * 0.1,
        na.rm = TRUE
      ),
    med5_negative = sum(c_across(c(MED5_4, MED5_5, MED5_6, MED5_8, MED5_9)) == 1, na.rm = TRUE) * 0.1 +
      sum(
        str_detect(str_to_lower(MED5_97_E), "dave") * 0.1,
        na.rm = TRUE
      ),
    med5_bonus_score = med5_positive - med5_negative
  ) %>%
  ungroup() %>%
  select(-med1_positive, -med1_negative, -med5_positive, -med5_negative)

### Final comp_score using raw average (not log or logistic) ###
hu_survey <- hu_survey %>%
  mutate(
    base_score = rowMeans(across(c(MED2_score, MED3_score, MED4_score)), na.rm = TRUE),
    comp_score = pmin(pmax(base_score + med1_bonus_score + med5_bonus_score, 0), 1)
  )

# Check summary

summary(hu_survey$comp_score)

## create binary comp_score for high (1) and low (0) embeddedness ## 

hu_survey <- hu_survey %>%
  mutate(
    comp_high = if_else(comp_score >= median(comp_score, na.rm = TRUE), 1, 0)
  )

table(hu_survey$comp_high) #0 == 953 #1 == 1047 

#####new style illiberalism battery##### 

### recalibrate question scales ### 

hu_survey <- hu_survey %>% 
  mutate(ILL1_1_v2 = #national elections are free and fair
           dplyr::case_when(
             ILL1_1 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_1 > 8 ~ 0.9,
             ILL1_1 > 7 ~ 0.8,
             ILL1_1 > 6 ~ 0.7, 
             ILL1_1 > 5 ~ 0.6,
             ILL1_1 > 4 ~ 0.5,
             ILL1_1 > 3 ~ 0.4,
             ILL1_1 > 2 ~ 0.3,
             ILL1_1 > 1 ~ 0.2, 
             ILL1_1 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_2_v2 = #rule of law is upheld
           dplyr::case_when(
             ILL1_2 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_2 > 8 ~ 0.9,
             ILL1_2 > 7 ~ 0.8,
             ILL1_2 > 6 ~ 0.7, 
             ILL1_2 > 5 ~ 0.6,
             ILL1_2 > 4 ~ 0.5,
             ILL1_2 > 3 ~ 0.4,
             ILL1_2 > 2 ~ 0.3,
             ILL1_2 > 1 ~ 0.2, 
             ILL1_2 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_3_v2 = #checks and balances exist between gov branches
           dplyr::case_when(
             ILL1_3 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_3 > 8 ~ 0.9,
             ILL1_3 > 7 ~ 0.8,
             ILL1_3 > 6 ~ 0.7, 
             ILL1_3 > 5 ~ 0.6,
             ILL1_3 > 4 ~ 0.5,
             ILL1_3 > 3 ~ 0.4,
             ILL1_3 > 2 ~ 0.3,
             ILL1_3 > 1 ~ 0.2, 
             ILL1_3 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_4_v2 = #opposition parties have a fair chance in elections
           dplyr::case_when(
             ILL1_4 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_4 > 8 ~ 0.9,
             ILL1_4 > 7 ~ 0.8,
             ILL1_4 > 6 ~ 0.7, 
             ILL1_4 > 5 ~ 0.6,
             ILL1_4 > 4 ~ 0.5,
             ILL1_4 > 3 ~ 0.4,
             ILL1_4 > 2 ~ 0.3,
             ILL1_4 > 1 ~ 0.2, 
             ILL1_4 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_5_v2 = #govs are responsive to citizens 
           dplyr::case_when(
             ILL1_5 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_5 > 8 ~ 0.9,
             ILL1_5 > 7 ~ 0.8,
             ILL1_5 > 6 ~ 0.7, 
             ILL1_5 > 5 ~ 0.6,
             ILL1_5 > 4 ~ 0.5,
             ILL1_5 > 3 ~ 0.4,
             ILL1_5 > 2 ~ 0.3,
             ILL1_5 > 1 ~ 0.2, 
             ILL1_5 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_6_v2 = #all parties get = media exposure 
           dplyr::case_when(
             ILL1_6 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_6 > 8 ~ 0.9,
             ILL1_6 > 7 ~ 0.8,
             ILL1_6 > 6 ~ 0.7, 
             ILL1_6 > 5 ~ 0.6,
             ILL1_6 > 4 ~ 0.5,
             ILL1_6 > 3 ~ 0.4,
             ILL1_6 > 2 ~ 0.3,
             ILL1_6 > 1 ~ 0.2, 
             ILL1_6 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_7_v2 = #opp parties operate freely 
           dplyr::case_when(
             ILL1_7 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_7 > 8 ~ 0.9,
             ILL1_7 > 7 ~ 0.8,
             ILL1_7 > 6 ~ 0.7, 
             ILL1_7 > 5 ~ 0.6,
             ILL1_7 > 4 ~ 0.5,
             ILL1_7 > 3 ~ 0.4,
             ILL1_7 > 2 ~ 0.3,
             ILL1_7 > 1 ~ 0.2, 
             ILL1_7 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_8_v2 = #human/civil rights groups operate freely 
           dplyr::case_when(
             ILL1_8 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_8 > 8 ~ 0.9,
             ILL1_8 > 7 ~ 0.8,
             ILL1_8 > 6 ~ 0.7, 
             ILL1_8 > 5 ~ 0.6,
             ILL1_8 > 4 ~ 0.5,
             ILL1_8 > 3 ~ 0.4,
             ILL1_8 > 2 ~ 0.3,
             ILL1_8 > 1 ~ 0.2, 
             ILL1_8 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_9_v2 = #media can operate freely 
           dplyr::case_when(
             ILL1_9 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_9 > 8 ~ 0.9,
             ILL1_9 > 7 ~ 0.8,
             ILL1_9 > 6 ~ 0.7, 
             ILL1_9 > 5 ~ 0.6,
             ILL1_9 > 4 ~ 0.5,
             ILL1_9 > 3 ~ 0.4,
             ILL1_9 > 2 ~ 0.3,
             ILL1_9 > 1 ~ 0.2, 
             ILL1_9 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_10_v2 = #gov tax the rich/subsidize the poor 
           dplyr::case_when(
             ILL1_10 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_10 > 8 ~ 0.9,
             ILL1_10 > 7 ~ 0.8,
             ILL1_10 > 6 ~ 0.7, 
             ILL1_10 > 5 ~ 0.6,
             ILL1_10 > 4 ~ 0.5,
             ILL1_10 > 3 ~ 0.4,
             ILL1_10 > 2 ~ 0.3,
             ILL1_10 > 1 ~ 0.2, 
             ILL1_10 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_11_v2 = #unemployed should receive state aid
           dplyr::case_when(
             ILL1_11 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_11 > 8 ~ 0.9,
             ILL1_11 > 7 ~ 0.8,
             ILL1_11 > 6 ~ 0.7, 
             ILL1_11 > 5 ~ 0.6,
             ILL1_11 > 4 ~ 0.5,
             ILL1_11 > 3 ~ 0.4,
             ILL1_11 > 2 ~ 0.3,
             ILL1_11 > 1 ~ 0.2, 
             ILL1_11 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_12_v2 = #civil rights protect from state oppression
           dplyr::case_when(
             ILL1_12 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_12 > 8 ~ 0.9,
             ILL1_12 > 7 ~ 0.8,
             ILL1_12 > 6 ~ 0.7, 
             ILL1_12 > 5 ~ 0.6,
             ILL1_12 > 4 ~ 0.5,
             ILL1_12 > 3 ~ 0.4,
             ILL1_12 > 2 ~ 0.3,
             ILL1_12 > 1 ~ 0.2, 
             ILL1_12 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_13_v2 = #state ensures equal income for all 
           dplyr::case_when(
             ILL1_13 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_13 > 8 ~ 0.9,
             ILL1_13 > 7 ~ 0.8,
             ILL1_13 > 6 ~ 0.7, 
             ILL1_13 > 5 ~ 0.6,
             ILL1_13 > 4 ~ 0.5,
             ILL1_13 > 3 ~ 0.4,
             ILL1_13 > 2 ~ 0.3,
             ILL1_13 > 1 ~ 0.2, 
             ILL1_13 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_14_v2 = #women have same rights as men 
           dplyr::case_when(
             ILL1_14 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_14 > 8 ~ 0.9,
             ILL1_14 > 7 ~ 0.8,
             ILL1_14 > 6 ~ 0.7, 
             ILL1_14 > 5 ~ 0.6,
             ILL1_14 > 4 ~ 0.5,
             ILL1_14 > 3 ~ 0.4,
             ILL1_14 > 2 ~ 0.3,
             ILL1_14 > 1 ~ 0.2, 
             ILL1_14 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL1_15_v2 = #minority rights enshrined in law 
           dplyr::case_when(
             ILL1_15 > 9 ~ 1, #essential characteristic of democracy 
             ILL1_15 > 8 ~ 0.9,
             ILL1_15 > 7 ~ 0.8,
             ILL1_15 > 6 ~ 0.7, 
             ILL1_15 > 5 ~ 0.6,
             ILL1_15 > 4 ~ 0.5,
             ILL1_15 > 3 ~ 0.4,
             ILL1_15 > 2 ~ 0.3,
             ILL1_15 > 1 ~ 0.2, 
             ILL1_15 > 0 ~ 0.1 #not at all an essential characteristic 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL2_1_v2 = #Hungarians should be tolerant of those who lead unconventional lives 
           dplyr::case_when(
             ILL2_1 > 4 ~ 1,  #agree strongly
             ILL2_1 > 3 ~ 0.8,
             ILL2_1 > 2 ~ 0.6,
             ILL2_1 > 1 ~ 0.4, 
             ILL2_1 > 0 ~ 0.2  #disagree strongly  
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL2_2_v2 = #good society allows for different opinions/ways of life to flourish
           dplyr::case_when(
             ILL2_2 > 4 ~ 1.0,  #agree strongly 
             ILL2_2 > 3 ~ 0.8,
             ILL2_2 > 2 ~ 0.6,
             ILL2_2 > 1 ~ 0.4, 
             ILL2_2 > 0 ~ 0.2   #disagree strongly 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL2_3_v2 = #judges should be selected based on cross party consensus
           dplyr::case_when(
             ILL2_3 > 4 ~ 1.0, #agree strongly
             ILL2_3 > 3 ~ 0.8,
             ILL2_3 > 2 ~ 0.6,
             ILL2_3 > 1 ~ 0.4, 
             ILL2_3 > 0 ~ 0.2  #disagree strongly 
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL2_4_v2 = #public media should be independent of political developments
           dplyr::case_when(
             ILL2_4 > 4 ~ 1.0,  #agree strongly
             ILL2_4 > 3 ~ 0.8,
             ILL2_4 > 2 ~ 0.6,
             ILL2_4 > 1 ~ 0.4, 
             ILL2_4 > 0 ~ 0.2   #disagree strongly
           ))

hu_survey <- hu_survey %>% 
  mutate(ILL2_5_v2 = #democracy is preferable to other political systems 
           dplyr::case_when(
             ILL2_5 > 4 ~ 1.0,  #agree strongly 
             ILL2_5 > 3 ~ 0.8,
             ILL2_5 > 2 ~ 0.6,
             ILL2_5 > 1 ~ 0.4, 
             ILL2_5 > 0 ~ 0.2   #disagree strongly 
           ))

## binary new illiberalism variable ## 

ill1_vars <- paste0("ILL1_", 1:15, "_v2")
ill2_vars <- paste0("ILL2_", 1:5, "_v2")
all_ill_vars <- c(ill1_vars, ill2_vars)
min_answered <- if (exists("min_answered")) min_answered else 12

# pipeline
hu_survey <- hu_survey %>%
  # ensure columns exist
  { if(length(setdiff(all_ill_vars, names(.)))>0) stop("Missing vars: ",
                                                       paste(setdiff(all_ill_vars, names(.)), collapse = ", ")) else . } %>%
  # coerce those columns to numeric
  mutate(across(all_of(all_ill_vars), ~ as.numeric(as.character(.)))) %>%
  rowwise() %>%
  mutate(
    answered_n = sum(!is.na(c_across(all_of(all_ill_vars)))),
    new_illib_level = if_else(answered_n >= min_answered,
                              mean(c_across(all_of(all_ill_vars)), na.rm = TRUE),
                              NA_real_)
  ) %>%
  ungroup()

summary(hu_survey$new_illib_level)

hu_survey <- hu_survey %>% 
  mutate(new_illib_binary = 
           dplyr::case_when(
             new_illib_level >= 0.885 ~ 1,  # Democratic Leaning: 1024
             new_illib_level < 0.885 ~ 0.   #Illiberal Leaning: 976  
           )
  )

table(hu_survey$new_illib_binary)

### old fashioned authoritarianism battery ###

## recalibrate question scales ## 

hu_survey <- hu_survey %>% 
  mutate(AUT1_1_v2 = #Hungary needs loyalty towards its leaders 
           dplyr::case_when(
             AUT1_1 > 4 ~ 0.2,  #disagree strongly
             AUT1_1 > 3 ~ 0.4,
             AUT1_1 > 2 ~ 0.6,
             AUT1_1 > 1 ~ 0.8, 
             AUT1_1 > 0 ~ 1.0   #agree strongly  
           ))

summary(hu_survey$AUT1_1_v2)

hu_survey <- hu_survey %>% 
  mutate(AUT1_2_v2 = #government control of media/education is necessary 
           dplyr::case_when(
             AUT1_2 > 4 ~ 0.2,  #disagree strongly 
             AUT1_2 > 3 ~ 0.4,
             AUT1_2 > 2 ~ 0.6,
             AUT1_2 > 1 ~ 0.8, 
             AUT1_2 > 0 ~ 1.0   #agree strongly
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT1_3_v2 = #protests/demonstrations against gov should be allowed
           dplyr::case_when(
             AUT1_3 > 4 ~ 0.2, #agree strongly
             AUT1_3 > 3 ~ 0.4,
             AUT1_3 > 2 ~ 0.6,
             AUT1_3 > 1 ~ 0.8, 
             AUT1_3 > 0 ~ 1.0  #disagree strongly 
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT1_4_v2 = #judges should be selected by ruling party 
           dplyr::case_when(
             AUT1_4 > 4 ~ 0.2,  #agree strongly
             AUT1_4 > 3 ~ 0.4,
             AUT1_4 > 2 ~ 0.6,
             AUT1_4 > 1 ~ 0.8, 
             AUT1_4 > 0 ~ 1.0   #disagree strongly
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT1_5_v2 = #public media should defend gov from criticism 
           dplyr::case_when(
             AUT1_5 > 4 ~ 0.2,  #agree strongly 
             AUT1_5 > 3 ~ 0.4,
             AUT1_5 > 2 ~ 0.6,
             AUT1_5 > 1 ~ 0.8, 
             AUT1_5 > 0 ~ 1.0   #disagree strongly 
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT2_1_v2 = #having a strong leader who does not bother w/ parliament/elections  
           dplyr::case_when(
             AUT2_1 > 4 ~ 1.0,  #very bad 
             AUT2_1 > 3 ~ 0.8,
             AUT2_1 > 2 ~ 0.6,
             AUT2_1 > 1 ~ 0.4, 
             AUT2_1 > 0 ~ 0.2   #very good   
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT2_2_v2 = #having a political system rooted in Christian morality
           dplyr::case_when(
             AUT2_2 > 4 ~ 1.0,  #very bad 
             AUT2_2 > 3 ~ 0.8,
             AUT2_2 > 2 ~ 0.6,
             AUT2_2 > 1 ~ 0.4, 
             AUT2_2 > 0 ~ 0.2   #very good   
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT3_v2 = #opinion on Hungarians who believe Kadar regime was better 
           dplyr::case_when(
             AUT3 > 4 ~ 1.0,  #very negative  
             AUT3 > 3 ~ 0.8,
             AUT3 > 2 ~ 0.6,
             AUT3 > 1 ~ 0.4, 
             AUT3 > 0 ~ 0.2   #very positive 
           ))

hu_survey <- hu_survey %>% 
  mutate(AUT4_v2 = #opinion on Singapore's one-party rule   
           dplyr::case_when(
             AUT4 > 4 ~ 1.0,  #very negative  
             AUT4 > 3 ~ 0.8,
             AUT4 > 2 ~ 0.6,
             AUT4 > 1 ~ 0.4, 
             AUT4 > 0 ~ 0.2   #very positive 
           ))

## old authoritarianism binary variable ##

# choose threshold (change 2 to 9 or 0 depending on your policy)
min_answered <- 2L

# columns list
aut1_vars <- paste0("AUT1_", 1:5, "_v2")
aut2_vars <- paste0("AUT2_", 1:2, "_v2")
all_aut_vars <- c(aut1_vars, aut2_vars, "AUT3_v2", "AUT4_v2")

# recompute row means (clean numeric vector)
tmp_means2 <- as.numeric(rowMeans(as.data.frame(hu_survey[all_aut_vars]), na.rm = TRUE))
attributes(tmp_means2) <- NULL

# assign into a fresh safe name and then mask only the rows with too few answers
hu_survey[["AUT_index_fixed"]] <- tmp_means2
hu_survey[["AUT_index_fixed"]][ which(hu_survey$answered_n < min_answered) ] <- NA_real_

# create binary if you want
hu_survey[["AUT_index_fixed_binary"]] <- NA_integer_
ok_idx <- which(!is.na(hu_survey[["AUT_index_fixed"]]))
hu_survey[["AUT_index_fixed_binary"]][ok_idx] <- ifelse(hu_survey[["AUT_index_fixed"]][ok_idx] >= 0.885, 1L, 0L)

summary(hu_survey[["AUT_index_fixed"]])

hu_survey <- hu_survey %>% 
  mutate(new_aut_binary = 
           dplyr::case_when(
             AUT_index_fixed >= 0.6 ~ 1,  # Democratic Leaning: 1024
             AUT_index_fixed < 0.6 ~ 0.   #Illiberal Leaning: 976  
           )
  )

table(hu_survey$new_aut_binary)

######## ALL RANDS AGGREGATED task start ########

## aggregate across all RANDS ##

##### aggregating all 7 treatments ##### 

#keep the four panels and the comparison between dosage 1 and dosage 2 but 
#treat each response to each of the 3 * 7 RAND's questions as individual respondents 

#questions of interest: Q2 (confidence in Fidesz), Q3 (anger), Q6 (interest in opposition) 

# Step 0: Create row_id to preserve match
hu_survey <- hu_survey %>%
  mutate(row_id = row_number())

# Step 1: Reshape combined responses for Q2, Q3, Q6
responses_long <- hu_survey %>%
  select(row_id, matches("^RAND[1-7]_Q[236]_combined$")) %>%
  pivot_longer(
    cols = -row_id,
    names_to = c("rand_index", "question"),
    names_pattern = "^RAND([1-7])_Q([236])_combined$",
    values_to = "response"
  ) %>%
  mutate(rand_index = as.integer(rand_index),
         question = paste0("Q", question))  # Q2, Q3, or Q6

# Step 2: Reshape dosage group info
dosage_long <- hu_survey %>%
  select(row_id, matches("^dosage_group_RAND[1-7]$")) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "rand_index",
    names_pattern = "^dosage_group_RAND([1-7])$",
    values_to = "dosage_group"
  ) %>%
  mutate(rand_index = as.integer(rand_index))

# Step 3: Join and clean up
hu_survey_long <- left_join(responses_long, dosage_long,
                            by = c("row_id", "rand_index")) %>%
  filter(!is.na(response), dosage_group %in% c("Dosage 1", "Dosage 2")) %>%
  mutate(
    dosage_group = factor(dosage_group, levels = c("Dosage 1", "Dosage 2"))
  ) %>%
  # Bring back in predictors and covariates
  left_join(
    hu_survey %>%
      select(row_id,
             comp_score, fidesz_or_not, education_level, pol_lean, comp_high,
             new_aut_binary, new_illib_binary, 
             DEM1, DEM2, DEM3, DEM4, DEM5, HHD, POL1, DEM6),
    by = "row_id"
  )

##### Fidesz only, contrasting dosage 1 versus dosage 2 ##### 

## does the dosage you receive affect your confidence in the government ## 

summary_df <- hu_survey_long %>%
  filter(
    fidesz_or_not == 1,
    question == "Q2",
    dosage_group %in% c("Dosage 1", "Dosage 2")
  ) %>%
  group_by(dosage_group) %>%
  summarise(
    mean_response = mean(response, na.rm = TRUE),
    n = sum(!is.na(response)),
    sd = sd(response, na.rm = TRUE),
    se = sd / sqrt(n),
    ci90 = qt(0.95, df = n - 1) * se,
    ci95 = qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

ggplot(summary_df, aes(x = dosage_group, y = mean_response)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0("n=", n),
        y = mean_response + ci95),
    nudge_y = 0.04,
    size = 3.4,
    color = "gray30"
  )+
  geom_errorbar(
    aes(ymin = mean_response - ci95,
        ymax = mean_response + ci95),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_errorbar(
    aes(ymin = mean_response - ci90,
        ymax = mean_response + ci90),
    width = 0.08,
    linewidth = 1.2
  ) +
  labs(
    x = "Dosage group",
    y = "Average confidence in the Fidesz government",
    title = "Effect of Information Dosage on Confidence among Fidesz Supporters",
    subtitle = "0 == losing confidence completely; 1 == confidence remains unchanged
    *thick bars = 90% CI, thin bars = 95% CI"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()

## does the dosage you receive affect your anger ## 

summary_df <- hu_survey_long %>%
  filter(
    fidesz_or_not == 1,
    question == "Q3",
    dosage_group %in% c("Dosage 1", "Dosage 2")
  ) %>%
  group_by(dosage_group) %>%
  summarise(
    mean_response = mean(response, na.rm = TRUE),
    n = sum(!is.na(response)),
    sd = sd(response, na.rm = TRUE),
    se = sd / sqrt(n),
    ci90 = qt(0.95, df = n - 1) * se,
    ci95 = qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

ggplot(summary_df, aes(x = dosage_group, y = mean_response)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0("n=", n),
        y = mean_response + ci95),
    nudge_y = 0.04,
    size = 3.4,
    color = "gray30"
  )+
  geom_errorbar(
    aes(ymin = mean_response - ci95,
        ymax = mean_response + ci95),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_errorbar(
    aes(ymin = mean_response - ci90,
        ymax = mean_response + ci90),
    width = 0.08,
    linewidth = 1.2
  ) +
  labs(
    x = "Dosage group",
    y = "Average level of anger",
    title = "Effect of Information Dosage on Degree of Anger among Fidesz Supporters",
    subtitle = "0 == completely indifferent; 1 == very angry
    *thick bars = 90% CI, thin bars = 95% CI"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()

## does the dosage affect your interest in the opposition ## 

summary_df <- hu_survey_long %>%
  filter(
    fidesz_or_not == 1,
    question == "Q6",
    dosage_group %in% c("Dosage 1", "Dosage 2")
  ) %>%
  group_by(dosage_group) %>%
  summarise(
    mean_response = mean(response, na.rm = TRUE),
    n = sum(!is.na(response)),
    sd = sd(response, na.rm = TRUE),
    se = sd / sqrt(n),
    ci90 = qt(0.95, df = n - 1) * se,
    ci95 = qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

ggplot(summary_df, aes(x = dosage_group, y = mean_response)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0("n=", n),
        y = mean_response + ci95),
    nudge_y = 0.04,
    size = 3.4,
    color = "gray30"
  )+
  geom_errorbar(
    aes(ymin = mean_response - ci95,
        ymax = mean_response + ci95),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_errorbar(
    aes(ymin = mean_response - ci90,
        ymax = mean_response + ci90),
    width = 0.08,
    linewidth = 1.2
  ) +
  labs(
    x = "Dosage group",
    y = "Average interest in the Opposition",
    title = "Effect of Information Dosage on Interest in the Opposition among Fidesz Supporters",
    subtitle = "0 == does not change my interest at all; 1 == increases my interest substantially
    *thick bars = 90% CI, thin bars = 95% CI"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic()

##### Fidesz only, contrasting more versus less liberal #####

# Helper to produce the summary table + plot for a single question

plot_by_lean_fidesz <- function(question_code, y_label, plot_title, plot_subtitle,
                                id_col = "respondent_id") {
  
  has_id <- id_col %in% names(hu_survey_long)
  
  df <- hu_survey_long %>%
    filter(
      question == question_code,
      fidesz_or_not == 1,                      # <-- restrict to Fidesz supporters
      as.character(pol_lean) %in% c("0", "1", "2")
    ) %>%
    mutate(
      pol_lean_chr = as.character(pol_lean),
      pol_lean_f = factor(pol_lean_chr,
                          levels = c("0","1","2"),
                          labels = c("Left","Center","Right"))
    )
  
  if (has_id) {
    # count unique respondents per group (recommended)
    summary_df <- df %>%
      group_by(pol_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),             # unique person count
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  } else {
    # fall back to row counts (as before)
    summary_df <- df %>%
      group_by(pol_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  }
  
  # handle small-n cases safely
  summary_df <- summary_df %>%
    mutate(
      ci90 = ifelse(is.na(ci90) & n > 0, NA_real_, ci90),
      ci95 = ifelse(is.na(ci95) & n > 0, NA_real_, ci95)
    )
  
  # Plot (with n labels above points)
  p <- ggplot(summary_df, aes(x = pol_lean_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Political leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0,1)) +
    theme_classic()
  
  print(summary_df)
  return(p)
}

# --- Examples: produce the three requested plots ---

# Q2: Average confidence in the Fidesz government
p_q2 <- plot_by_lean_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Political Leaning on Confidence in the Government",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2)

# Q3: Average level of anger
p_q3 <- plot_by_lean_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Political Leaning on Degree of Anger",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3)

# Q6: Average interest in the Opposition
p_q6 <- plot_by_lean_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Political Leaning on Interest in the Opposition",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6)

##### Fidesz only, contrasting high/low media embeddedness #####

plot_by_embeddedness_fidesz <- function(question_code, y_label, plot_title, plot_subtitle,
                                        id_col = "respondent_id") {
  
  has_id <- id_col %in% names(hu_survey_long)
  
  df <- hu_survey_long %>%
    filter(
      question == question_code,
      fidesz_or_not == 1,              # <-- restrict to Fidesz supporters
      comp_high %in% c(0, 1)
    ) %>%
    mutate(
      comp_high_f = factor(comp_high, levels = c(0, 1),
                           labels = c("Low embeddedness", "High embeddedness"))
    )
  
  if (has_id) {
    summary_df <- df %>%
      group_by(comp_high_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # count unique respondents
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(comp_high_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  }
  
  # safety for small-n
  summary_df <- summary_df %>%
    mutate(
      ci90 = ifelse(is.na(ci90) & n > 0, NA_real_, ci90),
      ci95 = ifelse(is.na(ci95) & n > 0, NA_real_, ci95)
    )
  
  p <- ggplot(summary_df, aes(x = comp_high_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Media embeddedness", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(summary_df)
  return(p)
}

# ---------------- Example calls ----------------

# Q2 — confidence in the government (embeddedness)
p_q2_emb <- plot_by_embeddedness_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Media Embeddedness on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_emb)

# Q3 — anger (embeddedness)
p_q3_emb <- plot_by_embeddedness_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Media Embeddedness on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_emb)

# Q6 — interest in the opposition (embeddedness)
p_q6_emb <- plot_by_embeddedness_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Media Embeddedness on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_emb)

# Plot by education (Fidesz supporters only) # 
plot_by_education_fidesz <- function(question_code, y_label, plot_title, plot_subtitle,
                                     id_col = "respondent_id") {
  
  has_id <- id_col %in% names(hu_survey_long)
  
  df <- hu_survey_long %>%
    filter(
      question == question_code,
      fidesz_or_not == 1,              # <-- restrict to Fidesz supporters
      education_level %in% c(0, 1)
    ) %>%
    mutate(
      education_f = factor(education_level, levels = c(0, 1),
                           labels = c("Low education", "High education"))
    )
  
  if (has_id) {
    summary_df <- df %>%
      group_by(education_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(education_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  }
  
  summary_df <- summary_df %>%
    mutate(
      ci90 = ifelse(is.na(ci90) & n > 0, NA_real_, ci90),
      ci95 = ifelse(is.na(ci95) & n > 0, NA_real_, ci95)
    )
  
  p <- ggplot(summary_df, aes(x = education_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Education level", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(summary_df)
  return(p)
}

# ---- Education-based plots (Fidesz only) ----

p_q2_edu <- plot_by_education_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Education on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_edu)

p_q3_edu <- plot_by_education_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Education on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_edu)

p_q6_edu <- plot_by_education_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Education on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_edu)

### RANDS aggregated - dosage & media embeddedness effect ### 

dodge_width <- 0.6

# Updated helper: plot by media embeddedness (Fidesz supporters only) with dosage grouping
plot_by_dos_embeddedness_fidesz <- function(question_code,
                                        y_label,
                                        plot_title,
                                        plot_subtitle,
                                        id_col = "respondent_id",
                                        data = hu_survey_long,
                                        dosage_col = "dosage_group",      # expects "Dosage 1"/"Dosage 2"
                                        embedded_col = "comp_high",       # expects 0/1
                                        fidesz_col = "fidesz_or_not",
                                        colors = c("#1f77b4", "#ff7f0e")) {
  
  # check for id presence
  has_id <- id_col %in% names(data)
  if (!embedded_col %in% names(data)) stop("embedded_col not found in data")
  if (!dosage_col %in% names(data)) stop("dosage_col not found in data")
  
  # prepare df filtered to question + Fidesz supporters + comp_high 0/1
  df <- data %>%
    filter(
      question == question_code,
      .data[[fidesz_col]] == 1,
      .data[[embedded_col]] %in% c(0, 1)
    ) %>%
    mutate(
      embedded_f = factor(.data[[embedded_col]], levels = c(0, 1),
                          labels = c("Low embeddedness", "High embeddedness")),
      dosage_f = factor(.data[[dosage_col]], levels = c("Dosage 1", "Dosage 2"))
    )
  
  # Safety: warn if unexpected dosage labels exist
  if (any(!is.na(df$dosage_f) & !(as.character(df$dosage_f) %in% c("Dosage 1", "Dosage 2")))) {
    warning("Some dosage_group values are not 'Dosage 1' or 'Dosage 2' and will be treated as NA.")
  }
  
  # Summarise by embedded_f x dosage_f
  if (has_id) {
    summary_df <- df %>%
      group_by(embedded_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(embedded_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # compute SE and CIs; require n > 1 for CIs
  summary_df <- summary_df %>%
    mutate(
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
      ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)
    )
  
  # force factor ordering (low -> high embeddedness; Dosage 1 then Dosage 2)
  summary_df <- summary_df %>%
    mutate(
      embedded_f = factor(embedded_f, levels = c("Low embeddedness", "High embeddedness")),
      dosage_f = factor(dosage_f, levels = c("Dosage 1", "Dosage 2"))
    )
  
  # print summary for diagnostics
  print(summary_df)
  
  # Plot: two points per embeddedness level (Dosage 1 & Dosage 2), with CIs
  dodge_width <- 0.6
  p <- ggplot(summary_df, aes(x = embedded_f, y = mean_response, color = dosage_f, group = dosage_f)) +
    # 95% CI (thin)
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  position = position_dodge(width = dodge_width),
                  width = 0.12, linewidth = 0.8, na.rm = TRUE) +
    # 90% CI (thicker)
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  position = position_dodge(width = dodge_width),
                  width = 0.06, linewidth = 1.2, na.rm = TRUE) +
    
  # points (dodge to left/right)
    # points (dodge to left/right)
    geom_point(aes(shape = dosage_f),
               position = position_dodge(width = dodge_width),
               size = 3) +
    
    # labels above the top of the 95% CI, dodged to align with points,
    geom_label(
      aes(label = paste0("n=", n),
          y = mean_response + ci95 + 0.02),    # explicit y anchor + small offset
      position = position_dodge(width = dodge_width),
      size = 3.2,
      color = "black",
      fill = "white",
      label.size = NA,        # no border line; use 0 if NA not supported
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_color_manual(values = colors, name = "Dosage") +
    scale_shape_manual(values = c(16, 17), name = "Dosage") +    # distinct shapes for clarity
    labs(x = "Media embeddedness", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 10),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

#call functions# 

p_q2_emb <- plot_by_dos_embeddedness_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Media Embeddedness & Dosage on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_emb)

p_q3_emb <- plot_by_dos_embeddedness_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Media Embeddedness & Dosage on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_emb)

p_q6_emb <- plot_by_dos_embeddedness_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Media Embeddedness & Dosage on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_emb)

### RANDS aggregated - dosage & education effect ### 

dodge_width <- 0.6

# Updated helper: plot by education (Fidesz supporters only) with dosage grouping
plot_by_education_dos_fidesz <- function(question_code,
                                     y_label,
                                     plot_title,
                                     plot_subtitle,
                                     id_col = "respondent_id",
                                     data = hu_survey_long,
                                     dosage_col = "dosage_group",               # name of column with "Dosage 1"/"Dosage 2"
                                     education_col = "education_level",         # 0/1 as before
                                     fidesz_col = "fidesz_or_not",              # flag for Fidesz supporters
                                     colors = c("#1f77b4", "#ff7f0e")) {
  
  # check for id presence
  has_id <- id_col %in% names(data)
  # ensure the dosage column exists
  if (!dosage_col %in% names(data)) stop("dosage_col not found in data")
  
  # prepare df filtered to question + Fidesz supporters + education levels 0/1
  df <- data %>%
    filter(
      question == question_code,
      .data[[fidesz_col]] == 1,
      .data[[education_col]] %in% c(0, 1)
    ) %>%
    mutate(
      education_f = factor(.data[[education_col]], levels = c(0, 1),
                           labels = c("Low education", "High education")),
      dosage_f = factor(.data[[dosage_col]], levels = c("Dosage 1", "Dosage 2"))
    )
  
  # Safety: if dosage labels differ in exact text, coerce any non-matching to NA and warn
  if (any(!is.na(df$dosage_f) & !(as.character(df$dosage_f) %in% c("Dosage 1", "Dosage 2")))) {
    warning("Some dosage_group values are not 'Dosage 1' or 'Dosage 2' and will be treated as NA.")
  }
  
  # Summarise by education_f x dosage_f
  if (has_id) {
    summary_df <- df %>%
      group_by(education_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(education_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # compute SE and CIs; if n <= 1 then set CI to NA
  summary_df <- summary_df %>%
    mutate(
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
      ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)
    )
  
  # ensure factor levels (education left-to-right low -> high)
  summary_df <- summary_df %>%
    mutate(
      education_f = factor(education_f, levels = c("Low education", "High education")),
      dosage_f = factor(dosage_f, levels = c("Dosage 1", "Dosage 2"))
    )
  
  # print summary for diagnostics
  print(summary_df)
  
  # Plot: two points per education level, color by dosage, dodged horizontally
  dodge_width <- 0.6
  p <- ggplot(summary_df, aes(x = education_f, y = mean_response, color = dosage_f, group = dosage_f)) +
    # 95% CI (thin)
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  position = position_dodge(width = dodge_width),
                  width = 0.12, linewidth = 0.8, na.rm = TRUE) +
    # 90% CI (thicker)
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  position = position_dodge(width = dodge_width),
                  width = 0.06, linewidth = 1.2, na.rm = TRUE) +# points (dodge to left/right)
    geom_point(aes(shape = dosage_f),
               position = position_dodge(width = dodge_width),
               size = 3) +
    
    # labels above the top of the 95% CI, dodged to align with points,
    # using geom_label so labels are legible (white box), and no border (label.size = 0).
    geom_label(
      aes(label = paste0("n=", n),
          y = mean_response + ci95 + 0.02),    # explicit y anchor + small offset
      position = position_dodge(width = dodge_width),
      size = 3.2,
      color = "black",
      fill = "white",
      label.size = NA,        # no border line; use 0 if NA not supported
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    geom_point(aes(shape = dosage_f), position = position_dodge(width = dodge_width), size = 3) +
    scale_color_manual(values = colors, name = "Dosage") +
    scale_shape_manual(values = c(16, 17), name = "Dosage") +    # different shapes for clarity
    labs(x = "Education level", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 10),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

p_q2_edu <- plot_by_education_dos_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Education & Dosage on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_edu)

p_q3_edu <- plot_by_education_dos_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Education & Dosage on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_edu)

p_q6_edu <- plot_by_education_dos_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Education & Dosage on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_edu)


######## RANDS DISAGGREGATED task start ########

### FIDESZ ONLY comparing Dosage 1 versus Dosage 2, RAND1 - RAND7 ### 

# Helper: map RAND -> RAND<r>_<index>_<suffix>, where index = 2*r-1 (dosage1) or 2*r (dosage2)
rand_varname_mapped <- function(rand, tre_suffix, dosage = 1) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  if (!(dosage %in% c(1,2))) stop("dosage must be 1 or 2")
  idx <- if (dosage == 1) 2L*rand - 1L else 2L*rand
  paste0("RAND", rand, "_", idx, "_", tre_suffix)
}

# Core plotting helper using mapped RAND names
plot_rand_generic_mapped <- function(data, rand, tre_suffix,
                                     x_label, y_label, title, subtitle,
                                     y_limits = c(0,1)) {
  
  # internal filter: Fidesz supporters only
  data_f <- data %>% filter(fidesz_or_not == 1)
  
  var1 <- rand_varname_mapped(rand, tre_suffix, dosage = 1) # RANDr_(2r-1)_y
  var2 <- rand_varname_mapped(rand, tre_suffix, dosage = 2) # RANDr_(2r)_y
  
  if (!var1 %in% names(data_f)) stop(paste0("Missing variable: ", var1))
  if (!var2 %in% names(data_f)) stop(paste0("Missing variable: ", var2))
  
  # Build long df: one response per respondent per dosage (aligned by respondent)
  df_tmp <- tibble::tibble(
    dosage_group = rep(c("Dosage 1", "Dosage 2"), each = nrow(data_f)),
    response = c(as.numeric(data_f[[var1]]), as.numeric(data_f[[var2]]))
  )
  
  summary_df <- df_tmp %>%
    group_by(dosage_group) %>%
    summarise(
      mean_response = mean(response, na.rm = TRUE),
      n = sum(!is.na(response)),
      sd = sd(response, na.rm = TRUE),
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      # only compute t-based CIs when n > 1; otherwise set to 0 (plot will show no bar)
      ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, 0),
      ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, 0),
      .groups = "drop"
    )
  
  p <- ggplot(summary_df, aes(x = dosage_group, y = mean_response)) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2) +
    labs(x = x_label, y = y_label, title = title, subtitle = subtitle) +
    scale_y_continuous(limits = y_limits) +
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- QUESTION-SPECIFIC FUNCTIONS (using mapped RAND variables) ----------

# Confidence in government => tre_suffix = 2
plot_confidence_by_rand <- function(rand, data) {
  plot_rand_generic_mapped(
    data = data,
    rand = rand,
    tre_suffix = 2,
    x_label = "Dosage group",
    y_label = "Average confidence in the Fidesz government",
    title = "Effect of Information Dosage on Confidence among Fidesz Supporters",
    subtitle = "0 == losing confidence completely; 1 == confidence remains unchanged\n*thick bars = 90% CI, thin bars = 95% CI"
  )
}

# Anger => tre_suffix = 3
plot_anger_by_rand <- function(rand, data) {
  plot_rand_generic_mapped(
    data = data,
    rand = rand,
    tre_suffix = 3,
    x_label = "Dosage group",
    y_label = "Average level of anger",
    title = "Effect of Information Dosage on Degree of Anger among Fidesz Supporters",
    subtitle = "0 == completely indifferent; 1 == very angry\n*thick bars = 90% CI, thin bars = 95% CI"
  )
}

# Interest in opposition => tre_suffix = 6
plot_interest_by_rand <- function(rand, data) {
  plot_rand_generic_mapped(
    data = data,
    rand = rand,
    tre_suffix = 6,
    x_label = "Dosage group",
    y_label = "Average interest in the Opposition",
    title = "Effect of Information Dosage on Interest in the Opposition among Fidesz Supporters",
    subtitle = "0 == does not change my interest at all; 1 == increases my interest substantially
*thick bars = 90% CI, thin bars = 95% CI"
  )
}

# ---------- CALLS (7 per function) ----------
# Now you can call these with RAND = 1..7. The functions filter internally to fidesz_or_not == 1.

# Example calls (confidence)
plot_confidence_by_rand(1, hu_survey)
plot_confidence_by_rand(2, hu_survey)
plot_confidence_by_rand(3, hu_survey)
plot_confidence_by_rand(4, hu_survey)
plot_confidence_by_rand(5, hu_survey)
plot_confidence_by_rand(6, hu_survey)
plot_confidence_by_rand(7, hu_survey)

# Example calls (anger)
plot_anger_by_rand(1, hu_survey)
plot_anger_by_rand(2, hu_survey)
plot_anger_by_rand(3, hu_survey)
plot_anger_by_rand(4, hu_survey)
plot_anger_by_rand(5, hu_survey)
plot_anger_by_rand(6, hu_survey)
plot_anger_by_rand(7, hu_survey)

# Example calls (interest)
plot_interest_by_rand(1, hu_survey)
plot_interest_by_rand(2, hu_survey)
plot_interest_by_rand(3, hu_survey)
plot_interest_by_rand(4, hu_survey)
plot_interest_by_rand(5, hu_survey)
plot_interest_by_rand(6, hu_survey)
plot_interest_by_rand(7, hu_survey)

### FIDESZ ONLY comparing effect of political leaning [left, centrist, right], RAND1 - RAND7 ###

# Helper to construct combined RAND varname: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper that builds summary & plot for a given RAND and question code
# (NO n labels on the plot)
plot_by_lean_rand_combined <- function(data, rand, qcode,
                                       y_label, plot_title, plot_subtitle,
                                       id_col = "respondent_id") {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  
  # restrict to Fidesz supporters and valid pol_lean values
  df <- data %>%
    filter(fidesz_or_not == 1, as.character(pol_lean) %in% c("0", "1", "2")) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      pol_lean_chr = as.character(pol_lean),
      pol_lean_f = factor(pol_lean_chr, levels = c("0", "1", "2"),
                          labels = c("Left", "Center", "Right"))
    )
  
  has_id <- id_col %in% names(df)
  
  if (has_id) {
    summary_df <- df %>%
      group_by(pol_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),               # unique respondents
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(pol_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  }
  
  # ensure presence of all three factor levels (so plot always shows Left/Center/Right)
  all_levels <- factor(c("Left", "Center", "Right"), levels = c("Left", "Center", "Right"))
  summary_df <- summary_df %>%
    tidyr::complete(pol_lean_f = all_levels, fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(pol_lean_f)
  
  # Print summary table
  print(summary_df)
  
  # Plot (no geom_text for n labels)
  p <- ggplot(summary_df, aes(x = pol_lean_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Political leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrapper functions (Q2, Q3, Q6) ----------

# Q2: confidence in government
plot_q2_by_lean <- function(rand, data, id_col = "respondent_id") {
  plot_by_lean_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = "Effect of Political Leaning on Confidence in the Government",
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q3: anger
plot_q3_by_lean <- function(rand, data, id_col = "respondent_id") {
  plot_by_lean_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = "Effect of Political Leaning on Degree of Anger",
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q6: interest in opposition
plot_q6_by_lean <- function(rand, data, id_col = "respondent_id") {
  plot_by_lean_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = "Effect of Political Leaning on Interest in the Opposition",
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (7 per function) ----------
# Calls for Q2
plot_q2_by_lean(1, hu_survey)
plot_q2_by_lean(2, hu_survey)
plot_q2_by_lean(3, hu_survey)
plot_q2_by_lean(4, hu_survey)
plot_q2_by_lean(5, hu_survey)
plot_q2_by_lean(6, hu_survey)
plot_q2_by_lean(7, hu_survey)

# Calls for Q3
plot_q3_by_lean(1, hu_survey)
plot_q3_by_lean(2, hu_survey)
plot_q3_by_lean(3, hu_survey)
plot_q3_by_lean(4, hu_survey)
plot_q3_by_lean(5, hu_survey)
plot_q3_by_lean(6, hu_survey)
plot_q3_by_lean(7, hu_survey)

# Calls for Q6
plot_q6_by_lean(1, hu_survey)
plot_q6_by_lean(2, hu_survey)
plot_q6_by_lean(3, hu_survey)
plot_q6_by_lean(4, hu_survey)
plot_q6_by_lean(5, hu_survey)
plot_q6_by_lean(6, hu_survey)
plot_q6_by_lean(7, hu_survey)

### FIDESZ ONLY comparing high/low media embeddedness for RAND1 - RAND7 ### 

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare embeddedness (comp_high == 0 vs 1) for one RAND and question
plot_by_embeddedness_rand_combined <- function(data, rand, qcode,
                                               y_label, plot_title, plot_subtitle,
                                               id_col = "respondent_id") {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  
  # restrict to Fidesz supporters and comp_high coded 0/1
  df <- data %>%
    filter(fidesz_or_not == 1, comp_high %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      comp_high_f = factor(comp_high, levels = c(0, 1),
                           labels = c("Low embeddedness", "High embeddedness"))
    )
  
  has_id <- id_col %in% names(df)
  
  if (has_id) {
    summary_df <- df %>%
      group_by(comp_high_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(comp_high_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  }
  
  # ensure both factor levels appear
  all_levels <- factor(c("Low embeddedness", "High embeddedness"),
                       levels = c("Low embeddedness", "High embeddedness"))
  summary_df <- summary_df %>%
    complete(comp_high_f = all_levels,
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(comp_high_f)
  
  # print summary for inspection
  print(summary_df)
  
  # plot (no n labels)
  p <- ggplot(summary_df, aes(x = comp_high_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Media embeddedness", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------

# Q2: confidence in government
plot_q2_by_embeddedness_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Media Embeddedness on Confidence (RAND ", rand, ")"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q3: anger
plot_q3_by_embeddedness_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Media Embeddedness on Degree of Anger (RAND ", rand, ")"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q6: interest in opposition
plot_q6_by_embeddedness_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Media Embeddedness on Interest in the Opposition (RAND ", rand, ")"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_embeddedness_rand(1, hu_survey)
plot_q2_by_embeddedness_rand(2, hu_survey)
plot_q2_by_embeddedness_rand(3, hu_survey)
plot_q2_by_embeddedness_rand(4, hu_survey)
plot_q2_by_embeddedness_rand(5, hu_survey)
plot_q2_by_embeddedness_rand(6, hu_survey)
plot_q2_by_embeddedness_rand(7, hu_survey)

# Q3 calls
plot_q3_by_embeddedness_rand(1, hu_survey)
plot_q3_by_embeddedness_rand(2, hu_survey)
plot_q3_by_embeddedness_rand(3, hu_survey)
plot_q3_by_embeddedness_rand(4, hu_survey)
plot_q3_by_embeddedness_rand(5, hu_survey)
plot_q3_by_embeddedness_rand(6, hu_survey)
plot_q3_by_embeddedness_rand(7, hu_survey)

# Q6 calls
plot_q6_by_embeddedness_rand(1, hu_survey)
plot_q6_by_embeddedness_rand(2, hu_survey)
plot_q6_by_embeddedness_rand(3, hu_survey)
plot_q6_by_embeddedness_rand(4, hu_survey)
plot_q6_by_embeddedness_rand(5, hu_survey)
plot_q6_by_embeddedness_rand(6, hu_survey)
plot_q6_by_embeddedness_rand(7, hu_survey)

### FIDESZ ONLY comparing high/low education x dosage 1/2, RAND 1 - RAND7 ### 

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare embeddedness (comp_high == 0 vs 1) for one RAND and question,
# with Dosage 1 vs Dosage 2 comparison. Automatically uses dosage_group_RAND<r> by default.
plot_by_dos_embeddedness_rand_combined <- function(data, rand, qcode,
                                                   y_label, plot_title, plot_subtitle,
                                                   id_col = "respondent_id",
                                                   dosage_col = NULL,    # if NULL we build dosage_group_RAND<rand>
                                                   embedded_col = "comp_high",
                                                   fidesz_col = "fidesz_or_not",
                                                   colors = c("#1f77b4", "#ff7f0e")) {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  if (!embedded_col %in% names(data)) stop("embedded_col not found in data")
  if (!fidesz_col %in% names(data)) stop("fidesz_col not found in data")
  
  # Default dosage column for this RAND (if caller didn't pass one)
  default_dosage_col <- paste0("dosage_group_RAND", rand)
  if (is.null(dosage_col)) {
    dosage_col <- default_dosage_col
  }
  
  if (!dosage_col %in% names(data)) {
    stop(paste0("dosage_col '", dosage_col, "' not found in data. Expected e.g. '", default_dosage_col, "'"))
  }
  
  # restrict to Fidesz supporters and comp_high coded 0/1
  df <- data %>%
    filter(.data[[fidesz_col]] == 1, .data[[embedded_col]] %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      embedded_f = factor(.data[[embedded_col]], levels = c(0, 1),
                          labels = c("Low embeddedness", "High embeddedness")),
      # use actual dosage column for this RAND
      dosage_f = factor(.data[[dosage_col]], levels = c("Dosage 1", "Dosage 2"))
    )
  
  # Safety: warn if unexpected dosage labels exist
  if (any(!is.na(df$dosage_f) & !(as.character(df$dosage_f) %in% c("Dosage 1", "Dosage 2")))) {
    warning("Some dosage values are not exactly 'Dosage 1' or 'Dosage 2' and will be treated as NA.")
  }
  
  has_id <- id_col %in% names(df)
  
  # Summarise by embedded_f x dosage_f
  if (has_id) {
    summary_df <- df %>%
      group_by(embedded_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(embedded_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # compute SE and CIs; require n > 1 for CIs
  summary_df <- summary_df %>%
    mutate(
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
      ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)
    )
  
  # ensure both factor levels appear for embeddedness × dosage (so plot always shows 4 points)
  embedded_levels <- c("Low embeddedness", "High embeddedness")
  dosage_levels <- c("Dosage 1", "Dosage 2")
  summary_df <- summary_df %>%
    complete(embedded_f = factor(embedded_levels, levels = embedded_levels),
             dosage_f = factor(dosage_levels, levels = dosage_levels),
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(embedded_f, dosage_f)
  
  # print summary for inspection
  print(summary_df)
  
  # Plot: two points per embeddedness level (Dosage 1 & Dosage 2), with CIs
  dodge_width <- 0.6
  p <- ggplot(summary_df, aes(x = embedded_f, y = mean_response, color = dosage_f, group = dosage_f)) +
    # 95% CI (thin)
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  position = position_dodge(width = dodge_width),
                  width = 0.12, linewidth = 0.8, na.rm = TRUE) +
    # 90% CI (thicker)
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  position = position_dodge(width = dodge_width),
                  width = 0.06, linewidth = 1.2, na.rm = TRUE) +
    # points (dodge to left/right)
    geom_point(aes(shape = dosage_f),
               position = position_dodge(width = dodge_width),
               size = 3) +
    
    # labels above the top of the 95% CI, dodged to align with points,
    # using geom_label so labels are legible (white box), and no border (label.size = 0).
    geom_label(
      aes(label = paste0("n=", n),
          y = mean_response + ci95 + 0.02),    # explicit y anchor + small offset
      position = position_dodge(width = dodge_width),
      size = 3.2,
      color = "black",
      fill = "white",
      label.size = NA,        # no border line; use 0 if NA not supported
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_color_manual(values = colors, name = "Dosage") +
    scale_shape_manual(values = c(16, 17), name = "Dosage") +
    labs(x = "Media embeddedness", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 10),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------
# wrappers now rely on the core helper which will use dosage_group_RAND<rand> automatically

plot_q2_by_dos_embeddedness_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Media Embeddedness on Confidence (RAND ", rand, ")"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

plot_q3_by_dos_embeddedness_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Media Embeddedness on Degree of Anger (RAND ", rand, ")"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

plot_q6_by_dos_embeddedness_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_embeddedness_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Media Embeddedness on Interest in the Opposition (RAND ", rand, ")"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_dos_embeddedness_rand(1, hu_survey)
plot_q2_by_dos_embeddedness_rand(2, hu_survey)
plot_q2_by_dos_embeddedness_rand(3, hu_survey)
plot_q2_by_dos_embeddedness_rand(4, hu_survey)
plot_q2_by_dos_embeddedness_rand(5, hu_survey)
plot_q2_by_dos_embeddedness_rand(6, hu_survey)
plot_q2_by_dos_embeddedness_rand(7, hu_survey)

# Q3 calls
plot_q3_by_dos_embeddedness_rand(1, hu_survey)
plot_q3_by_dos_embeddedness_rand(2, hu_survey)
plot_q3_by_dos_embeddedness_rand(3, hu_survey)
plot_q3_by_dos_embeddedness_rand(4, hu_survey)
plot_q3_by_dos_embeddedness_rand(5, hu_survey)
plot_q3_by_dos_embeddedness_rand(6, hu_survey)
plot_q3_by_dos_embeddedness_rand(7, hu_survey)

# Q6 calls
plot_q6_by_dos_embeddedness_rand(1, hu_survey)
plot_q6_by_dos_embeddedness_rand(2, hu_survey)
plot_q6_by_dos_embeddedness_rand(3, hu_survey)
plot_q6_by_dos_embeddedness_rand(4, hu_survey)
plot_q6_by_dos_embeddedness_rand(5, hu_survey)
plot_q6_by_dos_embeddedness_rand(6, hu_survey)
plot_q6_by_dos_embeddedness_rand(7, hu_survey)


### FIDESZ ONLY comparing high/low education, RAND1 - RAND7 ### 

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare education_level (0 vs 1) for one RAND and question
plot_by_education_rand_combined <- function(data, rand, qcode,
                                            y_label, plot_title, plot_subtitle,
                                            id_col = "respondent_id") {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  
  # restrict to Fidesz supporters and education_level coded 0/1
  df <- data %>%
    filter(fidesz_or_not == 1, education_level %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      education_f = factor(education_level, levels = c(0, 1),
                           labels = c("Low education", "High education"))
    )
  
  has_id <- id_col %in% names(df)
  
  if (has_id) {
    summary_df <- df %>%
      group_by(education_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(education_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  }
  
  # ensure both factor levels appear
  all_levels <- factor(c("Low education", "High education"),
                       levels = c("Low education", "High education"))
  summary_df <- summary_df %>%
    complete(education_f = all_levels,
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(education_f)
  
  # print summary for inspection
  print(summary_df)
  
  # plot (no n labels)
  p <- ggplot(summary_df, aes(x = education_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Education level", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------

# Q2: confidence in government
plot_q2_by_education_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_education_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Education on Confidence (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q3: anger
plot_q3_by_education_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_education_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Education on Degree of Anger (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q6: interest in opposition
plot_q6_by_education_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_education_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Education on Interest in the Opposition (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_education_rand(1, hu_survey)
plot_q2_by_education_rand(2, hu_survey)
plot_q2_by_education_rand(3, hu_survey)
plot_q2_by_education_rand(4, hu_survey)
plot_q2_by_education_rand(5, hu_survey)
plot_q2_by_education_rand(6, hu_survey)
plot_q2_by_education_rand(7, hu_survey)

# Q3 calls
plot_q3_by_education_rand(1, hu_survey)
plot_q3_by_education_rand(2, hu_survey)
plot_q3_by_education_rand(3, hu_survey)
plot_q3_by_education_rand(4, hu_survey)
plot_q3_by_education_rand(5, hu_survey)
plot_q3_by_education_rand(6, hu_survey)
plot_q3_by_education_rand(7, hu_survey)

# Q6 calls
plot_q6_by_education_rand(1, hu_survey)
plot_q6_by_education_rand(2, hu_survey)
plot_q6_by_education_rand(3, hu_survey)
plot_q6_by_education_rand(4, hu_survey)
plot_q6_by_education_rand(5, hu_survey)
plot_q6_by_education_rand(6, hu_survey)
plot_q6_by_education_rand(7, hu_survey)

### FIDESZ ONLY comparing high/low media embeddedness x dosage 1/2, RAND 1 - RAND7 ### 

dodge_width <- 0.6      

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare education_level (0 vs 1) for one RAND and question,
# now with Dosage 1 vs Dosage 2 comparison. Uses dosage_group_RAND<rand> by default.
plot_by_dos_education_rand_combined <- function(data, rand, qcode,
                                            y_label, plot_title, plot_subtitle,
                                            id_col = "respondent_id",
                                            dosage_col = NULL,      # defaults to dosage_group_RAND<rand>
                                            education_col = "education_level",
                                            fidesz_col = "fidesz_or_not",
                                            colors = c("#1f77b4", "#ff7f0e")) {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  if (!education_col %in% names(data)) stop("education_col not found in data")
  if (!fidesz_col %in% names(data)) stop("fidesz_col not found in data")
  
  # default dosage column for this RAND
  default_dosage_col <- paste0("dosage_group_RAND", rand)
  if (is.null(dosage_col)) dosage_col <- default_dosage_col
  
  if (!dosage_col %in% names(data)) {
    stop(paste0("dosage_col '", dosage_col, "' not found in data. Expected e.g. '", default_dosage_col, "'"))
  }
  
  # restrict to Fidesz supporters and education_level coded 0/1
  df <- data %>%
    filter(.data[[fidesz_col]] == 1, .data[[education_col]] %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      education_f = factor(.data[[education_col]], levels = c(0, 1),
                           labels = c("Low education", "High education")),
      dosage_f = factor(.data[[dosage_col]], levels = c("Dosage 1", "Dosage 2"))
    )
  
  # warn if dosage labels unexpected
  if (any(!is.na(df$dosage_f) & !(as.character(df$dosage_f) %in% c("Dosage 1", "Dosage 2")))) {
    warning("Some dosage values are not exactly 'Dosage 1' or 'Dosage 2' and will be treated as NA.")
  }
  
  has_id <- id_col %in% names(df)
  
  # Summarise by education_f x dosage_f
  if (has_id) {
    summary_df <- df %>%
      group_by(education_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(education_f, dosage_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # compute SE and CIs; require n > 1 for CIs
  summary_df <- summary_df %>%
    mutate(
      se = ifelse(n > 0, sd / sqrt(n), NA_real_),
      ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
      ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_)
    )
  
  # ensure both factor levels appear for education × dosage (so plot always shows 4 points)
  education_levels <- c("Low education", "High education")
  dosage_levels <- c("Dosage 1", "Dosage 2")
  summary_df <- summary_df %>%
    complete(education_f = factor(education_levels, levels = education_levels),
             dosage_f = factor(dosage_levels, levels = dosage_levels),
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(education_f, dosage_f)
  
  # print summary for inspection
  print(summary_df)
  
  # Plot: two points per education level (Dosage 1 & Dosage 2), with CIs
  dodge_width <- 0.6
  p <- ggplot(summary_df, aes(x = education_f, y = mean_response, color = dosage_f, group = dosage_f)) +
    # 95% CI (thin)
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  position = position_dodge(width = dodge_width),
                  width = 0.12, linewidth = 0.8, na.rm = TRUE) +
    # 90% CI (thicker)
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  position = position_dodge(width = dodge_width),
                  width = 0.06, linewidth = 1.2, na.rm = TRUE) +
    # points (dodge to left/right)
    geom_point(aes(shape = dosage_f),
               position = position_dodge(width = dodge_width),
               size = 3) +
    
    # labels above the top of the 95% CI, dodged to align with points,
    # using geom_label so labels are legible (white box), and no border (label.size = 0).
    geom_label(
      aes(label = paste0("n=", n),
          y = mean_response + ci95 + 0.02),    # explicit y anchor + small offset
      position = position_dodge(width = dodge_width),
      size = 3.2,
      color = "black",
      fill = "white",
      label.size = NA,        # no border line; use 0 if NA not supported
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_color_manual(values = colors, name = "Dosage") +
    scale_shape_manual(values = c(16, 17), name = "Dosage") +
    labs(x = "Education level", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 10),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------
plot_q2_by_dos_education_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_education_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Education on Confidence (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

plot_q3_by_dos_education_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_education_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Education on Degree of Anger (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

plot_q6_by_dos_education_rand <- function(rand, data = hu_survey, id_col = "respondent_id") {
  plot_by_dos_education_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Education on Interest in the Opposition (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_dos_education_rand(1, hu_survey)
plot_q2_by_dos_education_rand(2, hu_survey)
plot_q2_by_dos_education_rand(3, hu_survey)
plot_q2_by_dos_education_rand(4, hu_survey)
plot_q2_by_dos_education_rand(5, hu_survey)
plot_q2_by_dos_education_rand(6, hu_survey)
plot_q2_by_dos_education_rand(7, hu_survey)

# Q3 calls
plot_q3_by_dos_education_rand(1, hu_survey)
plot_q3_by_dos_education_rand(2, hu_survey)
plot_q3_by_dos_education_rand(3, hu_survey)
plot_q3_by_dos_education_rand(4, hu_survey)
plot_q3_by_dos_education_rand(5, hu_survey)
plot_q3_by_dos_education_rand(6, hu_survey)
plot_q3_by_dos_education_rand(7, hu_survey)

# Q6 calls
plot_q6_by_dos_education_rand(1, hu_survey)
plot_q6_by_dos_education_rand(2, hu_survey)
plot_q6_by_dos_education_rand(3, hu_survey)
plot_q6_by_dos_education_rand(4, hu_survey)
plot_q6_by_dos_education_rand(5, hu_survey)
plot_q6_by_dos_education_rand(6, hu_survey)
plot_q6_by_dos_education_rand(7, hu_survey)

######## ILLIBERALISM SCALES : ######## 

###### New Illiberalism Battery ######

# Plot by illeberalism/democratic leaning (Fidesz supporters only) # 
plot_by_illib_lean_fidesz <- function(question_code, y_label, plot_title, plot_subtitle,
                                      id_col = "respondent_id") {
  
  has_id <- id_col %in% names(hu_survey_long)
  
  df <- hu_survey_long %>%
    filter(
      question == question_code,
      fidesz_or_not == 1,              # <-- restrict to Fidesz supporters
      new_illib_binary %in% c(0, 1)
    ) %>%
    mutate(
      illib_lean_f = factor(new_illib_binary, levels = c(0, 1),
                            labels = c("Illiberal Leaning", "Democratic Leaning"))
    )
  
  if (has_id) {
    summary_df <- df %>%
      group_by(illib_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(illib_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  }
  
  summary_df <- summary_df %>%
    mutate(
      ci90 = ifelse(is.na(ci90) & n > 0, NA_real_, ci90),
      ci95 = ifelse(is.na(ci95) & n > 0, NA_real_, ci95)
    )
  
  p <- ggplot(summary_df, aes(x = illib_lean_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Illiberal/Democratic Leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(summary_df)
  return(p)
}

# ---- Illiberal/Democratic -based plots (Fidesz only) ----

p_q2_illib_lean <- plot_by_illib_lean_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Illiberal/Democratic Leaning on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_illib_lean)

p_q3_illib_lean <- plot_by_illib_lean_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Illiberal/Democratic Leaning on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_illib_lean)

p_q6_illib_lean <- plot_by_illib_lean_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Illiberal/Democratic Leaning on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_illib_lean)

### FIDESZ ONLY comparing Illiberalism/Democratic Leaning, RAND1 - RAND7 ### 

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare new_illib_binary (0 vs 1) for one RAND and question
plot_by_illib_lean_rand_combined <- function(data, rand, qcode,
                                             y_label, plot_title, plot_subtitle,
                                             id_col = "respondent_id") {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  
  # restrict to Fidesz supporters and new_illib_binary coded 0/1
  df <- data %>%
    filter(fidesz_or_not == 1, new_illib_binary %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      illib_lean_f = factor(new_illib_binary, levels = c(0, 1),
                            labels = c("Illiberal Leaning", "Democratic Leaning"))
    )
  
  has_id <- id_col %in% names(df)
  
  if (has_id) {
    summary_df <- df %>%
      group_by(illib_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(illib_lean_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  }
  
  # ensure both factor levels appear
  all_levels <- factor(c("Illiberal Leaning", "Democratic Leaning"),
                       levels = c("Illiberal Leaning", "Democratic Leaning"))
  summary_df <- summary_df %>%
    complete(illib_lean_f = all_levels,
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(illib_lean_f)
  
  # print summary for inspection
  print(summary_df)
  
  # plot (no n labels)
  p <- ggplot(summary_df, aes(x = illib_lean_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Illiberalism/Democratic Leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------

# Q2: confidence in government
plot_q2_by_illib_lean_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_illib_lean_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Illiberalism/Democratic Leaning on Confidence (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q3: anger
plot_q3_by_illib_lean_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_illib_lean_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Illiberalism/Democratic Leaning on Degree of Anger (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q6: interest in opposition
plot_q6_by_illib_lean_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_illib_lean_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Illiberalism/Democratic Leaning on Interest in the Opposition (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_illib_lean_rand(1, hu_survey)
plot_q2_by_illib_lean_rand(2, hu_survey)
plot_q2_by_illib_lean_rand(3, hu_survey)
plot_q2_by_illib_lean_rand(4, hu_survey)
plot_q2_by_illib_lean_rand(5, hu_survey)
plot_q2_by_illib_lean_rand(6, hu_survey)
plot_q2_by_illib_lean_rand(7, hu_survey)

# Q3 calls
plot_q3_by_illib_lean_rand(1, hu_survey)
plot_q3_by_illib_lean_rand(2, hu_survey)
plot_q3_by_illib_lean_rand(3, hu_survey)
plot_q3_by_illib_lean_rand(4, hu_survey)
plot_q3_by_illib_lean_rand(5, hu_survey)
plot_q3_by_illib_lean_rand(6, hu_survey)
plot_q3_by_illib_lean_rand(7, hu_survey)

# Q6 calls
plot_q6_by_illib_lean_rand(1, hu_survey)
plot_q6_by_illib_lean_rand(2, hu_survey)
plot_q6_by_illib_lean_rand(3, hu_survey)
plot_q6_by_illib_lean_rand(4, hu_survey)
plot_q6_by_illib_lean_rand(5, hu_survey)
plot_q6_by_illib_lean_rand(6, hu_survey)
plot_q6_by_illib_lean_rand(7, hu_survey)

###### Old Authoritarianism Battery ##### 

# Plot by illeberalism/democratic leaning (Fidesz supporters only) # 
plot_by_old_aut_fidesz <- function(question_code, y_label, plot_title, plot_subtitle,
                                   id_col = "respondent_id") {
  
  has_id <- id_col %in% names(hu_survey_long)
  
  df <- hu_survey_long %>%
    filter(
      question == question_code,
      fidesz_or_not == 1,              # <-- restrict to Fidesz supporters
      new_aut_binary %in% c(0, 1)
    ) %>%
    mutate(
      old_aut_f = factor(new_aut_binary, levels = c(0, 1),
                         labels = c("Authoritarian Leaning", "Democratic Leaning"))
    )
  
  if (has_id) {
    summary_df <- df %>%
      group_by(old_aut_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(old_aut_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = sd / sqrt(n),
        ci90 = qt(0.95, df = n - 1) * se,
        ci95 = qt(0.975, df = n - 1) * se,
        .groups = "drop"
      )
  }
  
  summary_df <- summary_df %>%
    mutate(
      ci90 = ifelse(is.na(ci90) & n > 0, NA_real_, ci90),
      ci95 = ifelse(is.na(ci95) & n > 0, NA_real_, ci95)
    )
  
  p <- ggplot(summary_df, aes(x = old_aut_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Authoritarian/Democratic Leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic()
  
  print(summary_df)
  return(p)
}

# ---- Authoritarian/Democratic -based plots (Fidesz only) ----

p_q2_old_aut <- plot_by_old_aut_fidesz(
  question_code = "Q2",
  y_label = "Average confidence in the Fidesz government",
  plot_title = "Effect of Authoritarian/Democratic Leaning on Confidence (Fidesz only)",
  plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q2_old_aut)

p_q3_old_aut <- plot_by_old_aut_fidesz(
  question_code = "Q3",
  y_label = "Average level of anger",
  plot_title = "Effect of Authoritarian/Democratic Leaning on Degree of Anger (Fidesz only)",
  plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q3_old_aut)

p_q6_old_aut <- plot_by_old_aut_fidesz(
  question_code = "Q6",
  y_label = "Average interest in the Opposition",
  plot_title = "Effect of Authoritarian/Democratic Leaning on Interest in the Opposition (Fidesz only)",
  plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)"
)
print(p_q6_old_aut)

### FIDESZ ONLY comparing Authoritarian/Democratic Leaning, RAND1 - RAND7 ### 

# Helper to construct RAND combined varnames: RAND<r>_Q<q>_combined
rand_combined_var <- function(rand, qcode) {
  if (!(rand %in% 1:7)) stop("rand must be 1..7")
  paste0("RAND", rand, "_Q", qcode, "_combined")
}

# Core helper: compare new_aut_binary (0 vs 1) for one RAND and question
plot_by_old_aut_rand_combined <- function(data, rand, qcode,
                                          y_label, plot_title, plot_subtitle,
                                          id_col = "respondent_id") {
  
  varname <- rand_combined_var(rand, qcode)
  if (!varname %in% names(data)) stop(paste0("Missing variable: ", varname))
  
  # restrict to Fidesz supporters and new_aut_binary coded 0/1
  df <- data %>%
    filter(fidesz_or_not == 1, new_aut_binary %in% c(0, 1)) %>%
    mutate(
      response = as.numeric(.data[[varname]]),
      old_aut_f = factor(new_aut_binary, levels = c(0, 1),
                         labels = c("Authoritarian Leaning", "Democratic Leaning"))
    )
  
  has_id <- id_col %in% names(df)
  
  if (has_id) {
    summary_df <- df %>%
      group_by(old_aut_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = n_distinct(.data[[id_col]]),   # unique respondents
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  } else {
    summary_df <- df %>%
      group_by(old_aut_f) %>%
      summarise(
        mean_response = mean(response, na.rm = TRUE),
        n = sum(!is.na(response)),
        sd = sd(response, na.rm = TRUE),
        se = ifelse(n > 0, sd / sqrt(n), NA_real_),
        ci90 = ifelse(n > 1, qt(0.95, df = n - 1) * se, NA_real_),
        ci95 = ifelse(n > 1, qt(0.975, df = n - 1) * se, NA_real_),
        .groups = "drop"
      )
  }
  
  # ensure both factor levels appear
  all_levels <- factor(c("Authoritarian Leaning", "Democratic Leaning"),
                       levels = c("Authoritarian Leaning", "Democratic Leaning"))
  summary_df <- summary_df %>%
    complete(old_aut_f = all_levels,
             fill = list(mean_response = NA_real_, n = 0, sd = NA_real_, se = NA_real_, ci90 = NA_real_, ci95 = NA_real_)) %>%
    arrange(old_aut_f)
  
  # print summary for inspection
  print(summary_df)
  
  # plot (no n labels)
  p <- ggplot(summary_df, aes(x = old_aut_f, y = mean_response)) +
    geom_point(size = 3) +
    geom_text(
      aes(label = paste0("n=", n),
          y = mean_response + ci95),
      nudge_y = 0.04,
      size = 3.4,
      color = "gray30"
    )+
    geom_errorbar(aes(ymin = mean_response - ci95, ymax = mean_response + ci95),
                  width = 0.15, linewidth = 0.8, na.rm = TRUE) +
    geom_errorbar(aes(ymin = mean_response - ci90, ymax = mean_response + ci90),
                  width = 0.08, linewidth = 1.2, na.rm = TRUE) +
    labs(x = "Authoritarian/Democratic Leaning", y = y_label, title = plot_title, subtitle = plot_subtitle) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.12))  # gives ~12% headroom at top
    )+
    theme_classic()
  
  print(p)
  invisible(summary_df)
}

# ---------- Three wrappers: Q2, Q3, Q6 (each takes rand 1..7) ----------

# Q2: confidence in government
plot_q2_by_old_aut_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_old_aut_rand_combined(
    data = data, rand = rand, qcode = 2,
    y_label = "Average confidence in the Fidesz government",
    plot_title = paste0("Effect of Authoritarian/Democratic Leaning on Confidence (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = losing confidence completely; 1 = confidence remains unchanged\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q3: anger
plot_q3_by_old_aut_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_old_aut_rand_combined(
    data = data, rand = rand, qcode = 3,
    y_label = "Average level of anger",
    plot_title = paste0("Effect of Authoritarian/Democratic Leaning on Degree of Anger (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = completely indifferent; 1 = very angry\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# Q6: interest in opposition
plot_q6_by_old_aut_rand <- function(rand, data, id_col = "respondent_id") {
  plot_by_old_aut_rand_combined(
    data = data, rand = rand, qcode = 6,
    y_label = "Average interest in the Opposition",
    plot_title = paste0("Effect of Authoritarian/Democratic Leaning on Interest in the Opposition (RAND ", rand, "; Fidesz only)"),
    plot_subtitle = "0 = does not change my interest at all; 1 = increases my interest substantially\n(thick bars = 90% CI, thin bars = 95% CI)",
    id_col = id_col
  )
}

# ---------- Example calls (RAND = 1..7) ----------
# Q2 calls
plot_q2_by_old_aut_rand(1, hu_survey)
plot_q2_by_old_aut_rand(2, hu_survey)
plot_q2_by_old_aut_rand(3, hu_survey)
plot_q2_by_old_aut_rand(4, hu_survey)
plot_q2_by_old_aut_rand(5, hu_survey)
plot_q2_by_old_aut_rand(6, hu_survey)
plot_q2_by_old_aut_rand(7, hu_survey)

# Q3 calls
plot_q3_by_old_aut_rand(1, hu_survey)
plot_q3_by_old_aut_rand(2, hu_survey)
plot_q3_by_old_aut_rand(3, hu_survey)
plot_q3_by_old_aut_rand(4, hu_survey)
plot_q3_by_old_aut_rand(5, hu_survey)
plot_q3_by_old_aut_rand(6, hu_survey)
plot_q3_by_old_aut_rand(7, hu_survey)

# Q6 calls
plot_q6_by_old_aut_rand(1, hu_survey)
plot_q6_by_old_aut_rand(2, hu_survey)
plot_q6_by_old_aut_rand(3, hu_survey)
plot_q6_by_old_aut_rand(4, hu_survey)
plot_q6_by_old_aut_rand(5, hu_survey)
plot_q6_by_old_aut_rand(6, hu_survey)
plot_q6_by_old_aut_rand(7, hu_survey)
