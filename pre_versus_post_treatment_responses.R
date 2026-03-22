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
library(rlang)

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

##### pre-treatment versus post-treatment responses: confidence in Fidesz, all respondents #####

# function # 
compare_pre_post_tru <- function(data,
                                 post_col,
                                 pre_col = "TRU_1",
                                 pre_label = "Pre-treatment",
                                 post_label = "Post-treatment",
                                 colors = c("#1f77b4", "#ff7f0e"),
                                 percent_digits = 1) {
  # Accept either a string ("RAND1_Q2_combined") or an unquoted symbol (RAND1_Q2_combined)
  post_sym <- if (is.character(post_col)) sym(post_col) else ensym(post_col)
  pre_sym  <- sym(pre_col)
  
  # extract and coerce to character so we can unify labels
  df_long <- data %>%
    transmute(
      pre  = as.character( (!!pre_sym) ),
      post = as.character( (!!post_sym) )
    ) %>%
    # drop rows where both are NA
    filter(!(is.na(pre) & is.na(post)))
  
  # union levels and determine a sensible ordering
  all_responses <- na.omit(unique(c(df_long$pre, df_long$post)))
  # try coercing to numeric for correct numeric ordering if possible
  numeric_candidates <- suppressWarnings(as.numeric(all_responses))
  if (!all(is.na(numeric_candidates))) {
    # keep only entries that are valid numeric representations, preserve labels exactly by mapping
    # but order by numeric value
    mapping <- data.frame(label = all_responses, numeric = numeric_candidates, stringsAsFactors = FALSE)
    # if at least half of values coercible to numeric, treat as numeric ordering
    if (sum(!is.na(mapping$numeric)) / nrow(mapping) >= 0.5) {
      # order by numeric, use original string labels as factor levels
      response_levels <- mapping %>%
        arrange(numeric) %>%
        pull(label)
    } else {
      response_levels <- sort(all_responses)
    }
  } else {
    response_levels <- sort(all_responses)
  }
  
  # If there are known expected response options (e.g., "1","2","3","4","5"), ensure they are included.
  # (Optional) You could also force response_levels <- c("1","2","3","4","5") if you want fixed levels.
  
  # Build a complete summary so both groups have every level (0 if none)
  summary_df <- bind_rows(
    df_long %>%
      filter(!is.na(pre)) %>%
      count(response = factor(pre, levels = response_levels), name = "n") %>%
      mutate(group = pre_label),
    df_long %>%
      filter(!is.na(post)) %>%
      count(response = factor(post, levels = response_levels), name = "n") %>%
      mutate(group = post_label)
  ) %>%
    complete(group = c(pre_label, post_label),
             response = factor(response_levels, levels = response_levels),
             fill = list(n = 0)) %>%
    group_by(group) %>%
    mutate(total_in_group = sum(n),
           proportion = ifelse(total_in_group > 0, n / total_in_group, 0)) %>%
    ungroup() %>%
    mutate(group = factor(group, levels = c(pre_label, post_label)))
  
  # Plot
  p <- ggplot(summary_df, aes(x = response, y = proportion, fill = group)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    # percent labels
    geom_text(aes(label = ifelse(total_in_group==0, "", 
                                 paste0(formatC(100 * proportion, format = "f", digits = percent_digits), "%"))),
              position = position_dodge(width = 0.9),
              vjust = -0.25,
              size = 3) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                       expand = expansion(mult = c(0, 0.06))) +
    scale_fill_manual(values = colors, name = NULL) +
    labs(
      x = "Question Response",
      y = "proportion of respondents",
      title = paste0("Comparing Confidence in Fidesz Responses: ", pre_label, " vs. ", post_label),
      subtitle = paste0("Pre: ", pre_col, "  —  Post: ", as_label(post_sym))
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

#call function# 

p1 <- compare_pre_post_tru(hu_survey, "RAND1_Q2_combined", pre_label = "Before treatment", post_label = "After RAND1")
print(p1)

p2 <- compare_pre_post_tru(hu_survey, "RAND2_Q2_combined", pre_label = "Before treatment", post_label = "After RAND2")
print(p2)

p3 <- compare_pre_post_tru(hu_survey, "RAND3_Q2_combined", pre_label = "Before treatment", post_label = "After RAND3")
print(p3)

p4 <- compare_pre_post_tru(hu_survey, "RAND4_Q2_combined", pre_label = "Before treatment", post_label = "After RAND4")
print(p4)

p5 <- compare_pre_post_tru(hu_survey, "RAND5_Q2_combined", pre_label = "Before treatment", post_label = "After RAND5")
print(p5)

p6 <- compare_pre_post_tru(hu_survey, "RAND6_Q2_combined", pre_label = "Before treatment", post_label = "After RAND6")
print(p6)

p7 <- compare_pre_post_tru(hu_survey, "RAND7_Q2_combined", pre_label = "Before treatment", post_label = "After RAND7")
print(p7)
