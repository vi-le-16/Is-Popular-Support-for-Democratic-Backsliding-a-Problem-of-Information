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

hu_survey <- hu_survey %>% 
  mutate(age_group = 
           dplyr::case_when(
             DEM2 >= 50 ~ 1,  # older respondents
             DEM2 < 50 ~ 0.   # younger respondents
           )
  )

table(hu_survey$age_group) #0 == 991 #1 == 1009


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

######## December 2025 Tasks START ########

# --- Party labels ---
party_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd",
  "Mi Hazánk", "A Nép Pártján", "TISZA", "Kétfarkú Kutya",
  "Második Reformkor", "MMN", "Other", "Don't know"
)

# --- Create labeled party variable ---
hu_survey <- hu_survey %>%
  mutate(
    POL2 = as.numeric(POL2),
    POL2_labeled = factor(POL2, levels = c(1:14, 99), labels = party_labels)
  )

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

### visualizations ### 

# ILL1_1 - National elections are free, fair, and regularly held #

# --- Mark small parties at the respondent-row level, THEN summarise ---

hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_1_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_1_v2, na.rm = TRUE),
    sd = sd(ILL1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  )+ 
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'National Elections Are Free, Fair, and Regularly Held'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_2 ##

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_2_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_2_v2, na.rm = TRUE),
    sd = sd(ILL1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  )+
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'the Rule of Law is upheld'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_3 ##

# --- Party labels ---
party_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd",
  "Mi Hazánk", "A Nép Pártján", "TISZA", "Kétfarkú Kutya",
  "Második Reformkor", "MMN", "Other", "Don't know"
)

# --- Create labeled party variable ---
hu_survey <- hu_survey %>%
  mutate(
    POL2 = as.numeric(POL2),
    POL2_labeled = factor(POL2, levels = c(1:14, 99), labels = party_labels)
  )

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_3_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_3_v2, na.rm = TRUE),
    sd = sd(ILL1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  )+
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'checks and balances exist between different government branches'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_4 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_4_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_4_v2, na.rm = TRUE),
    sd = sd(ILL1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties have a fair chance to win'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_5 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_5_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_5_v2, na.rm = TRUE),
    sd = sd(ILL1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Governments are responsive to citizens'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_6 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_6_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_6_v2, na.rm = TRUE),
    sd = sd(ILL1_6_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'All political parties receive equal media time'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_7 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_7_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_7_v2, na.rm = TRUE),
    sd = sd(ILL1_7_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties can operate freely'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_8 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_8_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_8_v2, na.rm = TRUE),
    sd = sd(ILL1_8_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Human and civil rights groups can operate freely'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_9 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_9_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_9_v2, na.rm = TRUE),
    sd = sd(ILL1_9_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'The media is free from political pressure'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_10 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_10_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_10_v2, na.rm = TRUE),
    sd = sd(ILL1_10_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Governments tax the rich and subsidize the poor'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_11 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_11_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_11_v2, na.rm = TRUE),
    sd = sd(ILL1_11_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'People receive state aid for unemployment'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_12 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_12_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_12_v2, na.rm = TRUE),
    sd = sd(ILL1_12_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Civil rights protect people from state oppression'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_13 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_13_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_13_v2, na.rm = TRUE),
    sd = sd(ILL1_13_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'The state ensures equal income for all'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## ILL1_14 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_14_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_14_v2, na.rm = TRUE),
    sd = sd(ILL1_14_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Women have the same rights as men'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL1_15 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL1_15_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL1_15_v2, na.rm = TRUE),
    sd = sd(ILL1_15_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: How essential for democracy is it that 'Minority rights are enshrined in law and protected'\n0 - not at all an essential characteristic of democracy; 1 - an essential characteristic of democracy",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL2_1 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL2_1_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL2_1_v2, na.rm = TRUE),
    sd = sd(ILL2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Hungarians should be tolerant of those who lead unconventional lives'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL2_2 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL2_2_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL2_2_v2, na.rm = TRUE),
    sd = sd(ILL2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: Do you agree that 'A good society is one where all kinds of different opinions 
    and ways of life can flourish'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL2_3 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL2_3_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL2_3_v2, na.rm = TRUE),
    sd = sd(ILL2_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Judges should be selected based on cross-party consensus'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL2_4 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL2_4_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL2_4_v2, na.rm = TRUE),
    sd = sd(ILL2_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: Do you agree that 'The role of public media is to report independently on political developments'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## ILL2_5 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(ILL2_5_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ILL2_5_v2, na.rm = TRUE),
    sd = sd(ILL2_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Illiberalism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Democracy is preferable to other political systems, 
    even if it sometimes means I disagree with the government’s decisions'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

### average of all the questions in the battery ### 

# --- party labels (your list) ---
party_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd",
  "Mi Hazánk", "A Nép Pártján", "TISZA", "Kétfarkú Kutya",
  "Második Reformkor", "MMN", "Other", "Don't know"
)

# --- ensure party factor exists ---
hu_survey <- hu_survey %>%
  mutate(
    POL2 = as.numeric(POL2),
    POL2_labeled = factor(POL2, levels = c(1:14, 99), labels = party_labels)
  )

# --- list only the *_v2 variable names (you said they exist) ---
ill1_v2_names <- paste0("ILL1_", 1:15, "_v2")
ill2_v2_names <- paste0("ILL2_", 1:5, "_v2")
all_item_names <- c(ill1_v2_names, ill2_v2_names)

# sanity-check: optionally uncomment to see which vars are present
# intersect(all_item_names, colnames(hu_survey))

# --- respondent-level average across the listed *_v2 items ---
# compute items_used and respondent average (will be NA if no items answered)
hu_survey_avg <- hu_survey %>%
  filter(!is.na(POL2_labeled)) %>%
  rowwise() %>%
  mutate(
    items_used = sum(!is.na(c_across(all_of(all_item_names)))),
    ill_all_avg = if_else(items_used > 0,
                          mean(c_across(all_of(all_item_names)), na.rm = TRUE),
                          NA_real_)
  ) %>%
  ungroup()

# --- Mark small parties at respondent level, THEN summarise ---
hu_survey2 <- hu_survey_avg %>%
  filter(!is.na(ill_all_avg)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(ill_all_avg, na.rm = TRUE),
    sd = sd(ill_all_avg, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- prepare palette that scales to number of groups ---
n_groups <- length(unique(summary_grouped$POL2_grouped))
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_groups)

# --- Plot with 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Average of All Responses to New Illiberal Democracy Battery by Party",
    subtitle = "Questions Included: ILL1_1–ILL1_15 & ILL2_1–ILL2_5",
    x = "Political Party",
    y = "Mean Response (0–1)",
    caption = "Notes: Error bars: 95% (thicker) and 90% (thinner). Parties with <40 respondents grouped as 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)

##### new illiberalism but fidesz only ### 

### binary more/less media embedded comparison ### 

## ILL1_1 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_1_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_1_v2, na.rm = TRUE),
    sd = sd(ILL1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'National Elections Are Free, Fair, and Regularly Held'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_2 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_2_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_2_v2, na.rm = TRUE),
    sd = sd(ILL1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'The rule of law is upheld, and corrupt officials are prosecuted for their crimes'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_3 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_3_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_3_v2, na.rm = TRUE),
    sd = sd(ILL1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Robust checks and balances exist between different branches of government'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_4 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_4_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_4_v2, na.rm = TRUE),
    sd = sd(ILL1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties have a fair chance to win the election'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_5 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_5_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_5_v2, na.rm = TRUE),
    sd = sd(ILL1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Governments are responsive to their citizens’ needs'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_6 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_6_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_6_v2, na.rm = TRUE),
    sd = sd(ILL1_6_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'All political parties receive equal media time 
    to expose voters to their policies and views’ needs'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_7 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_7_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_7_v2, na.rm = TRUE),
    sd = sd(ILL1_7_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties can operate freely'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_8 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_8_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_8_v2, na.rm = TRUE),
    sd = sd(ILL1_8_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Human and civil rights groups can operate freely'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_9 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_9_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_9_v2, na.rm = TRUE),
    sd = sd(ILL1_9_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'The media is free from political pressure'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_10 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_10_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_10_v2, na.rm = TRUE),
    sd = sd(ILL1_10_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Governments tax the rich and subsidize the poor'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_11 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_11_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_11_v2, na.rm = TRUE),
    sd = sd(ILL1_11_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'People receive state aid for unemployment'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_12 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_12_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_12_v2, na.rm = TRUE),
    sd = sd(ILL1_12_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Civil rights protect people from state oppression'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_13 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_13_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_13_v2, na.rm = TRUE),
    sd = sd(ILL1_13_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'The state ensures equal income for all'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_14 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_14_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_14_v2, na.rm = TRUE),
    sd = sd(ILL1_14_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Women have the same rights as men'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_15 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL1_15_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL1_15_v2, na.rm = TRUE),
    sd = sd(ILL1_15_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: How essential for democracy is it that 'Minority rights are enshrined in law and protected'\n0 - not at all; 1 - essential",
    x = "Media Embeddedness Score level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_1 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL2_1_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL2_1_v2, na.rm = TRUE),
    sd = sd(ILL2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: To what extent do you agree with 'Hungarians should be tolerant of those who lead unconventional lives'\n0 - disagree strongly; 1 - agree strongly",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_2 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL2_2_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL2_2_v2, na.rm = TRUE),
    sd = sd(ILL2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: To what extent do you agree with 'A good society is one where all kinds of different opinions and 
    ways of life can flourish'\n0 - disagree strongly; 1 - agree strongly",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_3 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL2_3_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL2_3_v2, na.rm = TRUE),
    sd = sd(ILL2_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: To what extent do you agree with 'Judges should be selected based on cross-party consensus'\n0 - disagree strongly; 1 - agree strongly",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_4 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL2_4_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL2_4_v2, na.rm = TRUE),
    sd = sd(ILL2_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: To what extent do you agree with 'The role of public media is to report independently on political developments'\n0 - disagree strongly; 1 - agree strongly",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_5 ##

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome and create a labelled Media Embeddedness Score factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(ILL2_5_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(ILL2_5_v2, na.rm = TRUE),
    sd = sd(ILL2_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Media Embeddedness Score",
    subtitle = "Q: To what extent do you agree with 'Democracy is preferable to other political systems, 
    even if it sometimes means I disagree with the government’s decisions'\n0 - disagree strongly; 1 - agree strongly",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

### binary more/less educated comparison ### 

## ILL1_1 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_1_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_1_v2, na.rm = TRUE),
    sd = sd(ILL1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'National Elections Are Free, Fair, and Regularly Held'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_2 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_2_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_2_v2, na.rm = TRUE),
    sd = sd(ILL1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'The rule of law is upheld, and corrupt officials are prosecuted for their crimes'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_3 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_3_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_3_v2, na.rm = TRUE),
    sd = sd(ILL1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Robust checks and balances exist between different branches of government'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_4 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_4_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_4_v2, na.rm = TRUE),
    sd = sd(ILL1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties have a fair chance to win the election'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_5 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_5_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_5_v2, na.rm = TRUE),
    sd = sd(ILL1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Governments are responsive to their citizens’ needs'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_6 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_6_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_6_v2, na.rm = TRUE),
    sd = sd(ILL1_6_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'All political parties receive equal media time 
    to expose voters to their policies and views’ needs'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_7 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_7_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_7_v2, na.rm = TRUE),
    sd = sd(ILL1_7_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties can operate freely'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_8 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_8_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_8_v2, na.rm = TRUE),
    sd = sd(ILL1_8_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Human and civil rights groups can operate freely'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_9 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_9_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_9_v2, na.rm = TRUE),
    sd = sd(ILL1_9_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'The media is free from political pressure'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_10 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_10_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_10_v2, na.rm = TRUE),
    sd = sd(ILL1_10_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Governments tax the rich and subsidize the poor'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_11 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_11_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_11_v2, na.rm = TRUE),
    sd = sd(ILL1_11_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'People receive state aid for unemployment'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_12 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_12_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_12_v2, na.rm = TRUE),
    sd = sd(ILL1_12_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Civil rights protect people from state oppression'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_13 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_13_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_13_v2, na.rm = TRUE),
    sd = sd(ILL1_13_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'The state ensures equal income for all'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_14 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_14_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_14_v2, na.rm = TRUE),
    sd = sd(ILL1_14_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Women have the same rights as men'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_15 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL1_15_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL1_15_v2, na.rm = TRUE),
    sd = sd(ILL1_15_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: How essential for democracy is it that 'Minority rights are enshrined in law and protected'\n0 - not at all; 1 - essential",
    x = "Education level",
    y = "Mean Response (0.1–1.0)",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_1 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL2_1_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL2_1_v2, na.rm = TRUE),
    sd = sd(ILL2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: To what extent do you agree with 'Hungarians should be tolerant of those who lead unconventional lives'\n0 - disagree strongly; 1 - agree strongly",
    x = "Education level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_2 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL2_2_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL2_2_v2, na.rm = TRUE),
    sd = sd(ILL2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: To what extent do you agree with 'A good society is one where all kinds of different opinions and 
    ways of life can flourish'\n0 - disagree strongly; 1 - agree strongly",
    x = "Education level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_3 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL2_3_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL2_3_v2, na.rm = TRUE),
    sd = sd(ILL2_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: To what extent do you agree with 'Judges should be selected based on cross-party consensus'\n0 - disagree strongly; 1 - agree strongly",
    x = "Education level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_4 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL2_4_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL2_4_v2, na.rm = TRUE),
    sd = sd(ILL2_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: To what extent do you agree with 'The role of public media is to report independently on political developments'\n0 - disagree strongly; 1 - agree strongly",
    x = "Education level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_5 ##

# --- Filter to Fidesz supporters and non-missing education / outcome and create a labelled education factor for plot x-axis ---

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(ILL2_5_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(ILL2_5_v2, na.rm = TRUE),
    sd = sd(ILL2_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Education",
    subtitle = "Q: To what extent do you agree with 'Democracy is preferable to other political systems, 
    even if it sometimes means I disagree with the government’s decisions'\n0 - disagree strongly; 1 - agree strongly",
    x = "Education level",
    y = "Mean Response",
    caption = "Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

### binary age comparison - ### 

## ILL1_1 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_1_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_1_v2, na.rm = TRUE),
    sd = sd(ILL1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'National Elections Are Free, Fair, and Regularly Held'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_2 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_2_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_2_v2, na.rm = TRUE),
    sd = sd(ILL1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'The rule of law is upheld, and corrupt officials are prosecuted for their crimes'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_3 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_3_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_3_v2, na.rm = TRUE),
    sd = sd(ILL1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Robust checks and balances exist between different branches of government'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_4 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_4_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_4_v2, na.rm = TRUE),
    sd = sd(ILL1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties have a fair chance to win the election'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_5 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_5_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_5_v2, na.rm = TRUE),
    sd = sd(ILL1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Governments are responsive to their citizens’ needs'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_6 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_6_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_6_v2, na.rm = TRUE),
    sd = sd(ILL1_6_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'All political parties receive equal media time 
    to expose voters to their policies and views’ needs'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_7 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_7_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_7_v2, na.rm = TRUE),
    sd = sd(ILL1_7_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Opposition parties can operate freely'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_8 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_8_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_8_v2, na.rm = TRUE),
    sd = sd(ILL1_8_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Human and civil rights groups can operate freely'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_9 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_9_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_9_v2, na.rm = TRUE),
    sd = sd(ILL1_9_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'The media is free from political pressure'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_10 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_10_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_10_v2, na.rm = TRUE),
    sd = sd(ILL1_10_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Governments tax the rich and subsidize the poor'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_11 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_11_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_11_v2, na.rm = TRUE),
    sd = sd(ILL1_11_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'People receive state aid for unemployment'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_12 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_12_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_12_v2, na.rm = TRUE),
    sd = sd(ILL1_12_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Civil rights protect people from state oppression'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_13 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_13_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_13_v2, na.rm = TRUE),
    sd = sd(ILL1_13_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'The state ensures equal income for all'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_14 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_14_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_14_v2, na.rm = TRUE),
    sd = sd(ILL1_14_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Women have the same rights as men'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL1_15 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL1_15_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL1_15_v2, na.rm = TRUE),
    sd = sd(ILL1_15_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: How essential for democracy is it that 'Minority rights are enshrined in law and protected'\n0 - not at all; 1 - essential",
    x = "Age Group",
    y = "Mean Response (0.1–1.0)",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_1 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL2_1_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL2_1_v2, na.rm = TRUE),
    sd = sd(ILL2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: To what extent do you agree with 'Hungarians should be tolerant of those who lead unconventional lives'\n0 - disagree strongly; 1 - agree strongly",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_2 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL2_2_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL2_2_v2, na.rm = TRUE),
    sd = sd(ILL2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: To what extent do you agree with 'A good society is one where all kinds of different opinions and 
    ways of life can flourish'\n0 - disagree strongly; 1 - agree strongly",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_3 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL2_3_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL2_3_v2, na.rm = TRUE),
    sd = sd(ILL2_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: To what extent do you agree with 'Judges should be selected based on cross-party consensus'\n0 - disagree strongly; 1 - agree strongly",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)


## ILL2_4 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL2_4_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL2_4_v2, na.rm = TRUE),
    sd = sd(ILL2_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: To what extent do you agree with 'The role of public media is to report independently on political developments'\n0 - disagree strongly; 1 - agree strongly",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## ILL2_5 ##

hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(age_group), !is.na(ILL2_5_v2)) %>%
  mutate(
    age_f = factor(age_group, levels = c(0, 1),
                   labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(age_f) %>%
  summarise(
    mean_score = mean(ILL2_5_v2, na.rm = TRUE),
    sd = sd(ILL2_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Choose a palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot ---
ggplot(summary_edu, aes(x = age_f, y = mean_score, color = age_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response of Fidesz Supporters to Illiberalism by Age Group",
    subtitle = "Q: To what extent do you agree with 'Democracy is preferable to other political systems, 
    even if it sometimes means I disagree with the government’s decisions'\n0 - disagree strongly; 1 - agree strongly",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Error bars: 95% (thicker) and 90% (thinner)."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

##### old fashioned authoritarianism battery #####

### recalibrate question scales ### 

hu_survey <- hu_survey %>% 
  mutate(AUT1_1_v2 = #Hungary needs loyalty towards its leaders 
           dplyr::case_when(
             AUT1_1 > 4 ~ 0.2,  #disagree strongly
             AUT1_1 > 3 ~ 0.4,
             AUT1_1 > 2 ~ 0.6,
             AUT1_1 > 1 ~ 0.8, 
             AUT1_1 > 0 ~ 1.0   #agree strongly  
           ))

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

### visualizations ### 

## AUT1_1 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT1_1_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT1_1_v2, na.rm = TRUE),
    sd = sd(AUT1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you agree that 'What Hungary needs most is loyalty towards its leaders'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT1_2 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT1_2_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT1_2_v2, na.rm = TRUE),
    sd = sd(AUT1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Government control of the media and education is necessary to uphold moral standards'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))


## AUT1_3 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT1_3_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT1_3_v2, na.rm = TRUE),
    sd = sd(AUT1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Protests and demonstrations against the government should be allowed'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT1_4 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT1_4_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT1_4_v2, na.rm = TRUE),
    sd = sd(AUT1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Judges should be selected by the ruling party'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT1_5 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT1_5_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT1_5_v2, na.rm = TRUE),
    sd = sd(AUT1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you agree that 'Public media should defend government against criticism'\n0 - strongly disagree; 1 - strongly agree",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT2_1 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT2_1_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT2_1_v2, na.rm = TRUE),
    sd = sd(AUT2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you think the following is good or bad: 'Having a strong leader who does not have to bother 
    with parliament and elections'\n0 - very good; 1 - very bad",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT2_2 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT2_2_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT2_2_v2, na.rm = TRUE),
    sd = sd(AUT2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: Do you think the following is good or bad: 'Having a political system rooted in Christian morality'\n0 - very good; 1 - very bad",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT3 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT3_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT3_v2, na.rm = TRUE),
    sd = sd(AUT3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: What is your opinion of Hungarians who believe they had a better life under the Kádár regime?\n0 - very positive; 1 - very negative",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

## AUT4 ## 

# --- Mark small parties at the respondent-row level, THEN summarise ---
hu_survey2 <- hu_survey %>%
  filter(!is.na(POL2_labeled), !is.na(AUT4_v2)) %>%
  group_by(POL2_labeled) %>%
  mutate(n_in_party = n()) %>%   # count per original party
  ungroup() %>%
  mutate(
    POL2_grouped = if_else(n_in_party < 40, "Other", as.character(POL2_labeled)),
    POL2_grouped = factor(POL2_grouped)  # keep as factor
  )

summary_grouped <- hu_survey2 %>%
  group_by(POL2_grouped) %>%
  summarise(
    mean_score = mean(AUT4_v2, na.rm = TRUE),
    sd = sd(AUT4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  # clamp CI endpoints inside [0,1] to avoid plotting beyond axis limits
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Plot with 90% (thin) and 95% (thicker) CIs ---
ggplot(summary_grouped, aes(x = reorder(POL2_grouped, mean_score),
                            y = mean_score,
                            color = POL2_grouped)) +
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker bar)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (slightly thinner bar, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Mean Response to Old-Fashioned Authoritarianism By Party Affiliation",
    subtitle = "Q: What is your opinion of the Singaporean one-party political system?\n0 - very positive; 1 - very negative",
    x = "Political Party",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals. Parties with fewer than 40 respondents grouped into 'Other'."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(
    n = length(unique(summary_grouped$POL2_grouped)), "Dark2"
  ))

##### old fashioned battery but fidesz only ##### 

### split by media embeddedness ### 

## AUT1_1 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_1_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT1_1_v2, na.rm = TRUE),
    sd = sd(AUT1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you agree that 'What Hungary needs most is loyalty towards its leaders'\n0 - strongly disagree; 1 - strongly agree",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_2 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_2_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT1_2_v2, na.rm = TRUE),
    sd = sd(AUT1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you agree that 'Government control of the media and education is necessary to uphold moral standards'\n0 - strongly disagree; 1 - strongly agree",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_3 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_3_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT1_3_v2, na.rm = TRUE),
    sd = sd(AUT1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you agree that 'Protests and demonstrations against the government should be allowed'\n0 - strongly disagree; 1 - strongly agree",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_4 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_4_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT1_4_v2, na.rm = TRUE),
    sd = sd(AUT1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you agree that 'Judges should be selected by the ruling party'\n0 - strongly disagree; 1 - strongly agree",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_5 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_5_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT1_5_v2, na.rm = TRUE),
    sd = sd(AUT1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you agree that 'Public media should defend government against criticism'\n0 - strongly disagree; 1 - strongly agree",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_1 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT2_1_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT2_1_v2, na.rm = TRUE),
    sd = sd(AUT2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you think the following is good or bad 'Having a strong leader who does not have to bother with parliament and elections'\n0 - very good; 1 - very bad",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_2 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT2_2_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT2_2_v2, na.rm = TRUE),
    sd = sd(AUT2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: Do you think the following is good or bad 'Having a political system rooted in Christian morality'\n0 - very good; 1 - very bad",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT3 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT3_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT3_v2, na.rm = TRUE),
    sd = sd(AUT3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: What is your opinion of Hungarians who believe they had a better life under the Kádár regime?\n0 - very positive; 1 - very negative",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT4 ## 

# --- Filter to Fidesz supporters and non-missing Media Embeddedness Score / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT4_v2)) %>%
  mutate(
    Media_Embeddedness_Score_f = factor(comp_high, levels = c(0, 1),
                                        labels = c("Low Media Embeddedness Score", "High Media Embeddedness Score"))
  )

# --- Summarise by Media Embeddedness Score group ---
summary_edu <- hu_fidesz %>%
  group_by(Media_Embeddedness_Score_f) %>%
  summarise(
    mean_score = mean(AUT4_v2, na.rm = TRUE),
    sd = sd(AUT4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = Media_Embeddedness_Score_f, y = mean_score, color = Media_Embeddedness_Score_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Media Embeddedness Score",
    subtitle = "Q: What is your opinion of the Singaporean one-party political system?\n0 - very positive; 1 - very negative",
    x = "Media Embeddedness Score level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

### split by education level ### 

## AUT1_1 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT1_1_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT1_1_v2, na.rm = TRUE),
    sd = sd(AUT1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you agree that 'What Hungary needs most is loyalty towards its leaders'\n0 - strongly disagree; 1 - strongly agree",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_2 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT1_2_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT1_2_v2, na.rm = TRUE),
    sd = sd(AUT1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you agree that 'Government control of the media and education is necessary to uphold moral standards'\n0 - strongly disagree; 1 - strongly agree",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_3 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT1_3_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT1_3_v2, na.rm = TRUE),
    sd = sd(AUT1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you agree that 'Protests and demonstrations against the government should be allowed'\n0 - strongly disagree; 1 - strongly agree",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_4 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT1_4_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT1_4_v2, na.rm = TRUE),
    sd = sd(AUT1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you agree that 'Judges should be selected by the ruling party'\n0 - strongly disagree; 1 - strongly agree",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_5 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT1_5_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT1_5_v2, na.rm = TRUE),
    sd = sd(AUT1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you agree that 'Public media should defend government against criticism'\n0 - strongly disagree; 1 - strongly agree",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_1 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT2_1_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT2_1_v2, na.rm = TRUE),
    sd = sd(AUT2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you think the following is good or bad 'Having a strong leader who does not have to bother with parliament and elections'\n0 - very good; 1 - very bad",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_2 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT2_2_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT2_2_v2, na.rm = TRUE),
    sd = sd(AUT2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: Do you think the following is good or bad 'Having a political system rooted in Christian morality'\n0 - very good; 1 - very bad",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT3 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT3_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT3_v2, na.rm = TRUE),
    sd = sd(AUT3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: What is your opinion of Hungarians who believe they had a better life under the Kádár regime?\n0 - very positive; 1 - very negative",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT4 ## 

# --- Filter to Fidesz supporters and non-missing education / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(education_level), !is.na(AUT4_v2)) %>%
  mutate(
    education_f = factor(education_level, levels = c(0, 1),
                         labels = c("Low education", "High education"))
  )

# --- Summarise by education group ---
summary_edu <- hu_fidesz %>%
  group_by(education_f) %>%
  summarise(
    mean_score = mean(AUT4_v2, na.rm = TRUE),
    sd = sd(AUT4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = education_f, y = mean_score, color = education_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Education",
    subtitle = "Q: What is your opinion of the Singaporean one-party political system?\n0 - very positive; 1 - very negative",
    x = "Education level",
    y = "Mean Response",
    caption = "Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

### split by Age Group ### 

## AUT1_1 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_1_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT1_1_v2, na.rm = TRUE),
    sd = sd(AUT1_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you agree that 'What Hungary needs most is loyalty towards its leaders'\n0 - strongly disagree; 1 - strongly agree",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_2 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_2_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT1_2_v2, na.rm = TRUE),
    sd = sd(AUT1_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you agree that 'Government control of the media and education is necessary to uphold moral standards'\n0 - strongly disagree; 1 - strongly agree",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_3 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_3_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT1_3_v2, na.rm = TRUE),
    sd = sd(AUT1_3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you agree that 'Protests and demonstrations against the government should be allowed'\n0 - strongly disagree; 1 - strongly agree",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_4 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_4_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT1_4_v2, na.rm = TRUE),
    sd = sd(AUT1_4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you agree that 'Judges should be selected by the ruling party'\n0 - strongly disagree; 1 - strongly agree",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT1_5 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT1_5_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT1_5_v2, na.rm = TRUE),
    sd = sd(AUT1_5_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you agree that 'Public media should defend government against criticism'\n0 - strongly disagree; 1 - strongly agree",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_1 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT2_1_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT2_1_v2, na.rm = TRUE),
    sd = sd(AUT2_1_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you think the following is good or bad 'Having a strong leader who does not have to bother with parliament and elections'\n0 - very good; 1 - very bad",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT2_2 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT2_2_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT2_2_v2, na.rm = TRUE),
    sd = sd(AUT2_2_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: Do you think the following is good or bad 'Having a political system rooted in Christian morality'\n0 - very good; 1 - very bad",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT3 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT3_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT3_v2, na.rm = TRUE),
    sd = sd(AUT3_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: What is your opinion of Hungarians who believe they had a better life under the Kádár regime?\n0 - very positive; 1 - very negative",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

## AUT4 ## 

# --- Filter to Fidesz supporters and non-missing Age Group / outcome ---
hu_fidesz <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz",
         !is.na(comp_high), !is.na(AUT4_v2)) %>%
  mutate(
    age_group_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Younger Respondents", "Older Respondents"))
  )

# --- Summarise by Age Group group ---
summary_edu <- hu_fidesz %>%
  group_by(age_group_f) %>%
  summarise(
    mean_score = mean(AUT4_v2, na.rm = TRUE),
    sd = sd(AUT4_v2, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_score - ci95),
    ymax95 = pmin(1, mean_score + ci95),
    ymin90 = pmax(0, mean_score - ci90),
    ymax90 = pmin(1, mean_score + ci90)
  )

# --- Palette for two groups ---
palette_vals <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]

# --- Plot: 95% (thicker) and 90% (thinner) CIs ---
ggplot(summary_edu, aes(x = age_group_f, y = mean_score, color = age_group_f)) +
  geom_point(size = 4) +
  
  geom_text(
    aes(
      label = paste0("n=", n),
      y = ymax95
    ),
    nudge_y = 0.035,   # push above CI
    size = 3.4,
    color = "gray30",
    show.legend = FALSE
  ) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # (optional) show sample sizes above the points — uncomment to enable
  # geom_text(aes(label = paste0("n=", n)), vjust = -1.6, size = 3, show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Fidesz Supporters: Mean 'Old-Fashioned Authoritarianism' by Age Group",
    subtitle = "Q: What is your opinion of the Singaporean one-party political system?\n0 - very positive; 1 - very negative",
    x = "Age Group",
    y = "Mean Response",
    caption = "Older Respondents = >50 years old; Younger Respondents = <50 years old
    Notes: Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  scale_color_manual(values = palette_vals)

