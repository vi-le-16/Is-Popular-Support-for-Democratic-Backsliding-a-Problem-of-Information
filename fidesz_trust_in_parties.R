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

#education binary variable#

hu_survey <- hu_survey %>% 
  mutate(education_level = 
           dplyr::case_when(
             DEM4 >= 3 ~ 1,  # high education
             DEM4 < 3 ~ 0.   # low education 
           )
  )

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

## recalibrate question responses ## 

hu_survey <- hu_survey %>% 
  mutate(TRU3_1_v2 = #Fidesz
           dplyr::case_when(
             TRU3_1 > 9 ~ 1, #very high trust 
             TRU3_1 > 8 ~ 0.9,
             TRU3_1 > 7 ~ 0.8,
             TRU3_1 > 6 ~ 0.7, 
             TRU3_1 > 5 ~ 0.6,
             TRU3_1 > 4 ~ 0.5,
             TRU3_1 > 3 ~ 0.4,
             TRU3_1 > 2 ~ 0.3,
             TRU3_1 > 1 ~ 0.2, 
             TRU3_1 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_2_v2 = #Jobbik
           dplyr::case_when(
             TRU3_2 > 9 ~ 1, #very high trust 
             TRU3_2 > 8 ~ 0.9,
             TRU3_2 > 7 ~ 0.8,
             TRU3_2 > 6 ~ 0.7, 
             TRU3_2 > 5 ~ 0.6,
             TRU3_2 > 4 ~ 0.5,
             TRU3_2 > 3 ~ 0.4,
             TRU3_2 > 2 ~ 0.3,
             TRU3_2 > 1 ~ 0.2, 
             TRU3_2 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_3_v2 = #MSZP
           dplyr::case_when(
             TRU3_3 > 9 ~ 1, #very high trust 
             TRU3_3 > 8 ~ 0.9,
             TRU3_3 > 7 ~ 0.8,
             TRU3_3 > 6 ~ 0.7, 
             TRU3_3 > 5 ~ 0.6,
             TRU3_3 > 4 ~ 0.5,
             TRU3_3 > 3 ~ 0.4,
             TRU3_3 > 2 ~ 0.3,
             TRU3_3 > 1 ~ 0.2, 
             TRU3_3 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_4_v2 = #DK
           dplyr::case_when(
             TRU3_4 > 9 ~ 1, #very high trust 
             TRU3_4 > 8 ~ 0.9,
             TRU3_4 > 7 ~ 0.8,
             TRU3_4 > 6 ~ 0.7, 
             TRU3_4 > 5 ~ 0.6,
             TRU3_4 > 4 ~ 0.5,
             TRU3_4 > 3 ~ 0.4,
             TRU3_4 > 2 ~ 0.3,
             TRU3_4 > 1 ~ 0.2, 
             TRU3_4 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_5_v2 = #LMP
           dplyr::case_when(
             TRU3_5 > 9 ~ 1, #very high trust 
             TRU3_5 > 8 ~ 0.9,
             TRU3_5 > 7 ~ 0.8,
             TRU3_5 > 6 ~ 0.7, 
             TRU3_5 > 5 ~ 0.6,
             TRU3_5 > 4 ~ 0.5,
             TRU3_5 > 3 ~ 0.4,
             TRU3_5 > 2 ~ 0.3,
             TRU3_5 > 1 ~ 0.2, 
             TRU3_5 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_6_v2 = #Momentum
           dplyr::case_when(
             TRU3_6 > 9 ~ 1, #very high trust 
             TRU3_6 > 8 ~ 0.9,
             TRU3_6 > 7 ~ 0.8,
             TRU3_6 > 6 ~ 0.7, 
             TRU3_6 > 5 ~ 0.6,
             TRU3_6 > 4 ~ 0.5,
             TRU3_6 > 3 ~ 0.4,
             TRU3_6 > 2 ~ 0.3,
             TRU3_6 > 1 ~ 0.2, 
             TRU3_6 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_7_v2 = #Mi Hazank
           dplyr::case_when(
             TRU3_7 > 9 ~ 1, #very high trust 
             TRU3_7 > 8 ~ 0.9,
             TRU3_7 > 7 ~ 0.8,
             TRU3_7 > 6 ~ 0.7, 
             TRU3_7 > 5 ~ 0.6,
             TRU3_7 > 4 ~ 0.5,
             TRU3_7 > 3 ~ 0.4,
             TRU3_7 > 2 ~ 0.3,
             TRU3_7 > 1 ~ 0.2, 
             TRU3_7 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_8_v2 = #TISZA
           dplyr::case_when(
             TRU3_8 > 9 ~ 1, #very high trust 
             TRU3_8 > 8 ~ 0.9,
             TRU3_8 > 7 ~ 0.8,
             TRU3_8 > 6 ~ 0.7, 
             TRU3_8 > 5 ~ 0.6,
             TRU3_8 > 4 ~ 0.5,
             TRU3_8 > 3 ~ 0.4,
             TRU3_8 > 2 ~ 0.3,
             TRU3_8 > 1 ~ 0.2, 
             TRU3_8 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU3_9_v2 = #Magyar
           dplyr::case_when(
             TRU3_9 > 9 ~ 1, #very high trust 
             TRU3_9 > 8 ~ 0.9,
             TRU3_9 > 7 ~ 0.8,
             TRU3_9 > 6 ~ 0.7, 
             TRU3_9 > 5 ~ 0.6,
             TRU3_9 > 4 ~ 0.5,
             TRU3_9 > 3 ~ 0.4,
             TRU3_9 > 2 ~ 0.3,
             TRU3_9 > 1 ~ 0.2, 
             TRU3_9 > 0 ~ 0.1 #no trust at all
           ))

### recalibrate question responses ### 

hu_survey <- hu_survey %>% 
  mutate(TRU4_1_v2 = #Orban
           dplyr::case_when(
             TRU4_1 > 9 ~ 1, #very high trust 
             TRU4_1 > 8 ~ 0.9,
             TRU4_1 > 7 ~ 0.8,
             TRU4_1 > 6 ~ 0.7, 
             TRU4_1 > 5 ~ 0.6,
             TRU4_1 > 4 ~ 0.5,
             TRU4_1 > 3 ~ 0.4,
             TRU4_1 > 2 ~ 0.3,
             TRU4_1 > 1 ~ 0.2, 
             TRU4_1 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_2_v2 = 
           dplyr::case_when(
             TRU4_2 > 9 ~ 1, #very high trust 
             TRU4_2 > 8 ~ 0.9,
             TRU4_2 > 7 ~ 0.8,
             TRU4_2 > 6 ~ 0.7, 
             TRU4_2 > 5 ~ 0.6,
             TRU4_2 > 4 ~ 0.5,
             TRU4_2 > 3 ~ 0.4,
             TRU4_2 > 2 ~ 0.3,
             TRU4_2 > 1 ~ 0.2, 
             TRU4_2 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_3_v2 = 
           dplyr::case_when(
             TRU4_3 > 9 ~ 1, #very high trust 
             TRU4_3 > 8 ~ 0.9,
             TRU4_3 > 7 ~ 0.8,
             TRU4_3 > 6 ~ 0.7, 
             TRU4_3 > 5 ~ 0.6,
             TRU4_3 > 4 ~ 0.5,
             TRU4_3 > 3 ~ 0.4,
             TRU4_3 > 2 ~ 0.3,
             TRU4_3 > 1 ~ 0.2, 
             TRU4_3 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_4_v2 =
           dplyr::case_when(
             TRU4_4 > 9 ~ 1, #very high trust 
             TRU4_4 > 8 ~ 0.9,
             TRU4_4 > 7 ~ 0.8,
             TRU4_4 > 6 ~ 0.7, 
             TRU4_4 > 5 ~ 0.6,
             TRU4_4 > 4 ~ 0.5,
             TRU4_4 > 3 ~ 0.4,
             TRU4_4 > 2 ~ 0.3,
             TRU4_4 > 1 ~ 0.2, 
             TRU4_4 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_5_v2 = 
           dplyr::case_when(
             TRU4_5 > 9 ~ 1, #very high trust 
             TRU4_5 > 8 ~ 0.9,
             TRU4_5 > 7 ~ 0.8,
             TRU4_5 > 6 ~ 0.7, 
             TRU4_5 > 5 ~ 0.6,
             TRU4_5 > 4 ~ 0.5,
             TRU4_5 > 3 ~ 0.4,
             TRU4_5 > 2 ~ 0.3,
             TRU4_5 > 1 ~ 0.2, 
             TRU4_5 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_6_v2 =
           dplyr::case_when(
             TRU4_6 > 9 ~ 1, #very high trust 
             TRU4_6 > 8 ~ 0.9,
             TRU4_6 > 7 ~ 0.8,
             TRU4_6 > 6 ~ 0.7, 
             TRU4_6 > 5 ~ 0.6,
             TRU4_6 > 4 ~ 0.5,
             TRU4_6 > 3 ~ 0.4,
             TRU4_6 > 2 ~ 0.3,
             TRU4_6 > 1 ~ 0.2, 
             TRU4_6 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_7_v2 =
           dplyr::case_when(
             TRU4_7 > 9 ~ 1, #very high trust 
             TRU4_7 > 8 ~ 0.9,
             TRU4_7 > 7 ~ 0.8,
             TRU4_7 > 6 ~ 0.7, 
             TRU4_7 > 5 ~ 0.6,
             TRU4_7 > 4 ~ 0.5,
             TRU4_7 > 3 ~ 0.4,
             TRU4_7 > 2 ~ 0.3,
             TRU4_7 > 1 ~ 0.2, 
             TRU4_7 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_8_v2 =
           dplyr::case_when(
             TRU4_8 > 9 ~ 1, #very high trust 
             TRU4_8 > 8 ~ 0.9,
             TRU4_8 > 7 ~ 0.8,
             TRU4_8 > 6 ~ 0.7, 
             TRU4_8 > 5 ~ 0.6,
             TRU4_8 > 4 ~ 0.5,
             TRU4_8 > 3 ~ 0.4,
             TRU4_8 > 2 ~ 0.3,
             TRU4_8 > 1 ~ 0.2, 
             TRU4_8 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_9_v2 = 
           dplyr::case_when(
             TRU4_9 > 9 ~ 1, #very high trust 
             TRU4_9 > 8 ~ 0.9,
             TRU4_9 > 7 ~ 0.8,
             TRU4_9 > 6 ~ 0.7, 
             TRU4_9 > 5 ~ 0.6,
             TRU4_9 > 4 ~ 0.5,
             TRU4_9 > 3 ~ 0.4,
             TRU4_9 > 2 ~ 0.3,
             TRU4_9 > 1 ~ 0.2, 
             TRU4_9 > 0 ~ 0.1 #no trust at all
           ))

hu_survey <- hu_survey %>% 
  mutate(TRU4_10_v2 = 
           dplyr::case_when(
             TRU4_10 > 9 ~ 1, #very high trust 
             TRU4_10 > 8 ~ 0.9,
             TRU4_10 > 7 ~ 0.8,
             TRU4_10 > 6 ~ 0.7, 
             TRU4_10 > 5 ~ 0.6,
             TRU4_10 > 4 ~ 0.5,
             TRU4_10 > 3 ~ 0.4,
             TRU4_10 > 2 ~ 0.3,
             TRU4_10 > 1 ~ 0.2, 
             TRU4_10 > 0 ~ 0.1 #no trust at all
           ))

###### FIDESZ ONLY VISUALIZATIONS ######

### visualizations ### 

# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU3_", 1:9, "_v2")

# OPTIONAL: human-readable labels for each TRU item (edit to match your codebook)
# Example placeholder — replace with the actual party names corresponding to TRU3_1 ... TRU3_9
party_tru_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd", "Mi Hazánk", "OtherParty"
)
# If length mismatch, fallback to item names
if(length(party_tru_labels) != length(tru_items)) {
  party_tru_labels <- tru_items
}

# --- Filter to Fidesz supporters and pivot long ---
hu_fidesz_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz") %>%
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%            # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# --- summarise per TRU item (one point per target party) ---
summary_tru <- hu_fidesz_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    # map item to friendly label (preserve order)
    party_label = party_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(party_label = factor(party_label, levels = party_label)) # order by mean

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)

# --- Plot ---
ggplot(summary_tru, aes(x = party_label, y = mean_trust, color = party_label)) +
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
    title = "Fidesz Supporters' Trust in Political Parties",
    subtitle = "1 = very high trust; 0 = no trust.",
    x = "Target Party",
    y = "Mean Level of Trust",
    caption = "Error bars: 95% (thicker) and 90% (thinner). Only Fidesz supporters included."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)

## high versus low education ## 

# --- Filter to Fidesz supporters and reshape long ---
hu_fidesz_tru <- hu_survey %>%
  filter(POL2_labeled == "Fidesz",
         !is.na(education_level)) %>%
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × education ---
summary_tru <- hu_fidesz_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# compute dodge once so everything lines up
dodge <- position_dodge(width = 0.5)

# Add a label_y so labels sit just above the 95% CI
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))   # keep within [0,1]

# --- Plot with dodged, labeled counts and light label backgrounds ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +               # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",    # white background for readability
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +  # add top padding for labels
  labs(
    title = "Fidesz Supporters: Trust in Political Parties by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

##### high versus low media embeddedness ##### 

### visualizations ###

# --- Filter to Fidesz supporters and reshape long, using comp_high ---
hu_fidesz_tru <- hu_survey %>%
  filter(POL2_labeled == "Fidesz", !is.na(comp_high)) %>%
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_fidesz_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# set dodge so points, errorbars and labels align
dodge <- position_dodge(width = 0.5)

# add a label y-position so labels sit above the 95% CI (keeps <= 1)
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Fidesz Supporters: Trust in Political Parties by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

######## trust in politicians ######### 

### visualizations ### 

## all fidesz supporters ## 

# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU4_", 1:10, "_v2")

# OPTIONAL: human-readable labels for each TRU item (edit to match your codebook)
# Example placeholder — replace with the actual party names corresponding to TRU4_1 ... TRU4_9
people_tru_labels <- c(
  "Orbán Viktor", "Márki-Zay Péter", "Novák Katalin", "Gyurcsány Ferenc", "Jakab Péter", "Magyar Péter", "Karácsony Gergely", "Donáth Anna", "Dobrev Klára", "Kovács Gergely"
)

# If length mismatch, fallback to item names
if(length(people_tru_labels) != length(tru_items)) {
  people_tru_labels <- tru_items
}

# --- Filter to Fidesz supporters and pivot long ---
hu_fidesz_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled == "Fidesz") %>%
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%            # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# --- summarise per TRU item (one point per target party) ---
summary_tru <- hu_fidesz_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    # map item to friendly label (preserve order)
    party_label = people_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(party_label = factor(party_label, levels = party_label)) # order by mean

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)


# ensure we have a label position just above the 95% CI but ≤ 1
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot ---
ggplot(summary_tru, aes(x = party_label, y = mean_trust, color = party_label)) +
  geom_point(size = 3) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # n labels: placed above the 95% CI with a white label background for readability
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             size = 3.4,
             color = "gray20",
             fill = "white",
             alpha = 0.9,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Fidesz Supporters' Trust in Politicians",
    subtitle = "1 = very high trust; 0 = no trust.",
    x = "Target Party",
    y = "Mean Level of Trust",
    caption = "Error bars: 95% (thicker) and 90% (thinner). Only Fidesz supporters included."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)


## high versus low education ## 

# --- Filter to Fidesz supporters and reshape long ---
hu_fidesz_tru <- hu_survey %>%
  filter(POL2_labeled == "Fidesz",
         !is.na(education_level)) %>%
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × education ---
summary_tru <- hu_fidesz_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything aligns
dodge <- position_dodge(width = 0.5)

# --- Plot ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +   # group needed for dodging labels
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Fidesz Supporters: Trust in Politicians by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

### high versus low media embeddedness ### 

# --- Filter to Fidesz supporters and reshape long, using comp_high ---
hu_fidesz_tru <- hu_survey %>%
  filter(POL2_labeled == "Fidesz", !is.na(comp_high)) %>%
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_fidesz_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything lines up
dodge <- position_dodge(width = 0.5)

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group needed for proper dodging
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Fidesz Supporters: Trust in Politicians by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

###### NON-FIDESZ RESPONDENTS VISUALIZATIONS ######


# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU3_", 1:9, "_v2")

# friendly labels for items (adjust to your codebook)
party_tru_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd", "Mi Hazánk", "OtherParty"
)
if(length(party_tru_labels) != length(tru_items)) party_tru_labels <- tru_items

# --- Filter to non-Fidesz supporters correctly and pivot long ---
hu_non_fidesz_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz") %>%   # <--- correct selection
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%            # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# (If instead you want only a specific set of parties, use:
# filter(POL2_labeled %in% c("Jobbik","MSZP","DK","LMP","Momentum","Párbeszéd","Mi Hazánk","OtherParty")) )

# --- summarise per TRU item (one point per target party) ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    party_label = party_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(party_label = factor(party_label, levels = party_label))

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)

# --- label y (so n= sits above the 95% CI) + plotting tweaks for readability ---
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

ggplot(summary_tru, aes(x = party_label, y = mean_trust, color = party_label)) +
  geom_point(size = 3) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # readable n labels (white background so they aren't masked by points/errorbars)
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             size = 3.4, color = "gray20", fill = "white", alpha = 0.9,
             label.r = unit(0.12, "lines"), show.legend = FALSE) +
  
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Non-Fidesz supporters' Trust in Political Parties",
       subtitle = "1 = very high trust; 0 = no trust.",
       x = "Target Party", y = "Mean Level of Trust",
       caption = "Error bars: 95% (thicker) and 90% (thinner). Only Non-Fidesz supporters included.") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)

### high versus low education ### 

# --- Filter to Non-Fidesz supporters and reshape long ---
hu_non_fidesz_tru <- hu_survey %>%
  # keep respondents with a party label and NOT Fidesz, and with education info
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz", !is.na(education_level)) %>%
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × education ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# compute dodge once so everything lines up
dodge <- position_dodge(width = 0.5)

# Add a label_y so labels sit just above the 95% CI
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))   # keep within [0,1]

# --- Plot with dodged, labeled counts and light label backgrounds ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +               # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",    # white background for readability
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +  # add top padding for labels
  labs(
    title = "Non-Fidesz supporters: Trust in Political Parties by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

### high versus low media embeddedness ##### 

# --- Filter to Non-Fidesz supporters and reshape long, using comp_high ---
hu_non_fidesz_tru <- hu_survey %>%
  # keep respondents with a party label that is NOT Fidesz, and with comp_high info
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz", !is.na(comp_high)) %>%
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# set dodge so points, errorbars and labels align
dodge <- position_dodge(width = 0.5)

# add a label y-position so labels sit above the 95% CI (keeps <= 1)
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Non-Fidesz supporters: Trust in Political Parties by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])


######## trust in politicians ######### 

# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU4_", 1:10, "_v2")

# Human-readable labels for each TRU item (adjust to your codebook)
people_tru_labels <- c(
  "Orbán Viktor", "Márki-Zay Péter", "Novák Katalin", "Gyurcsány Ferenc",
  "Jakab Péter", "Magyar Péter", "Karácsony Gergely", "Donáth Anna",
  "Dobrev Klára", "Kovács Gergely"
)
# If length mismatch, fallback to item names
if (length(people_tru_labels) != length(tru_items)) people_tru_labels <- tru_items

# --- Filter to non-Fidesz supporters and pivot long (correct selection) ---
hu_non_fidesz_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz") %>%
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%            # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# --- summarise per TRU item (one point per target person) ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    # map item to friendly label (preserve order)
    people_label = people_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(people_label = factor(people_label, levels = people_label)) # order by mean

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)

# ensure we have a label position just above the 95% CI but ≤ 1
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot ---
ggplot(summary_tru, aes(x = people_label, y = mean_trust, color = people_label)) +
  geom_point(size = 3) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # n labels: placed above the 95% CI with a white label background for readability
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             size = 3.4,
             color = "gray20",
             fill = "white",
             alpha = 0.9,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Non-Fidesz supporters' Trust in Political Figures",
    subtitle = "1 = very high trust; 0 = no trust.",
    x = "Target Person",
    y = "Mean Level of Trust",
    caption = "Error bars: 95% (thicker) and 90% (thinner). Only non-Fidesz supporters included."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)

## high versus low education ## 

# --- TRU items (assumed) ---
tru_items <- paste0("TRU4_", 1:10, "_v2")

# --- Filter to Non-Fidesz supporters and reshape long ---
hu_non_fidesz_tru <- hu_survey %>%
  # keep respondents with a party label that is NOT Fidesz, and with education info
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz", !is.na(education_level)) %>%
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target person × education ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything aligns
dodge <- position_dodge(width = 0.5)

# --- Plot ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +   # group needed for dodging labels
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Non-Fidesz supporters: Trust in Political Figures by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Person",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

## high versus low media embeddedness ## 

# --- Filter to Non-Fidesz supporters and reshape long, using comp_high ---
hu_non_fidesz_tru <- hu_survey %>%
  # keep respondents with a party label that is NOT Fidesz, and with comp_high info
  filter(!is.na(POL2_labeled), POL2_labeled != "Fidesz", !is.na(comp_high)) %>%
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_non_fidesz_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything lines up
dodge <- position_dodge(width = 0.5)

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group needed for proper dodging
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "Non-Fidesz supporters: Trust in Politicians by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

###### ALL RESPONDENTS VISUALIZATIONS ######

### visualizations ### 

# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU3_", 1:9, "_v2")

# OPTIONAL: human-readable labels for each TRU item (edit to match your codebook)
# Example placeholder — replace with the actual party names corresponding to TRU3_1 ... TRU3_9
party_tru_labels <- c(
  "Fidesz", "Jobbik", "MSZP", "DK", "LMP", "Momentum", "Párbeszéd", "Mi Hazánk", "OtherParty"
)
# If length mismatch, fallback to item names
if(length(party_tru_labels) != length(tru_items)) {
  party_tru_labels <- tru_items
}

hu_all_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled)) %>%   # keep all respondents with a party label
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%      # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# -- summarise per TRU item (one point per target party) ---
summary_tru <- hu_all_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    # map item to friendly label (preserve order)
    party_label = party_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(party_label = factor(party_label, levels = party_label)) # order by mean

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)

# --- Plot ---
ggplot(summary_tru, aes(x = party_label, y = mean_trust, color = party_label)) +
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
    title = "All Respondents' Trust in Political Parties",
    subtitle = "1 = very high trust; 0 = no trust.",
    x = "Target Party",
    y = "Mean Level of Trust",
    caption = "Error bars: 95% (thicker) and 90% (thinner). All Respondents included."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)

### high versus low education ### 

# --- Filter to All Respondents and reshape long ---
hu_all_tru <- hu_survey %>%
  filter(!is.na(education_level)) %>%     # remove Fidesz-only restriction
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × education ---
summary_tru <- hu_all_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# compute dodge once so everything lines up
dodge <- position_dodge(width = 0.5)

# Add a label_y so labels sit just above the 95% CI
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))   # keep within [0,1]

# --- Plot with dodged, labeled counts and light label backgrounds ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +               # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",    # white background for readability
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +  # add top padding for labels
  labs(
    title = "All Respondents: Trust in Political Parties by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

##### high versus low media embeddedness ##### 

### visualizations ###

# --- Filter to All Respondents and reshape long, using comp_high ---
hu_all_tru <- hu_survey %>%
  filter(!is.na(comp_high)) %>%               # keep all respondents with comp_high info
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = party_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_all_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# set dodge so points, errorbars and labels align
dodge <- position_dodge(width = 0.5)

# add a label y-position so labels sit above the 95% CI (keeps <= 1)
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group so dodge works consistently
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: dodge horizontally to match the point, place at label_y
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "All Respondents: Trust in Political Parties by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

######## trust in politicians ######### 

### visualizations ### 

## All Respondents ## 

# --- TRU items (you said they exist as *_v2) ---
tru_items <- paste0("TRU4_", 1:10, "_v2")

# OPTIONAL: human-readable labels for each TRU item (edit to match your codebook)
# Example placeholder — replace with the actual party names corresponding to TRU4_1 ... TRU4_9
people_tru_labels <- c(
  "Orbán Viktor", "Márki-Zay Péter", "Novák Katalin", "Gyurcsány Ferenc", "Jakab Péter", "Magyar Péter", "Karácsony Gergely", "Donáth Anna", "Dobrev Klára", "Kovács Gergely"
)

# If length mismatch, fallback to item names
if(length(people_tru_labels) != length(tru_items)) {
  people_tru_labels <- tru_items
}

hu_all_tru <- hu_survey %>%
  filter(!is.na(POL2_labeled)) %>%   # keep all respondents with a party label
  select(all_of(tru_items)) %>%
  mutate(id = row_number()) %>%      # temporary id for pivoting
  pivot_longer(cols = -id, names_to = "item", values_to = "trust") %>%
  filter(!is.na(trust))

# --- summarise per TRU item (one point per target party) ---
summary_tru <- hu_all_tru %>%
  group_by(item) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90),
    # map item to friendly label (preserve order)
    party_label = people_tru_labels[match(item, tru_items)]
  ) %>%
  arrange(mean_trust) %>%
  mutate(party_label = factor(party_label, levels = party_label)) # order by mean

# --- palette sized to number of items ---
n_items <- nrow(summary_tru)
palette_vals <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_items)


# ensure we have a label position just above the 95% CI but ≤ 1
summary_tru <- summary_tru %>% mutate(label_y = pmin(1, ymax95 + 0.03))

# --- Plot ---
ggplot(summary_tru, aes(x = party_label, y = mean_trust, color = party_label)) +
  geom_point(size = 3) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9) +
  
  # 90% CI (thinner, drawn on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9) +
  
  # n labels: placed above the 95% CI with a white label background for readability
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             size = 3.4,
             color = "gray20",
             fill = "white",
             alpha = 0.9,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # layout / scale tweaks: add top padding so labels don't get clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "All Respondents' Trust in Politicians",
    subtitle = "1 = very high trust; 0 = no trust.",
    x = "Target Party",
    y = "Mean Level of Trust",
    caption = "Error bars: 95% (thicker) and 90% (thinner). All Respondents included."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values = palette_vals)


## high versus low education ## 

# --- Filter to All Respondents and reshape long ---
hu_all_tru <- hu_survey %>%
  filter(!is.na(education_level)) %>%
  select(education_level, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    education_f = factor(
      education_level,
      levels = c(0, 1),
      labels = c("Low education", "High education")
    ),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × education ---
summary_tru <- hu_all_tru %>%
  group_by(party_label, education_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional but nice) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything aligns
dodge <- position_dodge(width = 0.5)

# --- Plot ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = education_f,
           group = education_f)) +   # group needed for dodging labels
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "All Respondents: Trust in Politicians by Education",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust",
    color = "Education level",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])

### high versus low media embeddedness ### 

# --- Filter to All Respondents and reshape long, using comp_high ---
hu_all_tru <- hu_survey %>%
  filter(!is.na(comp_high)) %>%
  select(comp_high, all_of(tru_items)) %>%
  pivot_longer(
    cols = all_of(tru_items),
    names_to = "item",
    values_to = "trust"
  ) %>%
  filter(!is.na(trust)) %>%
  mutate(
    comp_high_f = factor(comp_high, levels = c(0, 1),
                         labels = c("Low embeddedness", "High embeddedness")),
    party_label = people_tru_labels[match(item, tru_items)]
  )

# --- Summarise by target party × comp_high ---
summary_tru <- hu_all_tru %>%
  group_by(party_label, comp_high_f) %>%
  summarise(
    mean_trust = mean(trust, na.rm = TRUE),
    sd = sd(trust, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci95 = 1.96 * se,
    ci90 = 1.645 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin95 = pmax(0, mean_trust - ci95),
    ymax95 = pmin(1, mean_trust + ci95),
    ymin90 = pmax(0, mean_trust - ci90),
    ymax90 = pmin(1, mean_trust + ci90)
  )

# --- Order parties by overall mean trust (optional) ---
party_order <- summary_tru %>%
  group_by(party_label) %>%
  summarise(overall_mean = mean(mean_trust)) %>%
  arrange(overall_mean) %>%
  pull(party_label)

summary_tru$party_label <- factor(summary_tru$party_label, levels = party_order)

# ensure labels sit just above the 95% CI (but never above 1)
summary_tru <- summary_tru %>%
  mutate(label_y = pmin(1, ymax95 + 0.03))

# shared dodge so everything lines up
dodge <- position_dodge(width = 0.5)

# --- Plot: two points per party (Low vs High embeddedness) ---
ggplot(summary_tru,
       aes(x = party_label,
           y = mean_trust,
           color = comp_high_f,
           group = comp_high_f)) +   # group needed for proper dodging
  # points
  geom_point(size = 3, position = dodge) +
  
  # 95% CI (thicker)
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95),
                width = 0.2, linewidth = 1.2, alpha = 0.9,
                position = dodge) +
  
  # 90% CI (thinner, on top)
  geom_errorbar(aes(ymin = ymin90, ymax = ymax90),
                width = 0.15, linewidth = 0.7, alpha = 0.9,
                position = dodge) +
  
  # n labels: aligned with points, readable, unclipped
  geom_label(aes(y = label_y, label = paste0("n=", n)),
             position = dodge,
             size = 3.0,
             color = "gray20",
             fill = "white",
             alpha = 0.85,
             label.r = unit(0.12, "lines"),
             show.legend = FALSE) +
  
  # add top padding so labels aren't clipped
  scale_y_continuous(limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.06))) +
  
  labs(
    title = "All Respondents: Trust in Politicians by Media Embeddedness",
    subtitle = "1 = very high trust; 0 = no trust at all",
    x = "Target Party",
    y = "Mean Trust (0–1)",
    color = "Media embeddedness",
    caption = "Error bars show 95% (thicker) and 90% (thinner) confidence intervals."
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "Dark2")[1:2])



