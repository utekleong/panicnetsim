#loading packages
library(tidyverse)
library(haven)

#loading data and subsetting to SCID data

#STEP
data_step <- readxl::read_excel("./data/data_step.xlsx") %>% 
  select(code, Baseline_SCID_Dx, "SCID F2":"SCID F14") %>%
  rename(id = code,
         SCID_Dx = Baseline_SCID_Dx,
         palpitations = 'SCID F2',
         sweating = 'SCID F3',
         shake = 'SCID F4',
         sob = 'SCID F5',
         choking = 'SCID F6',
         chestpain = 'SCID F7',
         nausea = 'SCID F8',
         dizzy = 'SCID F9',
         hotchill = 'SCID F10',
         numb = 'SCID F11',
         deperson = 'SCID F12',
         crazy = 'SCID F13',
         dying = 'SCID F14')
data_step <- data_step[-1,]
data_step[,3:15] <- lapply(data_step[,3:15], as.double)
  
#STEP-T
data_stept <- readxl::read_excel("./data/data_stept.xlsx") %>% 
  select(id, SCID_diagnosis, F2:F14)  %>%
  rename(SCID_Dx = SCID_diagnosis,
         palpitations = F2,
         sweating = F3,
         shake = F4,
         sob = F5,
         choking = F6,
         chestpain = F7,
         nausea = F8,
         dizzy = F9,
         hotchill = F10,
         numb = F11,
         deperson = F12,
         crazy = F13,
         dying = F14)
data_stept <- data_stept[-1,]
data_stept[,3:15] <- lapply(data_stept[,3:15], as.double)


# CDR Pilot
#number of patients with "Panic Attack" does not tally with the original study
data_cdrpilot <- read_sav("./data/data_cdrpilot.sav")  %>% 
  select(Subject_ID, starts_with("SCID"), -SCID_Duration_to_peak)

##demographic data
data_cdrpilot_demo <- read_sav("./data/data_cdrpilot.sav") %>%
  as_factor(levels = "default") %>% 
  select(Subject_ID, Age, Gender, Marital_status, Race, Education, Income_Range) %>% 
  rename(id = Subject_ID,
         age = Age,
         gender = Gender,
         marital_status = Marital_status,
         ethnicity = Race,
         education = Education,
         income = Income_Range)
data_cdrpilot_demo $id <- paste0("CDRP", data_cdrpilot_demo $id)
#write.csv(data_cdrpilot_demo, "./data/demo_cdrpilot.csv", row.names = FALSE)

data_cdrpilot$SCID_Dx <- as.factor(data_cdrpilot$SCID_Dx)
levels(data_cdrpilot$SCID_Dx) <- c("Panic Disorder", "Panic Attack", "No Diagnosis")
  
data_cdrpilot <- data_cdrpilot %>%  
  filter(SCID_Dx != "No Diagnosis") %>% 
  rename(id = Subject_ID,
         palpitations = SCID_Palpitations,
         sweating = SCID_Sweating,
         shake = SCID_Tremble_Shake,
         sob = SCID_SOB,
         choking = SCID_Choking,
         chestpain = SCID_Chest_Pain,
         nausea = SCID_Nausea,
         dizzy = SCID_Dizzy,
         hotchill = SCID_Chills_Flushes,
         numb = SCID_Parethesias,
         deperson = SCID_Derealization,
         crazy = SCID_Lose_control,
         dying = SCID_Dying)

# CDR Validation
data_cdrvalid <- read_sav("./data/data_cdrvalid.sav")  %>% 
  select(Subject_ID, CRF_QE6, starts_with("SCID"), -SCID_Duration_to_peak)

##demographic data
data_cdrvalid_demo <- read_sav("./data/data_cdrvalid.sav")  %>%
  as_factor(levels = "default") %>% 
  select(Subject_ID, CRF_QA2, CRF_QA1, CRF_QA4, CRF_QA5, CRF_QA8, CRF_QA10) %>% 
  rename(id = Subject_ID,
         age = CRF_QA2,
         gender = CRF_QA1,
         marital_status = CRF_QA4,
         ethnicity = CRF_QA5,
         education = CRF_QA8,
         income = CRF_QA10)
data_cdrvalid_demo$id <- paste0("CDRV", data_cdrvalid_demo$id)
#write.csv(data_cdrvalid_demo, "./data/demo_cdrvalid.csv", row.names = FALSE)

data_cdrvalid$CRF_QE6 <- as.factor(data_cdrvalid$CRF_QE6)
levels(data_cdrvalid$CRF_QE6) <- c("Panic Disorder", "Panic Attack", "No Diagnosis")

data_cdrvalid <- data_cdrvalid %>%  
  filter(CRF_QE6 == "Panic Disorder" | CRF_QE6 == "Panic Attack") %>% 
  rename(id = Subject_ID,
         SCID_Dx = CRF_QE6,
         palpitations = SCID_Palpitations,
         sweating = SCID_Sweating,
         shake = SCID_Tremble_Shake,
         sob = SCID_SOB,
         choking = SCID_Choking,
         chestpain = SCID_Chest_Pain,
         nausea = SCID_Nausea,
         dizzy = SCID_Dizzy,
         hotchill = SCID_Chills_Flushes,
         numb = SCID_Parethesias,
         deperson = SCID_Derealization,
         crazy = SCID_Lose_control,
         dying = SCID_Dying)

#combining the 4 datasets
data_cdrpilot$id <- paste0("CDRP", data_cdrpilot$id)
data_cdrvalid$id <- paste0("CDRV", data_cdrvalid$id)

data_combined <- rbind(data_cdrpilot, data_cdrvalid) %>% 
  rbind(data_step) %>% 
  rbind(data_stept)

data_combined_recoded <- data_combined %>% 
  mutate(across(palpitations:hotchill, 
                ~ case_match(.x, 
                             1 ~ 0,
                             2 ~ 1,
                             3 ~ 1))) %>%
  select(-SCID_Dx) %>% 
  na.omit()
#write.csv(data_combined_recoded, "./data/data_combined.csv", row.names = FALSE)

data_combined_sensitivity <- data_combined %>% 
  mutate(across(palpitations:hotchill, 
                ~ case_match(.x, 
                             1 ~ 0,
                             2 ~ 0,
                             3 ~ 1))) %>%
  select(-SCID_Dx) %>% 
  na.omit()
#write.csv(data_combined_sensitivity, "./data/data_combined_sensitivity.csv", row.names = FALSE)
