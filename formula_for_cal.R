library(tidyverse)
library(gsheet)
library(openxlsx)


# step1: patients and bed needs  input----
n_covid_pt = 140
#icu
icu_ratio = 0.3
icu_vent_ratio = 0.2
#general
covid_general_ratio = 0.6


# read team ratio from google-----
# teams_staff = gsheet2tbl("https://docs.google.com/spreadsheets/d/1YuNDBqcQm7Qc5YOkzDBId_yg3nqL1dzg/edit#gid=470575383") %>%
#   rename_all(tolower)
# 
# team_ratio = teams_staff %>%
#   mutate(team_tpye = case_when(team_structure_id == 1 ~ "ICU",
#                           team_structure_id == 2 ~ "General")) %>%
#   select(team_structure_id, team_tpye, role, n_bed_per_person, n_bed_per_person_stretch)
# 
# save(team_ratio, file = "data/team_ratio.rdata")

# step2: load ratio ------
load("data/team_ratio.rdata")


# step3 assign staff -----
# ICU ------
n_icu_pt = n_covid_pt*icu_ratio
n_icu_pt_vent = ceiling(n_icu_pt*icu_vent_ratio)
n_icu_pt_non_vent = n_icu_pt - n_icu_pt_vent

# icu team ratio 
icu_ratio = team_ratio %>% 
  filter(team_tpye == "ICU") 

staff_icu = icu_ratio %>% 
  transmute(role,
            n_staff = ceiling(n_icu_pt/n_bed_per_person),
            n_staff_strech = ceiling(n_icu_pt/n_bed_per_person_stretch))


# General --------
n_pt_covid_gen = n_covid_pt*covid_general_ratio

# icu team ratio 
gen_ratio = team_ratio %>% 
  filter(team_tpye == "General") 

staff_gen = gen_ratio %>% 
  transmute(role,
            n_staff = ceiling(n_pt_covid_gen/n_bed_per_person),
            n_staff_strech = ceiling(n_pt_covid_gen/n_bed_per_person_stretch))








