library(tidyverse)
library(gsheet)
library(openxlsx)


# step1: patients and bed needs  input----
n_covid_pt = 140
#icu
n_pt_icu = 40
n_pt_icu_vent = 10


n_pt_non_icu = n_covid_pt - n_pt_icu
n_pt_icu_non_vent = n_pt_icu - n_pt_icu_vent


# # read team ratio from google-----
# teams_staff = gsheet2tbl("https://docs.google.com/spreadsheets/d/1RoKOdtWo5XSD6O6s2iXKygoLpGAklZuV8Ik1r0qDwXM/edit#gid=0") %>%
#   rename_all(tolower)
# 
# team_ratio = teams_staff %>%
#   mutate(team_tpye = case_when(team_structure_id == 1 ~ "ICU",
#                           team_structure_id == 2 ~ "General")) %>%
#   select(team_structure_id, team_tpye, role, n_bed_per_person, n_bed_per_person_stretch)
# # 
# # save(team_ratio, file = "data/team_ratio.rdata")
#  saveRDS(team_ratio, file = "../covid_staff_calculator/data/team_ratio.rds")

# step2: load ratio ------
load("./covid_staff_calculator/data/team_ratio.rds")


# step3 assign staff -----
# ICU ------
# icu team ratio 
icu_ratio = team_ratio %>% 
  filter(team_tpye == "ICU") 

staff_icu = icu_ratio %>% 
  transmute(role,
            n_staff = ceiling(n_icu_pt/n_bed_per_person),
            n_staff_strech = ceiling(n_icu_pt/n_bed_per_person_stretch))


# General --------
n_pt_covid_gen = n_covid_pt*covid_general_ratio

# non icu team ratio 
gen_ratio = team_ratio %>% 
  filter(team_tpye == "General") 

staff_gen = gen_ratio %>% 
  transmute(role,
            n_staff = ceiling(n_pt_covid_gen/n_bed_per_person),
            n_staff_strech = ceiling(n_pt_covid_gen/n_bed_per_person_stretch))



full_join(staff_gen, staff_icu, by = "role") %>% 
   mutate_if(is.numeric, as.integer)  %>% 
   mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
   tidyext::row_sums(n_staff, n_staff_needed, varname = "n_normal", na_rm = T) %>% 
   tidyext::row_sums(n_staff_strech, n_person_strech_icu, varname = "n_strech", na_rm = T) %>% 
   select(role, n_normal, n_strech) 



