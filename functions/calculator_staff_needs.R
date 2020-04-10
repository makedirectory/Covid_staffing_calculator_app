calculator_staff_needs <- function(team_type, team_ratios, n_pt) {
  team_ratios %>% 
    transmute(team_type = team_type,
              role,
              n_staff = ceiling(n_pt/as.numeric(ratio)),
              n_staff_strech = ceiling(n_pt/as.numeric(ratio_s)),
              n_staff_day = n_staff*(24/shift_length_hr),
              n_staff_strech_day = n_staff_strech*(24/shift_length_hr),
              n_staff_week = n_staff_day*7/shift_per_week,
              n_staff_strech_week = n_staff_day*7/shift_per_week) %>%
    mutate_if(is.numeric, as.integer)
}
