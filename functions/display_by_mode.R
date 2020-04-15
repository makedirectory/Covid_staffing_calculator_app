display_by_mode <- function(data, staff_needs) {
  data %>%
    filter(!is.na(role)) %>%
    select(team_type, role, !!staff_needs) %>%
    pivot_wider(names_from = team_type,
                values_from = !!staff_needs) %>%
    tidyext::row_sums(gen, icu, varname = "all", na_rm = TRUE) %>%
    filter(all != 0) %>%
    mutate_if(is.numeric, as.integer)
}

