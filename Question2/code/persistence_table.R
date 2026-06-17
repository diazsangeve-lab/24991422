# Summarise the persistence series into the clean table the agency asked for:
# mean rank similarity by gender and horizon, before and from 1990, with the gap.
persistence_table <- function(pers){
    pers %>%
        mutate(Era    = if_else(Year < 1990, "Pre-1990", "1990 onward"), # the two eras
               Gender = recode(Gender, M = "Boys", F = "Girls"),
               Horizon = str_c(Horizon, "-year")) %>%
        group_by(Gender, Horizon, Era) %>%
        summarise(Spearman = mean(Spearman), .groups = "drop") %>% # average within each cell
        pivot_wider(names_from = Era, values_from = Spearman) %>% # eras side by side
        mutate(Change = `1990 onward` - `Pre-1990`) %>% # the decline
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        arrange(Gender, Horizon)
}