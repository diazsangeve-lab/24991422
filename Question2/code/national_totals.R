# Collapse the national name table across gender, giving one count per name-year.
# Most of the culture work does not care about gender, so this is the base table:
national_totals <- function(nat){
    nat %>%
        group_by(Year, Name) %>% # ignore gender
        summarise(Count = sum(Count), .groups = "drop") # total babies with that name
}