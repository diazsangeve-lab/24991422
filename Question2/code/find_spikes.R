# Data-driven discovery of surges. For every name I take the year-on-year jump,
# and keep names that were already sizable and at least doubled. Nothing here is
# hand-picked, the spikes fall out of the data itself.
find_spikes <- function(nat, min_count = 300, min_prev = 30, min_growth = 2){
    national_totals(nat) %>%
        arrange(Name, Year) %>% # in time order per name
        group_by(Name) %>%
        mutate(Prev = lag(Count), Growth = Count / Prev) %>% # the year-on-year jump
        ungroup() %>%
        filter(Count >= min_count, Prev >= min_prev, Growth >= min_growth) %>%  # real, sizable surges
        group_by(Name) %>% slice_max(Growth, n = 1) %>% ungroup() %>%  # each name's sharpest year
        arrange(desc(Growth)) # biggest first
}