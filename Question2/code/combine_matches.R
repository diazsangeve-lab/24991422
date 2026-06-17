# Stack the music and screen match tables into one tidy set for the bubble and
# the event study, keeping the stronger link where a name appears in both.
combine_matches <- function(bb, hbo){
    bind_rows(
        bb  %>%
            transmute(Name, Source = "Music",  Event_Year = Chart_Year,  Peak_Year, Post, Ratio),
        hbo %>%
            transmute(Name, Source = "Screen", Event_Year = release_year, Peak_Year, Post, Ratio)
    ) %>%
        arrange(desc(Ratio)) %>% # strongest links first
        distinct(Name, .keep_all = TRUE) # one row per name
}