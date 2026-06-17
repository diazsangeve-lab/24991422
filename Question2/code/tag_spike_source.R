# Label each data-driven surge by what, if anything, in the culture datasets
# explains it. A surge is "Music" if a singer of that name first charted nearby,
# "Screen" if a character of that name appeared in a title nearby, else "Other".
tag_spike_source <- function(spikes, bb, hbo, tol = 2){
    music  <- bb  %>%
        transmute(Name, Src_Year = Chart_Year,  Src = "Music") # singer links

    screen <- hbo %>%
        transmute(Name, Src_Year = release_year, Src = "Screen") # character links

    links  <- bind_rows(music, screen)

    spikes %>%
        left_join(links, by = "Name", relationship = "many-to-many") %>%
        mutate(hit = !is.na(Src) & abs(Year - Src_Year) <= tol) %>%  # the link must line up in time
        group_by(Name, Year, Count, Prev, Growth) %>%
        summarise(Source = case_when(any(hit & Src == "Screen") ~ "Screen", # screen takes priority
                                     any(hit & Src == "Music")  ~ "Music",
                                     TRUE ~ "Other"),
                  .groups = "drop") %>%
        arrange(desc(Growth))
}