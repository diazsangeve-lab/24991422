# Did people name children after chart-topping singers? I take every act that
# reached the Billboard top ten, read its first name, and test for a baby-name
# surge in the years after it first charted.
billboard_name_spikes <- function(nat, charts, top_rank = 10, min_post = 250, min_ratio = 3){
    tot <- national_totals(nat)

    arrivals <-
        charts %>%
        mutate(Year = year(date)) %>% # chart year from the weekly date
        filter(rank <= top_rank) %>% # top-ten hits only
        mutate(Name = first_name(artist)) %>% filter(!is.na(Name)) %>%
        group_by(Name, artist) %>%
        summarise(Chart_Year = min(Year), .groups = "drop") # when the act first broke through

    arrivals %>%
        mutate(spk = map2(Name, Chart_Year, ~name_surge(tot, .x, .y))) %>% # the baby-name response
        unnest(spk) %>%
        filter(Post >= min_post, Post >= min_ratio * (Pre + 5)) %>% # a real surge, not noise
        arrange(desc(Ratio)) %>% # strongest first
        distinct(Name, .keep_all = TRUE) # keep each name's best link
}