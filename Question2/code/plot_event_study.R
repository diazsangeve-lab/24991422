# An event study pooling the culturally driven names. For each matched name I
# take a window around the year its trigger landed, FILL ANY ABSENT YEARS WITH
# ZERO (rare names simply do not appear in the births file before they exist),
# then scale the name to its own peak so every name sits on a zero-to-one footing.
# Averaging across names gives the typical shape and speed of a naming response.
plot_event_study <- function(tot, events, back = 4, fwd = 6){

    map_dfr(seq_len(nrow(events)), function(i){  # for each matched name-event
        nm <- events$Name[i];
        yr <- events$Event_Year[i];
        src <- events$Source[i]

        tibble(Source = src, t = -back:fwd, Year = (yr - back):(yr + fwd)) %>%  # every year in the window
            left_join(tot %>% filter(Name == nm) %>% select(Year, Count), by = "Year") %>%
            mutate(Count = replace_na(Count, 0)) %>% # absent years are real zeros, not missing
            mutate(Share = Count / max(Count)) %>% # scale to this name's own peak
            select(Source, t, Share)
    }) %>%
    group_by(Source, t) %>% # average across names at each offset
    summarise(Share = mean(Share, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(t, Share, colour = Source)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +  # the cultural event
    geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
    scale_colour_manual(values = c(Music = "darkorchid4", Screen = "turquoise3")) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    labs(x = "Years from the cultural event (0 = the hit or premiere)",
         y = "Share of the name's own peak", colour = "",
         title = "How fast a culture names a child",
         subtitle = "Average naming response, with each name scaled so its own peak is one",
         caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
