# An event study pooling the culturally driven names. I line every name up on the
# year its trigger landed, scale it to its own pre-event level, then average across
# names. The result shows the typical shape and speed of a naming response.
plot_event_study <- function(tot, events, back = 4, fwd = 6){
    map_dfr(seq_len(nrow(events)), function(i){ # for each matched name-event
        nm <- events$Name[i];
        yr <- events$Event_Year[i];
        src <- events$Source[i]

        s <-
            tot %>%
            filter(Name == nm, Year >= yr - back, Year <= yr + fwd)

        base <-
            s %>%
            filter(Year < yr) %>%
            summarise(m = mean(Count)) %>%
            pull(m)  # pre-event level

        s %>%
            transmute(Source = src, t = Year - yr, Index = Count / (base + 1)) # index to pre-event
    }) %>%
    group_by(Source, t) %>% # average across names
    summarise(Index = mean(Index), .groups = "drop") %>%
    ggplot(aes(t, Index, colour = Source)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +  # the event
    geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
    scale_colour_manual(values = c(Music = "darkorchid4", Screen = "darkslategray1")) +
    theme_minimal() +
    labs(x = "Years from the cultural event", y = "Babies named, relative to before",
         title = "How fast a culture names a child",
         subtitle = "Average naming response around a hit song or a screen premiere")
}