# Trajectories of a chosen set of names over time. Used for the fade-versus-stick view and for the
# Billboard and HBO, always fed names that the data surfaced.
plot_trajectories <- function(tot, names, events = NULL, from = 1940, title = "", subtitle = ""){
    d <- tot %>% filter(Name %in% names, Year >= from) # just the names of interest

    g <- ggplot(d, aes(Year, Count, colour = Name))
    if(!is.null(events))
        g <- g + geom_vline(data = events, aes(xintercept = Event_Year), # mark the trigger years
                            colour = "grey80", linewidth = 0.3)
    g +
        geom_line(linewidth = 0.8) + # each name's path
        scale_colour_brewer(palette = "Set2") +
        theme_minimal() +
        labs(x = "", y = "Babies named", colour = "", title = title, subtitle = subtitle)
}