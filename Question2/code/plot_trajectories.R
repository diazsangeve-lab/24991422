# Trajectories of a chosen set of names over time. Used for the fade-versus-stick view and for the
# Billboard and HBO, always fed names that the data surfaced.
plot_trajectories <- function(tot, names, events = NULL, from = 1940, title = "", subtitle = ""){
    d <- tot %>% filter(Name %in% names, Year >= from) # just the names of interest

    # Isolate the exact peak year for each name to anchor the labels
    peaks <- d %>%
        group_by(Name) %>%
        slice_max(Count, n = 1, with_ties = FALSE) %>%
        ungroup()

    g <- ggplot(d, aes(x = Year, y = Count, fill = Name, colour = Name))

    if(!is.null(events)){
        g <- g + geom_vline(data = events, aes(xintercept = Event_Year),
                            colour = "grey70", linetype = "dashed", linewidth = 0.6)
    }

    g +
        # Creative swap: Overlapping transparent areas ground the lines to the axis
        geom_area(alpha = 0.15, position = "identity", colour = NA) +
        geom_line(linewidth = 1.2) +
        # Creative swap: Drop the legend and label the peaks directly with a ring marker
        geom_point(data = peaks, size = 3, shape = 21, fill = "white", stroke = 1.2) +
        ggrepel::geom_text_repel(data = peaks, aes(label = Name),
                                 size = 4, fontface = "bold",
                                 nudge_y = max(d$Count) * 0.08, # Push labels slightly above the peak
                                 segment.color = NA, show.legend = FALSE) +
        scale_colour_brewer(palette = "Set2", guide = "none") +
        scale_fill_brewer(palette = "Set2", guide = "none") +
        theme_minimal() +
        labs(x = "", y = "Babies named",
             title = title, subtitle = subtitle,
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 12, face = "bold"),
              plot.subtitle = element_text(size = 10),
              panel.grid.minor = element_blank()) # Clean up background grid
}