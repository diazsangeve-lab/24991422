# The supervisor's bubble chart, built from the names the data can attribute to a
# singer or a screen character. Names run along the x-axis, the year of the peak
# up the y-axis, the bubble is babies named, and colour marks music against screen.
# Ranking by the surge ratio surfaces the names that culture all but created.
plot_spike_bubble <- function(matches, n = 22){
    matches %>%
        slice_max(Ratio, n = n) %>% # the clearest culture-driven names
        mutate(Name = fct_reorder(Name, Peak_Year)) %>% # left to right in time
        ggplot(aes(x = Name, y = Peak_Year, colour = Source)) +
        # Lollipops anchor the bubbles to the timeline
        geom_segment(aes(xend = Name, y = min(Peak_Year) - 2, yend = Peak_Year),
                     colour = "grey85", linewidth = 0.8) +
        # Jitter helps if multiple names peak in the same year
        geom_jitter(aes(size = Post), alpha = 0.85, width = 0.2, height = 0.2) +
        geom_text(aes(label = Post), size = 3, vjust = -1.8, fontface = "bold", colour = "black") +
        # Vibrant palette and sized to highlight the "surge" impact
        scale_size(range = c(4, 18), guide = "none") +
        scale_colour_manual(values = c(Screen = "#D62828", Music = "#7209B7")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        coord_cartesian(clip = "off") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              panel.grid.major.x = element_blank()) + # Clean vertical axis
        labs(x = "", y = "Year of peak", colour = "",
             title = "Singers and characters who named a generation",
             subtitle = "Names that surged after a cultural hit, sized by babies at peak",
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 11))
}
