# The supervisor's bubble chart, built from the names the data can attribute to a
# singer or a screen character. Names run along the x-axis, the year of the peak
# up the y-axis, the bubble is babies named, and colour marks music against screen.
# Ranking by the surge ratio surfaces the names that culture all but created.
plot_spike_bubble <- function(matches, n = 22){
    matches %>%
        slice_max(Ratio, n = n) %>% # the clearest culture-driven names
        mutate(Name = fct_reorder(Name, Peak_Year)) %>% # left to right in time
        ggplot(aes(Name, Peak_Year, size = Post, colour = Source)) +
        geom_point(alpha = 0.85) +  # one bubble per name
        geom_text(aes(label = Post), size = 2.4, vjust = -1.6, show.legend = FALSE, colour = "black") +  # the count
        scale_size(range = c(3, 16), guide = "none") +
        scale_colour_manual(values = c(Screen = "red3", Music = "darkorchid2")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "", y = "Year of peak", colour = "",
             title = "Singers and characters who named a generation",
             subtitle = "Names that surged after a hit or a premiere, sized by babies at peak",
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
