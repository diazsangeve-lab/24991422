# How long the films run. Median runtime by country for the major producers, which
# sets the length of a typical film from each tradition side by side:
plot_length <- function(movies, countries){

    movies %>%
        unnest(country) %>% filter(country %in% countries) %>%
        group_by(country) %>%
        summarise(Runtime = median(runtime, na.rm = TRUE), .groups = "drop") %>% # typical film length
        ggplot(aes(reorder(country, Runtime), Runtime)) +
        geom_point(aes(size = Runtime, colour = Runtime), alpha = 0.9) +
        scale_colour_viridis_c(guide = "none") +
        scale_size_continuous(range = c(4, 12), guide = "none") +
        coord_flip() +
        geom_text(aes(label = round(Runtime)), hjust = 0.5, vjust = 0.5, size = 3, colour = "floralwhite") +
        theme_minimal() +
        labs(x = NULL, y = "Median runtime (minutes)",
             title = "How long the movies run, by country",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
