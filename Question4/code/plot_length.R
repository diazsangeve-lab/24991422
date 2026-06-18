# How long the films run. Median runtime by country for the major producers, which
# sets the length of a typical film from each tradition side by side:
plot_length <- function(movies, countries){

    movies %>%
        unnest(country) %>% filter(country %in% countries) %>%
        group_by(country) %>%
        summarise(Runtime = median(runtime, na.rm = TRUE), .groups = "drop") %>% # typical film length
        ggplot(aes(reorder(country, Runtime), Runtime)) +
        geom_col(fill = "red") + coord_flip() +
        geom_text(aes(label = round(Runtime)), hjust = -0.2, size = 3) +
        theme_minimal() +
        labs(x = NULL, y = "Median runtime (minutes)",
             title = "How long the movies run, by country",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
