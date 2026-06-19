# Runtime distributions for the major producing countries, drawn as gradient
# ridges. Each tradition sits at its own length, with India shifted clearly to the
# right of the American films:
plot_runtime_ridges <- function(movies, countries){

    movies %>%
        unnest(country) %>%
        filter(country %in% countries, !is.na(runtime), runtime > 0) %>%
        ggplot(aes(x = runtime, y = reorder(country, runtime, median), fill = after_stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 2.2, rel_min_height = 0.01, colour = "white") +
        scale_fill_viridis_c(option = "rocket", guide = "none") +
        coord_cartesian(xlim = c(40, 200)) +
        theme_minimal() +
        labs(x = "Runtime (minutes)", y = "",
             title = "Each film tradition runs to its own length",
             subtitle = "Distribution of film runtime for the major producers",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
