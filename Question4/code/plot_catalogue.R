# Context for everything that follows. How the catalogue splits between films and
# shows, and how heavily it leans on recent releases, shown by decade:
plot_catalogue <- function(titles){

    titles %>%
        filter(release_year <= 2022) %>%
        mutate(Decade = floor(release_year / 10) * 10) %>% # group releases into decades
        count(Decade, type) %>%
        ggplot(aes(x = Decade, y = n, fill = type)) +
        # A stacked area plot for a smoother time-series narrative
        geom_area(alpha = 0.85, colour = "white", linewidth = 0.5) +
        scale_fill_manual(values = c(MOVIE = "red3", SHOW = "black")) +
        theme_minimal() +
        labs(x = "Release decade", y = "Titles", fill = NULL,
             title = "A young, movie-heavy catalogue",
             caption = "Data source: Netflix Titles Dataset") +
        theme(plot.title = element_text(size = 11, face = "bold"),
              plot.subtitle = element_text(size = 9),
              legend.position = "top")
}
