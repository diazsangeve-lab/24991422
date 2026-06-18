# Context for everything that follows. How the catalogue splits between films and
# shows, and how heavily it leans on recent releases, shown by decade:
plot_catalogue <- function(titles){

    titles %>%
        filter(release_year <= 2022) %>%
        mutate(Decade = floor(release_year / 10) * 10) %>% # group releases into decades
        count(Decade, type) %>%
        ggplot(aes(factor(Decade), n, fill = type)) +
        geom_col() + # films and shows stacked
        scale_fill_manual(values = c(MOVIE = "red3", SHOW = "black")) +
        theme_minimal() +
        labs(x = "Release decade", y = "Titles", fill = NULL,
             title = "A young, movie-heavy catalogue",
             caption = "Data source: Netflix Titles Dataset") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
