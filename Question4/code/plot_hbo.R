# Netflix set against HBO. The median IMDb score per genre for each platform's
# films, the creative comparison the brief invites, using the HBO titles from the
# baby-names question. genres_keep avoids clashing with the raw genres column:
plot_hbo <- function(nf_movies, hbo_movies, genres_keep){

    bind_rows(
        nf_movies  %>% unnest(genre) %>% mutate(Platform = "Netflix"), # Netflix films
        hbo_movies %>% unnest(genre) %>% mutate(Platform = "HBO")) %>% # HBO films
        filter(genre %in% genres_keep) %>%
        group_by(Platform, genre) %>%
        summarise(Score = median(imdb_score, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(genre, Score, fill = Platform, group = Platform)) +
        # Closed polygon for each platform – the radar shape
        geom_polygon(aes(colour = Platform), alpha = 0.25, linewidth = 1.2) +
        # Optional: points at each genre vertex for clarity
        geom_point(aes(colour = Platform), size = 2.5) +
        scale_fill_manual(values = c(Netflix = "red", HBO = "purple3"),
                          aesthetics = c("fill", "colour")) +
        coord_polar(start = - pi / length(genres_keep)) +   # the radar projection
        coord_cartesian(ylim = c(5.5, 7.5)) +               # keep your zoom
        theme_minimal() +
        labs(x = NULL, y = "Median IMDb score",
             title = "HBO's films rate higher in every genre",
             caption = "Data source: Netflix and HBO Titles Datasets") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),  # rotates genre labels around the circle
              plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
