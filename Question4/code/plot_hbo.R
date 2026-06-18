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
        ggplot(aes(genre, Score, fill = Platform)) +
        geom_col(position = "dodge") +   # two bars per genre
        scale_fill_manual(values = c(Netflix = "red", HBO = "purple3")) +
        coord_cartesian(ylim = c(5.5, 7.5)) + # zoom in on the gap
        theme_minimal() +
        labs(x = NULL, y = "Median IMDb score",
             title = "HBO's films rate higher in every genre",
             caption = "Data source: Netflix and HBO Titles Datasets") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
