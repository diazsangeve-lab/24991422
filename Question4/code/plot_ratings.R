# Ratings against reach. Each genre placed by its median IMDb score, the acclaim,
# against its median IMDb votes, the popularity, so the gap between what critics
# reward and what audiences watch is plain. The keep argument is named genres_keep
# so it does not clash with the raw genres column inside filter:
plot_ratings <- function(movies, genres_keep){

    movies %>%
        unnest(genre) %>% filter(genre %in% genres_keep) %>%
        group_by(genre) %>%
        summarise(Score = median(imdb_score, na.rm = TRUE), # acclaim
                  Votes = median(imdb_votes, na.rm = TRUE), .groups = "drop") %>% # popularity
        ggplot(aes(Votes, Score)) +
        geom_point(colour = "red", size = 3) +
        ggrepel::geom_text_repel(aes(label = genre), size = 3) + # label each genre
        scale_x_log10(labels = scales::comma) +
        theme_minimal() +
        labs(x = "Median IMDb votes, popularity (log scale)", y = "Median IMDb score, acclaim",
             title = "Acclaim and popularity pull apart",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
