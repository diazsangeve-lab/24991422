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
        ggplot(aes(x = Votes, y = Score)) +
        geom_label(aes(label = genre, fill = Score, size = Votes),
                   colour = "white", fontface = "bold", label.size = 0,
                   alpha = 0.85, show.legend = FALSE) +
        scale_fill_gradient(low = "steelblue", high = "red3") +
        scale_size_continuous(range = c(2.5, 6)) +
        scale_x_log10(
            labels = scales::comma,
            expand = expansion(mult = c(0.15, 0.15))   # breathe room left and right
        ) +
        scale_y_continuous(
            expand = expansion(mult = c(0.1, 0.1))     # breathe room top and bottom
        ) +
        coord_cartesian(clip = "off") +                 # don't clip labels at borders
        theme_minimal() +
        labs(x = "Median IMDb votes, popularity (log scale)", y = "Median IMDb score, acclaim",
             title = "Acclaim and popularity pull apart",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11, face = "bold"),
              plot.subtitle = element_text(size = 9))
}