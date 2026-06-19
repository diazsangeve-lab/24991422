# Every film binned by its IMDb score against its audience size, a hexbin density
# with a fitted line. It shows where the catalogue actually sits, and a faint
# upward tilt that runs against the genre-level split between acclaim and reach:
plot_score_hex <- function(movies){

    movies %>%
        filter(!is.na(imdb_score), !is.na(imdb_votes), imdb_votes > 0) %>%
        ggplot(aes(imdb_votes, imdb_score)) +
        geom_hex(bins = 30) +                                  # density of films in score-votes space
        geom_smooth(method = "lm", colour = "red", se = FALSE, linewidth = 0.7) +  # overall tilt
        scale_x_log10(labels = scales::comma) +
        scale_fill_viridis_c(option = "plasma", trans = "log", labels = scales::comma) +
        theme_minimal() +
        labs(x = "IMDb votes, popularity (log scale)", y = "IMDb score, acclaim", fill = "Films",
             title = "Where the catalogue actually sits on acclaim and reach",
             subtitle = "Every film binned by its score and its audience size",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
