# How often two genres share a film, as a conditional heatmap. Each cell is the
# probability that a film tagged with the row genre is also tagged with the column
# genre, which exposes drama as the hub the others attach to:
plot_genre_cooccur <- function(movies, genres_keep){

    pairs <- movies %>%
        select(id, genre) %>% unnest(genre) %>%
        filter(genre %in% genres_keep)  # in-scope genre tags per film

    pairs %>%
        inner_join(pairs, by = "id", relationship = "many-to-many") %>%   # every genre pair on a film
        count(genre.x, genre.y) %>%
        group_by(genre.x) %>%
        mutate(Share = n / n[genre.x == genre.y]) %>%  # P(column genre | row genre)
        ungroup() %>%
        ggplot(aes(genre.x, genre.y, fill = Share)) +
        geom_tile(colour = "white", linewidth = 0.3) +
        geom_text(aes(label = scales::percent(Share, accuracy = 1)), size = 2.4, colour = "grey85") +
        scale_fill_viridis_c(option = "cividis", labels = scales::percent, guide = "none") +
        theme_minimal() +
        labs(x = "", y = "",
             title = "Which genres travel together",
             subtitle = "Chance a film's row genre also carries the column genre",
             caption = "Data source: Netflix Movies Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9),
              axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
}
