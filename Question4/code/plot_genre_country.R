# What each country makes. A heat map of the share of a country's films that carry
# each genre, so the national signatures stand out at a glance. genres_keep avoids
# clashing with the raw genres column inside filter:
plot_genre_country <- function(movies, countries, genres_keep){

    movies %>%
        unnest(country) %>% filter(country %in% countries) %>% # keep the major producers
        add_count(country, name = "country_n") %>%  # films per country
        unnest(genre) %>% filter(genre %in% genres_keep) %>% # keep the major genres
        count(country, genre, country_n, name = "g") %>%
        mutate(Share = g / country_n * 100) %>%   # % of country's films in the genre
        ggplot(aes(genre, country, fill = Share)) +
        geom_tile() +
        geom_text(aes(label = round(Share)), size = 2.6) +  # the share, in each cell
        scale_fill_gradient(low = "white", high = "red3") +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = "% of films",
             title = "What each country makes",
             caption = "Data source: Netflix Titles Dataset") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
