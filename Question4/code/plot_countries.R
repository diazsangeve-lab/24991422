# Where the films are made. The production-country codes counted across all movies,
# a film with two origins counting for each, top n shown:
plot_countries <- function(movies, n = 10){

    movies %>%
        unnest(country) %>% # one row per film-country
        count(country, name = "Movies") %>%
        slice_max(Movies, n = n) %>%
        ggplot(aes(reorder(country, Movies), Movies)) +
        geom_col(fill = "red3") + coord_flip() +  # ranked horizontal bars
        geom_text(aes(label = Movies), hjust = -0.2, size = 3) +
        theme_minimal() +
        labs(x = NULL, y = "Movies on Netflix (up to 2022)",
             title = "Where Netflix's movies are made",
             caption = "Data source: Netflix Titles Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
