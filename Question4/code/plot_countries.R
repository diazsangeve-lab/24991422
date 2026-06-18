# Where the films are made. The production-country codes counted across all movies,
# a film with two origins counting for each, top n shown:
plot_countries <- function(movies, n = 10){

    movies %>%
        unnest(country) %>% # one row per film-country
        count(country, name = "Movies") %>%
        slice_max(Movies, n = n) %>%
        ggplot(aes(x = reorder(country, Movies), y = Movies)) +
        geom_segment(aes(xend = reorder(country, Movies), y = 0, yend = Movies, colour = Movies),
                     linewidth = 2, lineend = "round") +
        scale_colour_gradientn(colours = c("brown1", "brown3", "brown4"), guide = "none") +
        coord_polar("x") +
        geom_text(aes(label = paste0(country, "  ", Movies), y = max(Movies) * 1.1),
                  colour = "black", size = 3, fontface = "bold",
                  hjust = "outward", vjust = "outward") +
        theme_minimal() +
        theme(axis.text.x = element_blank(),   # remove original country names
              axis.line.y = element_blank(),  # kill the y-axis line
              axis.ticks.y = element_blank(), # kill any tick marks
              axis.text.y = element_blank(),  # kill y-axis labels (radial distances)
              panel.grid.major.y = element_blank() # kill the vertical (radial) grid line
        ) +
        labs(x = NULL, y = "Movies on Netflix (up to 2022)",
             title = "Where Netflix's movies are made",
             caption = "Data source: Netflix Titles Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}