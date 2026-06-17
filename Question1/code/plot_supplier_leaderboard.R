# The suppliers worth stocking: mean rating against mean cost, for roasters with a
# real range on offer. Labelled so we can pick value champions and premium names:
plot_supplier_leaderboard <- function(df, min_coffees = 10, n_label = 5){
    plot_data <-
        df %>%
        group_by(roaster) %>%                       # group by supplier
        summarise(Rating = mean(Rating, na.rm = TRUE), # mean rating
                  Cost   = mean(Cost_Per_100g, na.rm = TRUE),  # mean cost
                  n      = n()) %>%              # how many coffees they offer
    filter(n >= min_coffees, Rating >= 93) %>% # well-stocked, high-scoring suppliers
    mutate(value_score = Rating / Cost) # combined value score, higher is better

    top_rating <-
        plot_data %>%
        slice_max(Rating, n = n_label) %>%
        pull(roaster)

    top_value <-
        plot_data %>%
        slice_max(value_score, n = n_label) %>%
        pull(roaster)

    cheapest <-
        plot_data %>%
        slice_min(Cost, n = n_label) %>%
        pull(roaster)

    label_these <-
        union(union(top_rating, top_value), cheapest)

    plot_data <-
        plot_data %>%
        mutate(label = if_else(roaster %in% label_these,
                               as.character(roaster),
                               NA_character_))

    ggplot(plot_data, aes(Cost, Rating, size = n)) +
        geom_point(colour = "#7B4B2A", alpha = 0.8) +                  # espresso bubbles
        ggrepel::geom_text_repel(aes(label = label),
                                 size = 2.6,
                                 na.rm = TRUE,
                                 max.overlaps = 15) +
        scale_size(range = c(3, 11), guide = "none") +
        theme_minimal() +
        labs(x = "Average cost per 100g (USD)", y = "Average rating",
             title = "Suppliers",
             subtitle = "Supply companies worth stocking, by quality and price.",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 11))
}
