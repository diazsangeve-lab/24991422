# The suppliers worth stocking: mean rating against mean cost, for roasters with a
# real range on offer. Labelled so I can pick value champions and premium names:
plot_supplier_leaderboard <- function(df, min_coffees = 10, n_label = 5){  # define function to plot top suppliers
    plot_data <-  # initialize intermediate dataframe for the plot
        df %>%  # process the main dataframe
        group_by(roaster) %>% # group by supplier
        summarise(Rating = mean(Rating, na.rm = TRUE), # mean rating
                  Cost = mean(Cost_Per_100g, na.rm = TRUE),  # mean cost
                  n = n()) %>%  # how many coffees they offer
        filter(n >= min_coffees, Rating >= 93) %>% # well-stocked, high-scoring suppliers
        mutate(value_score = Rating / Cost) # combined value score, higher is better

    top_rating <-  # isolate roasters with the highest ratings
        plot_data %>%  # use the summarized dataframe
        slice_max(Rating, n = n_label) %>%  # get top n suppliers by rating
        pull(roaster)  # extract as vector

    top_value <-  # isolate roasters with the highest value score
        plot_data %>%  # use the summarized dataframe
        slice_max(value_score, n = n_label) %>%  # get top n suppliers by value
        pull(roaster)  # extract as vector

    cheapest <-  # isolate roasters with the lowest cost
        plot_data %>%  # use the summarized dataframe
        slice_min(Cost, n = n_label) %>%  # get top n suppliers by minimum cost
        pull(roaster)  # extract as vector

    label_these <-  # combine unique roasters to label on the chart
        union(union(top_rating, top_value), cheapest)  # merge all vectors into one list

    plot_data <-  # update the main plotting dataframe
        plot_data %>%  # modify existing rows
        mutate(label = if_else(roaster %in% label_these,  # check if roaster is in label list
                               as.character(roaster),  # set label to roaster name if matched
                               NA_character_))  # otherwise set label to NA

    ggplot(plot_data, aes(x = Cost, y = Rating)) +
        # Creative addition: Topographical contour lines to map the densest market areas
        geom_density_2d(colour = "grey85", linewidth = 0.6) +
        # Refined bubbles with a white border for crisp separation
        geom_point(aes(size = n), fill = "#7B4B2A", colour = "white", shape = 21, alpha = 0.85) +
        ggrepel::geom_text_repel(aes(label = label),
                                 size = 2.6,
                                 na.rm = TRUE,
                                 max.overlaps = 15,
                                 box.padding = 0.5,
                                 segment.color = "grey60") +
        scale_size(range = c(3, 11), guide = "none") +
        theme_minimal() +
        labs(x = "Average cost per 100g (USD)", y = "Average rating",
             title = "Suppliers",
             subtitle = "Supply companies worth stocking, by quality and price.",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 11))
}
