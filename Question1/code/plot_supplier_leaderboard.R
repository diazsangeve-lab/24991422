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

    ggplot(plot_data, aes(Cost, Rating, size = n)) +  # setup main plot aesthetics
        geom_point(colour = "#7B4B2A", alpha = 0.8) + # espresso bubbles
        ggrepel::geom_text_repel(aes(label = label),  # add non-overlapping labels
                                 size = 2.6,  # set label text size
                                 na.rm = TRUE,  # ignore missing labels
                                 max.overlaps = 15) +  # maximum allowed overlaps
        scale_size(range = c(3, 11), guide = "none") +  # adjust bubble sizing and remove legend
        theme_minimal() +  # apply minimal charting theme
        labs(x = "Average cost per 100g (USD)", y = "Average rating",  # configure axis titles
             title = "Suppliers",  # set plot title
             subtitle = "Supply companies worth stocking, by quality and price.",  # set plot subtitle
             caption = "Data source: Coffee Dataset") +  # set plot caption
        theme(plot.title = element_text(size = 14),  # format the title size
              plot.subtitle = element_text(size = 11))  # format the subtitle size
}