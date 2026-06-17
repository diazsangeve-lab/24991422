# Rating against cost for each origin region: the top-left is the value sweet spot
# (high rating, low cost). Bubble size is the number of coffees available:
plot_region_value <- function(df){
    df %>%
        filter(Region != "Other") %>%   # drop the unlabelled catch-all
        group_by(Region) %>%         # group by origin region
        summarise(Rating = mean(Rating, na.rm = TRUE),  # mean rating
                  Cost = mean(Cost_Per_100g, na.rm = TRUE),  # mean cost per 100g
                  n = n()) %>%  # supply (number of coffees)
        filter(n >= 20) %>%  # only well-represented regions
        ggplot(aes(Cost, Rating, size = n)) +
        geom_point(aes(colour = Region), alpha = 0.8) +  # caramel bubbles
        ggrepel::geom_text_repel(aes(label = Region), size = 3) +  # label each region
        scale_size(range = c(3, 12), guide = "none") +
        theme_minimal() +
        labs(x = "Average cost per 100g (USD)",
             y = "Average rating",
             title = "Rating vs Cost",
             subtitle = "Average rating per average cost of region sourced.",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 11))
}
