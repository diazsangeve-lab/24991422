# Average rating by roast level, to show which roasts drink best:
plot_roast <- function(df){
    df %>%
        filter(!is.na(roast)) %>%    # drop coffees with no roast level
        group_by(roast) %>%      # group by roast strength
        summarise(Rating = mean(Rating, na.rm = TRUE), n = n()) %>% # mean rating and count per roast
        filter(n >= 5) %>%     # drop tiny groups
        mutate(roast = fct_reorder(roast, Rating)) %>%    # order bars by rating
        ggplot(aes(roast, Rating)) +
        geom_col(aes(fill = Rating)) +   # fill mapped to numeric rating
        scale_fill_gradient(low = "coral4", high = "cornsilk") + # espresso to cream
        geom_text(aes(label = round(Rating, 1)), hjust = -0.15, size = 3) +  # rating label on each bar
        coord_flip(ylim = c(88, 95)) +      # flip for readability
        theme_minimal() +
        labs(x = "",
             y = "Average rating",
             title = "Roast Ratings",
             subtitle = "Chart of the ratings for each level of roast",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 11))
}
