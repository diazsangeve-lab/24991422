# Map the coffee origin regions on a world map, highlighting the sourcing
# belt in the palette's core brown.
plot_origin_map <- function(df){

    # Extract the unique, cleaned regions from the dataset
    origins <- df %>%
        filter(Region != "Other", !is.na(Region)) %>%
        distinct(Region) %>%
        # Hawaii maps to the USA in standard map datasets
        mutate(region_map = case_when(
            Region == "Hawaii" ~ "USA",
            TRUE ~ Region
        )) %>%
        pull(region_map)

    # Retrieve the polygon data for the world map
    world_map <- map_data("world")

    # Plot the map, filling the origin countries with the brand brown
    world_map %>%
        mutate(is_coffee_origin = region %in% origins) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = is_coffee_origin), colour = "white", linewidth = 0.2) +
        scale_fill_manual(values = c(`FALSE` = "grey90", `TRUE` = "#7B4B2A"), guide = "none") +
        coord_fixed(1.3) + # Standard projection ratio
        theme_void() +     # Strips out axes, grids, and background for a clean map
        labs(title = "Global Coffee Sourcing",
             subtitle = "Regions where the highest-rated coffees are sourced",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              plot.caption = element_text(size = 9, colour = "grey40", hjust = 0.5))
}