# Distribution view: how a handful of standout names rise, peak and fall. I turn
# each name's yearly count into a filled area, stacked into small rows, so the
# shape and timing of each wave is easy to compare. Names come from the surges.
plot_name_distribution <- function(tot, names, from = 1940){
    tot %>%
        filter(Name %in% names, Year >= from) %>%
        group_by(Name) %>% mutate(Share = Count / max(Count)) %>%
        ungroup() %>%  # scale each to its own peak
        mutate(Name = fct_reorder(Name,
                                  Year,
                                  .fun = function(y) y[which.max(Share[order(y)])])) %>%
        ggplot(aes(Year, Share)) +
        geom_area(aes(fill = Name), alpha = 0.85) + # the wave for each name
        facet_grid(rows = vars(Name), switch = "y") + # one row per name
        scale_fill_viridis_d(guide = "none") +
        theme_minimal() +
        theme(strip.text.y.left = element_text(angle = 0), axis.text.y = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(x = "", y = "",
             title = "The life of a name",
             subtitle = "Each name scaled to its own peak, showing how fast it rose and fell",
             caption = "Data source: US Baby Names Dataset") +
    theme(plot.title = element_text(size = 11),
          plot.subtitle = element_text(size = 9))
}
