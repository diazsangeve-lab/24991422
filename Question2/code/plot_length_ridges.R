# The distribution of name length over time, drawn as one ridge per decade, with
# births used as weights so the curves reflect babies rather than distinct names:
plot_length_ridges <- function(nat){

    nat %>%
        mutate(Len = str_length(Name), Decade = (Year %/% 10) * 10) %>% # letters per name, by decade
        group_by(Decade, Len) %>%
        summarise(Babies = sum(Count), .groups = "drop_last") %>%
        mutate(Share = Babies / sum(Babies)) %>%  # length distribution within the decade
        ungroup() %>%
        ggplot(aes(x = Len, y = factor(Decade), height = Share, group = Decade, fill = Decade)) +
        ggridges::geom_ridgeline(scale = 3, colour = "white", alpha = 0.9) + # one ridge per decade
        scale_fill_viridis_c(option = "mako", guide = "none") +
        scale_x_continuous(breaks = 2:12) +
        theme_minimal() +
        labs(x = "Letters in the name", y = "",
             title = "Name length rose and then edged back",
             subtitle = "Distribution of name length by decade, births weighted",
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
