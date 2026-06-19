# A heatmap of how often each opening letter starts a name, by decade. Fashion
# operates even at the level of a single initial, not just the whole name:
plot_initial_heat <- function(tot){

    tot %>%
        mutate(Initial = str_sub(Name, 1, 1), # first letter of the name
               Decade  = (Year %/% 10) * 10) %>% # bucket years into decades
        group_by(Decade, Initial) %>%
        summarise(Babies = sum(Count), .groups = "drop_last") %>%
        mutate(Share = Babies / sum(Babies)) %>%  # share within each decade
        ungroup() %>%
        ggplot(aes(factor(Decade), Initial, fill = Share)) +
        geom_tile(colour = "white", linewidth = 0.2) + # one cell per letter-decade
        scale_fill_viridis_c(option = "magma", labels = scales::percent) +
        scale_y_discrete(limits = rev) +  # A at the top
        theme_minimal() +
        labs(x = "", y = "First letter", fill = "Share of births",
             title = "Which letters open a name, by decade",
             subtitle = "Share of all births whose first name starts with each letter",
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9),
              panel.grid = element_blank())
}
