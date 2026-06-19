# How concentrated naming is each year, measured by the share of births held by the
# ten most popular names, with a fitted trend showing the long spread outward:
plot_name_concentration <- function(tot){

    tot %>%
        group_by(Year) %>%
        mutate(rank = rank(-Count, ties.method = "first"), # rank names within the year
               total = sum(Count)) %>%
        filter(rank <= 10) %>% # keep the ten biggest
        summarise(Top10 = sum(Count) / first(total), .groups = "drop") %>% # their share of births
        ggplot(aes(Year, Top10)) +
        geom_point(colour = "grey55", size = 1.4) +
        geom_smooth(method = "lm", colour = "hotpink3", fill = "hotpink3", alpha = 0.15) + # fitted decline
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        labs(x = "", y = "Share of births in the top 10 names",
             title = "Naming has spread out over a century",
             subtitle = "Concentration in the ten most popular names each year, with a fitted trend",
             caption = "Data source: US Baby Names Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
