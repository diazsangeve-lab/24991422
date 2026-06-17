# The persistence time series. I average the three horizons into one line per
# gender and mark 1990, so the reader can judge the agency's claim at a glance.
plot_persistence <- function(pers){
    pers %>%
        group_by(Gender, Year) %>%  # collapse the horizons
        summarise(Spearman = mean(Spearman), .groups = "drop") %>%
        mutate(Gender = recode(Gender, M = "Boys", F = "Girls")) %>%
        ggplot(aes(Year, Spearman, colour = Gender)) +
        geom_vline(xintercept = 1990, linetype = "dashed", colour = "grey60") + # the 1990 line
        geom_line(linewidth = 0.7) + # the persistence trend
        geom_smooth(se = FALSE, linewidth = 0.4, linetype = "dotted") + # gentle trend guide
        scale_colour_manual(values = c(Boys = "skyblue3", Girls = "hotpink3")) +
        theme_minimal() +
        labs(x = "", y = "Rank persistence (Spearman)", colour = "",
             title = "Do popular names persist?",
             subtitle = "Each year's top 25 names against the next three years, averaged")
}