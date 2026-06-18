# What sets the very best coffees apart: share of reviews mentioning each flavour
# word, comparing the top coffees (rating 95+) against the whole catalogue:
plot_flavour_fingerprint <- function(df, words, top_cut = 95){
    share <- function(sub) map_dbl(words, ~mean(str_detect(str_to_lower(sub$desc_all), .x),na.rm = TRUE))  # helper function calculating word frequency
    tibble(Word = words,  # each flavour word
           Top  = share(df %>% filter(Rating >= top_cut)),  # share among the best coffees
           All  = share(df)) %>%   # share across all coffees
        mutate(Gap = Top - All) %>%   # how much more the best use it
        arrange(desc(Gap)) %>% slice_head(n = 8) %>%  # the most distinguishing words
        pivot_longer(c(Top, All), names_to = "Set", values_to = "Share") %>%  # pivot data for grouped bar chart
        ggplot(aes(x = reorder(Word, Gap), y = Share)) +
        geom_line(aes(group = Word), color = "gray80", linewidth = 1.2) +       # Connects the two points
        geom_point(aes(color = Set), size = 3.5) +                              # The Top vs All markers
        coord_flip() +
        scale_y_continuous(labels = scales::percent) +
        scale_color_manual(values = c(Top = "burlywood4", All = "darksalmon")) +
        theme_minimal() +
        labs(x = "",
             y = "Reviews mentioning the word",
             title = "What the best coffees taste like",
             subtitle = "Words far more common in 95-plus coffees than across all",
             caption = "Data source: Coffee Dataset") +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 11))
}
