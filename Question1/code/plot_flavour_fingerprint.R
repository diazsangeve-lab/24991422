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
        ggplot(aes(reorder(Word, Gap), Share, fill = Set)) +   # order by gap, so the standouts lead
        geom_col(position = "dodge") +       # top vs all, side by side
        coord_flip() +  # rotate the chart for horizontal bars
        scale_y_continuous(labels = scales::percent) +  # format y-axis as percentage
        scale_fill_manual(values = c(Top = "burlywood4", All = "darksalmon")) +  # assign custom colors to the bars
        theme_minimal() +  # apply minimal charting theme
        labs(x = "",  # remove the x-axis title
             y = "Reviews mentioning the word",  # set the y-axis title
             title = "What the best coffees taste like",  # set the main title
             subtitle = "Words far more common in 95-plus coffees than across all",  # set the subtitle
             caption = "Data source: Coffee Dataset") +  # set the caption
        theme(plot.title = element_text(size = 14),  # adjust the title text size
              plot.subtitle = element_text(size = 11))  # adjust the subtitle text size
}