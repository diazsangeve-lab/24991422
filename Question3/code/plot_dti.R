# The debt-to-income cap visual. Default rate across DTI bands, with reference lines
# at the tolerance levels the Director might pick, so the cap can be read straight
# off the curve. Bands are ordered so the line reads left to right:
plot_dti <- function(df){

    df %>%
        filter(!is.na(dti)) %>% # drop the trimmed codes
        mutate(band = cut(dti, c(0, 10, 15, 20, 25, 30, 35, 40, 60), right = FALSE, # even DTI bands
                          labels = c("0-10","10-15","15-20","20-25","25-30","30-35","35-40","40+"))) %>%
        group_by(band) %>% # keep band order for the line
        summarise(Default = mean(default) * 100, .groups = "drop") %>%
        ggplot(aes(x = band, y = Default, group = 1)) +
        # Creative addition: Shaded area underneath the trend to emphasize rising risk
        geom_area(fill = "steelblue4", alpha = 0.3) +
        # Kept the line and points for exact reading
        geom_line(colour = "steelblue4", linewidth = 1) +
        geom_point(colour = "steelblue4", size = 2.5) +
        geom_hline(yintercept = c(20, 25), linetype = "dashed", colour = "grey60") + # tolerance reference lines
        theme_minimal() +
        labs(x = "Debt-to-income band", y = "Default rate (%)",
             title = "Default rises smoothly with debt-to-income",
             subtitle = "Dashed lines mark the 20 and 25 percent tolerance levels",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))

}
