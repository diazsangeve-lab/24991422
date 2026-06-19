# The distribution of interest rate within each grade, drawn as a violin with a
# boxplot inside. The bands barely overlap, which shows the price is set almost
# entirely by the grade the lender assigns:
plot_rate_violin <- function(loans){

    loans %>%
        ggplot(aes(grade, int_rate, fill = grade)) +
        geom_violin(colour = NA, alpha = 0.5, scale = "width") +  # rate spread within each grade
        geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white") +
        scale_fill_viridis_d(guide = "none") +
        theme_minimal() +
        labs(x = "Credit grade", y = "Interest rate (%)",
             title = "The grade sets the price almost on its own",
             subtitle = "Distribution of interest rate within each grade",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
