# Default rate in every grade and debt-to-income cell, as a heatmap. Reading down
# the grades moves default far more than reading across the debt bands, which puts
# the two factors on the same canvas and shows grade dominating:
plot_grade_dti_heat <- function(loans){

    loans %>%
        filter(!is.na(dti)) %>%
        mutate(DTI_band = cut(dti, c(0, 10, 15, 20, 25, 30, 40, 60), right = FALSE,
                              labels = c("0-10","10-15","15-20","20-25","25-30","30-40","40+"))) %>%
        filter(!is.na(DTI_band)) %>%
        group_by(grade, DTI_band) %>%
        summarise(Default = mean(default) * 100, n = n(), .groups = "drop") %>%
        filter(n >= 100) %>%   # drop thinly populated cells
        ggplot(aes(DTI_band, grade, fill = Default)) +
        geom_tile(colour = "white", linewidth = 0.3) +
        geom_text(aes(label = sprintf("%.0f", Default)), size = 2.6, colour = "grey15") +
        scale_fill_viridis_c(option = "inferno") +
        scale_y_discrete(limits = rev) +  # A at the top
        theme_minimal() +
        labs(x = "Debt-to-income band", y = "Credit grade", fill = "Default %",
             title = "Grade and debt stack, but grade dominates",
             subtitle = "Default rate in each grade and debt-to-income cell",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9),
              panel.grid = element_blank())
}
