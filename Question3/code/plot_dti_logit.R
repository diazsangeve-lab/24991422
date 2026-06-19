# A logistic fit of default on debt-to-income, split by loan term. The fitted
# curves show default climbing with debt and the longer term sitting above the
# shorter at every level:
plot_dti_logit <- function(loans){

    loans %>%
        filter(!is.na(dti)) %>%
        ggplot(aes(dti, default, colour = term, fill = term)) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"), # fitted default probability
                    linewidth = 0.8, alpha = 0.15) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(0, 40)) + # focus on the populated range
        scale_colour_manual(values = c("36 months" = "steelblue4", "60 months" = "tomato3"),
                            aesthetics = c("colour", "fill")) +
        theme_minimal() +
        labs(x = "Debt-to-income ratio", y = "Fitted probability of default",
             colour = "Term", fill = "Term",
             title = "Default rises with debt, and the long term sits above the short",
             subtitle = "Logistic fit of default on debt-to-income, by loan term",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11), plot.subtitle = element_text(size = 9))
}
