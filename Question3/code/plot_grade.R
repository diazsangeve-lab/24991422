# The headline on grading. Default rate by LC grade, which should climb steadily
# from A to G if the grade is doing its job of separating good risks from bad:
plot_grade <- function(df){

    default_rate(df, grade) %>%                                                        # default rate per grade
        ggplot(aes(grade, Default)) +
        geom_col(fill = "steelblue4") +                                                   # one bar per grade
        geom_text(aes(label = sprintf("%.0f%%", Default)), vjust = -0.4, size = 3) +   # label each bar
        theme_minimal() +
        labs(x = "Lending Club credit grade", y = "Default rate (%)",
             title = "Default rate climbs steeply down the grade scale",
             subtitle = "Resolved loans, fully paid against charged off",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
