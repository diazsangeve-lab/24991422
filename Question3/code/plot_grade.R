# The headline on grading. Default rate by LC grade, which should climb steadily
# from A to G if the grade is doing its job of separating good risks from bad:
plot_grade <- function(df){

    default_rate(df, grade) %>% # default rate per grade
        ggplot(aes(x = grade, y = Default)) +
        geom_segment(aes(xend = grade, y = 0, yend = Default), color = "steelblue4", linewidth = 1) +
        geom_point(color = "steelblue4", size = 3) +
        geom_text(aes(label = sprintf("%.0f%%", Default)), vjust = -1.2, size = 3) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        theme_minimal() +
        labs(x = "Lending Club credit grade", y = "Default rate (%)",
             title = "Default rate climbs steeply down the grade scale",
             subtitle = "Resolved loans, fully paid against charged off",
             caption = "Data source: Loans and Credit Dataset") +
        theme(plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))
}
