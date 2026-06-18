# Where borrowers default. State default rates ranked, with Texas picked out in red
# so the Director can see at once whether his state stands apart from the rest:
plot_states <- function(df){

    default_rate(df, addr_state) %>%                                                   # default rate per state
        filter(Loans >= 500) %>%                                                       # only well-represented states
        mutate(is_tx = addr_state == "TX") %>%                                         # flag Texas
        ggplot(aes(reorder(addr_state, Default), Default, fill = is_tx)) +
        geom_col() + coord_flip() +                                                    # ranked horizontal bars
        scale_fill_manual(values = c(`FALSE` = "grey", `TRUE` = "red"), guide = "none") +
        theme_minimal() +
        labs(x = NULL, y = "Default rate (%)",
             title = "State default rates, Texas in red",
             subtitle = "Resolved loans, states with at least 500 loans",
             caption = "Data source: Loans and Credit Dataset") +
        theme(axis.text.y = element_text(size = 5),
              plot.title = element_text(size = 11),
              plot.subtitle = element_text(size = 9))                                    # 50 states, so small labels
}
