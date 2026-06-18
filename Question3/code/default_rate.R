# The one workhorse summary used throughout. Grouping the resolved loans by
# variable that is passed in and return the default rate as a percentage alongside the
# number of loans behind it, riskiest group first:
default_rate <- function(df, ...){

    df %>%
        group_by(...) %>% # group by the chosen field(s)
        summarise(Default = mean(default) * 100, # default rate as a percentage
                  Loans   = n(), .groups = "drop") %>% # loans behind the estimate
        arrange(desc(Default))  # riskiest group at the top
}
