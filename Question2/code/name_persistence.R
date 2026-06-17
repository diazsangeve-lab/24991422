# Persistence of the top names. For each year I take that gender's 25 most
# popular names, then compare their ranking to the same names at horizons of 1, 2
# and 3 years ahead. A Spearman near 1 means the order held, lower means it moved.
name_persistence <- function(nat, gender, topn = 25, horizons = 1:3){
    sub <-
        nat %>%
        filter(Gender == gender) %>% # one gender at a time
        group_by(Year) %>%  # within each year
        mutate(rank = rank(-Count, ties.method = "first")) %>% # rank names, 1 = most popular
        ungroup()

    years <- sort(unique(sub$Year)) # all years present

    map_dfr(horizons, function(h){ # for each look-ahead horizon
        yy <- years[years + h <= max(years)] # years with a full horizon
        map_dfr(yy, function(y){   # walk the base years
            top <-
                sub %>%
                filter(Year == y, rank <= topn) %>%
                select(Name, rank0 = rank) # top 25 now

            fut <-
                sub %>%
                filter(Year == y + h) %>%
                select(Name, rankh = rank) # ranks h years on

            j <- top %>%
                left_join(fut, by = "Name") %>% # line up the same names
                mutate(rankh = replace_na(rankh, max(fut$rankh) + 1)) # names that fell away get worst rank
            tibble(Year = y, Gender = gender, Horizon = h,
                   Spearman = cor(j$rank0, j$rankh, method = "spearman"))  # rank similarity of the two tables
        })
    })
}