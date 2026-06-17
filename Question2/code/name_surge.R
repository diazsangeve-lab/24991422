# Measure how much a single name surged around a given year. I compare the mean
# count in the three years before to the peak in the years from the event on:
name_surge <- function(tot, name, year, back = 3, fwd = 4){
    s <- tot %>% filter(Name == name) # this name's history

    pre <- s %>% filter(Year >= year - back, Year < year) %>% # the years just before
        summarise(m = mean(Count)) %>% pull(m)
    pre <- if_else(is.na(pre), 0, pre) # no prior history counts as zero

    post <- s %>% filter(Year >= year, Year <= year + fwd) # the event year and after
    if(nrow(post) == 0)
        return(tibble(Pre = round(pre), Post = 0, Peak_Year = year, Ratio = 0))

    tibble(Pre       = round(pre), # baseline before
           Post      = max(post$Count), # peak after the event
           Peak_Year = post$Year[which.max(post$Count)], # when that peak fell
           Ratio     = round(max(post$Count) / (pre + 1), 1)) # how many times bigger
}