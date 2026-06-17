# Roll the state-level baby names file up to a national count per name, year and
# gender. The file is large, so I aggregate straight after reading it:
collate_names <- function(path = "data/US_Baby_names/Baby_Names_By_US_State.rds"){
    read_rds(path) %>% # the big state-level file
        group_by(Year, Gender, Name) %>% # one row per name-year-gender
        summarise(Count = sum(Count), .groups = "drop") # national count across all states
}