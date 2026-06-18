# Who fills the catalogue. The actors credited on the most Netflix films, a quick
# read on which film industry the platform leans on:
top_actors <- function(credits, movies, n = 8){

    credits %>%
        filter(role == "ACTOR", id %in% movies$id) %>% # actors on the in-scope films
        count(name, name = "Films") %>%
        slice_max(Films, n = n, with_ties = FALSE) %>%
        arrange(desc(Films))
}
