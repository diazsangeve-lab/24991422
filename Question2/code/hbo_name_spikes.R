# Did screen characters move the cradle? I read the first name of every HBO
# character, attach its title's release year and audience score, and test for a
# baby-name surge afterwards. Names ride out of popular shows and films.
hbo_name_spikes <- function(nat, titles, credits, min_post = 200, max_pre = 150, min_ratio = 3){
    tot <- national_totals(nat)

    chars <-
        credits %>%
        mutate(Name = first_name(character)) %>%
        filter(!is.na(Name)) %>%
        left_join(titles %>%
                      select(id, title, release_year, type, tmdb_score), by = "id") %>%
        filter(!is.na(release_year), release_year >= 1958, release_year <= 2012) %>% # need a window in the baby data
        distinct(Name, title, .keep_all = TRUE)

    chars %>%
        mutate(spk = map2(Name, release_year, ~name_surge(tot, .x, .y))) %>%  # baby-name response
        unnest(spk) %>%
        filter(Post >= min_post, Pre <= max_pre, Post >= min_ratio * (Pre + 5)) %>%  # a real surge
        group_by(Name) %>%
        slice_max(tmdb_score, n = 1, with_ties = FALSE) %>% ungroup() %>% # best-known title per name
        arrange(desc(Ratio)) # strongest links first
}
