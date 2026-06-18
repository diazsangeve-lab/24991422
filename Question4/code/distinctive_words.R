# Textual analysis of the descriptions. For each genre I find the words most
# over-represented in its films' descriptions relative to the whole catalogue, a
# simple lift measure, so the language each genre reaches for becomes visible.
distinctive_words <- function(movies, genres_keep, min_count = 8, n = 6){

    stop <- c("the","a","an","and","of","to","in","is","it","with","for","on","at","by","from",   # generic words
              "this","that","his","her","their","its","when","while","who","into","life","story",
              "world","find","finds","must","they","them","than","over","back","new","young","old",
              "two","one","family","love","man","woman","after","about")

    word_movie <- movies %>%
        distinct(id, description) %>% # one description per film
        mutate(word = str_extract_all(str_to_lower(description), "[a-z]{4,}")) %>% # words of four letters or more
        unnest(word) %>% filter(!word %in% stop) %>% distinct(id, word) # count each word once per film

    N       <- n_distinct(word_movie$id) # films with a description
    overall <- word_movie %>% count(word, name = "all") # films using each word, catalogue-wide

    movie_genre <- movies %>% select(id, genre) %>% unnest(genre) %>% distinct(id, genre) # film-genre pairs
    genre_n     <- movie_genre %>% count(genre, name = "gn") # films in each genre

    word_movie %>%
        inner_join(movie_genre, by = "id") %>% # tag each word with the film's genres
        filter(genre %in% genres_keep) %>%
        count(genre, word, name = "g") %>% # films in genre using the word
        left_join(genre_n, by = "genre") %>%
        left_join(overall, by = "word") %>%
        filter(g >= min_count) %>% # ignore rare words
        mutate(lift = (g / gn) / (all / N)) %>% # over-representation vs the catalogue
        group_by(genre) %>% slice_max(lift, n = n, with_ties = FALSE) %>% ungroup() %>%
        arrange(genre, desc(lift))
}
