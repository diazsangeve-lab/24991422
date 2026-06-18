# Deriving the indicator words straight from the reviews.
# Every word in the tasting notes is ranked by how much its presence lifts the
# expert rating (mean rating of coffees that use the word minus those that do not).
# I strip out only standard stop-words, the review platform's own vocabulary, the
# generic coffee-process terms, and the roaster names, then take the strongest n.
derive_keywords <- function(df, n = 12, min_share = 0.02){

    stop_words <- c("the","a","an","and","or","of","to","in","is","it","as","with","for","on","at","by",
                    "from","this","that","these","those","be","are","was","were","has","have","had","not",
                    "but","its","their","his","her","you","your","we","our","they","them","then","than","so",
                    "very","more","most","much","also","which","who","what","when","where","while","into",
                    "out","up","down","over","under","again","once","here","there","all","any","both","each",
                    "few","other","some","such","only","own","same","too","can","will","just","about","after",
                    "before","non","per")

    # not flavours: the review site's own words plus generic product and process terms
    non_sensory <- c("list","selected","top","star","multi","flavor","flavors","report","rated","review",
                     "reviewed","highest","finest","best","note","notes","hint","hints","evaluated","produced",
                     "coffee","espresso","blend","cup","roast","roasted","roaster","shop","drink","drinking",
                     "quality","high","overall","current","price","priced","value","month","results","sample",
                     "samples","acidity","body","finish","structure","mouthfeel","aroma","toned")

    roaster_tok <- df$roaster %>% str_to_lower() %>% # roaster names appear in some reviews
        str_extract_all("[a-z]{3,}") %>% unlist() %>% unique()

    drop <- unique(c(stop_words, non_sensory, roaster_tok))

    N <- nrow(df); R_tot <- sum(df$Rating)   # totals for the lift calculation

    df %>%
        transmute(Rating, word = str_extract_all(str_to_lower(desc_all), "[a-z]{3,}")) %>%
        mutate(.id = row_number()) %>%
        unnest(word) %>%
        distinct(.id, Rating, word) %>%  # count each word once per coffee
        filter(!word %in% drop) %>%
        group_by(word) %>%
        summarise(k = n(), s = sum(Rating), .groups = "drop") %>%  # coffees with the word, and their rating sum
        filter(k >= min_share * N) %>%     # ignore rare words
        mutate(lift = s / k - (R_tot - s) / (N - k)) %>%     # mean rating with the word minus without
        slice_max(lift, n = n) %>%  # the strongest indicators
        arrange(desc(lift)) %>%
        pull(word)
}
