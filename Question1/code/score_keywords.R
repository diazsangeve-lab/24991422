# Count how many of the data-derived indicator words appear in each coffee's
# reviews. The keyword vector is passed in so the function stays general and can
# be reused on a training split for held-out validation:
score_keywords <- function(df, keywords){

    hits <-
        keywords %>%# the derived indicator words
        map_dfc(~str_detect(str_to_lower(df$desc_all), .x)) %>%  # TRUE/FALSE column per word
        rowSums(na.rm = TRUE)  # how many distinct words each coffee hits

    df %>% mutate(Student_Score = hits)  # add the match score
}
