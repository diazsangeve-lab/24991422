# Count how many of the student-survey flavour words show up in each coffee's
# reviews. The keyword vector is passed in so the function stays general:
score_keywords <- function(df, keywords){

    hits <-
        keywords %>%    # the student survey words
        map_dfc(~str_detect(str_to_lower(df$desc_all), .x)) %>%   # TRUE/FALSE column per keyword
        rowSums()               # how many distinct words each coffee hits

    df %>% mutate(Student_Score = hits)        # add the match score
}
