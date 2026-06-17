# Pull a usable first name out of a messy artist or character string. I drop any
# collaborators, anything after a slash or bracket, and keep the leading word:
first_name <- function(x){
    x %>%
        str_remove(regex("\\s+(feat|featuring|with|x|vs|&|,).*", ignore_case = TRUE)) %>%  # drop collaborators
        str_remove("[/(].*") %>% # drop "/ Tin Man" or "(voice)"
        str_squish() %>% # tidy whitespace
        word(1) %>% # the first word
        (\(w) if_else(str_detect(w, "^[A-Za-z]{3,}$"), str_to_title(w), NA_character_))()  # letters only, 3+
}