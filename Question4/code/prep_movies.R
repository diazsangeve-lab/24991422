# The brief asks about movies up to 2022, so keep films released in that window and
# leave the shows aside for the movie-specific cuts:
prep_movies <- function(df){

    df %>%
        filter(type == "MOVIE", release_year <= 2022) # films only, up to 2022
}
