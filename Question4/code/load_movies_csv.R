# Read the supplementary Netflix movies file, the classic catalogue export that
# carries the age rating and the listed duration the titles file does not:
load_movies_csv <- function(path = "data/netflix/netflix_movies.csv"){

    read_csv(path, show_col_types = FALSE) # show_id, country, rating, duration, listed_in
}
