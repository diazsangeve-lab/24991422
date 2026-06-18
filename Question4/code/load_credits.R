# Read the cast and crew file, one row per person per title, used to see who fills
# the catalogue:
load_credits <- function(path = "data/netflix/credits.rds"){

    read_rds(path) # person_id, id, name, character, role
}
