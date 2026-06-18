# Read a streaming catalogue (Netflix or HBO titles.rds, which share a layout) and
# parse the two list-style text columns into proper list columns that can unnest. The
# genres read as lowercase words and the production countries as two-letter codes:
load_streaming <- function(path){

    read_rds(path) %>% # the IMDb-sourced titles
        mutate(genre = str_extract_all(genres, "[a-z]+"), # "['drama','comedy']" -> c("drama","comedy")
               country = str_extract_all(production_countries, "[A-Z]{2}")) # "['US','GB']" -> c("US","GB")
}
