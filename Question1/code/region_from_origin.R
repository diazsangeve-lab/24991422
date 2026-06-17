# Tag each coffee with a broad origin region, read out of the two origin columns.
# 'lookup' is a named vector of regex pattern = "Region label":
region_from_origin <- function(df, lookup){

    blob <- str_c(df$origin_1, " ", df$origin_2)   # search both origin fields together

    Region <-
        blob %>%
        map_chr(function(t){     # for each coffee's origin text
            hit <- lookup[map_lgl(names(lookup),       # which pattern matches
                                  ~str_detect(t, regex(.x, ignore_case = TRUE)))]
            if(length(hit) == 0) "Other" else unname(hit[[1]])   # default to Other if none match
        })

    df %>% mutate(Region = Region)      # add the region label
}
