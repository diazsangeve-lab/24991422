# Load the coffee csv, forcing UTF-8 so the curly quotes and accented characters
# that break in Excel read in cleanly, then transliterate them to plain ASCII:
load_coffee <- function(path = "data/Coffee/Coffee.csv"){ # define function to load the dataset

    readr::read_csv(path,  # load csv from the provided path
                    locale = readr::locale(encoding = "UTF-8"),  # read the strange characters properly
                    show_col_types = FALSE) %>%    # silence the column-spec message
        mutate(across(where(is.character),    # for every text column
                      ~stringi::stri_trans_general(., "Latin-ASCII"))) %>%  # smart quotes -> plain quotes etc.
        mutate(desc_all = str_c(desc_1, " ", desc_2, " ", desc_3))     # one combined review field to search
}
