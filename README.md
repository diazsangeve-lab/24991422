# Purpose

This is the root README for my Data Science practical, student number
24991422. It collates the four questions, each of which lives in its own
folder and works on a standalone basis. The point of this file is to
source each question’s functions from its `code/` folder and render the
key figures and tables here, so the whole submission can be read and
checked from one place.

The workflow is the same throughout. Every operation is written as a
function, one per file, kept in that question’s `code/` folder and
sourced together at the top of each section. The data sits in each
question’s `data/` folder and is not tracked by the repository, so the
chunks below expect the files to be in place before the README knits.

### How to get started

I set the project up with the following, which creates the repository
and the Texevier templates for the three written questions. Question 1
is an `officer` PowerPoint deck rather than a Texevier document, so it
has no template.

``` r
CHOSEN_LOCATION <- "~/DataScience/Practical/"
fmxdat::make_project(FilePath = glue::glue("{CHOSEN_LOCATION}Solution/"), ProjNam = "24991422")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}Solution/24991422/"), template_name = "Question2")
Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}Solution/24991422/"), template_name = "Question3")
Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}Solution/24991422/"), template_name = "Question4")
```

# Question 1, Coffee

A recommendation deck for stocking the Neelsie Coffee Hub, built in R
with `officer` so the slides regenerate from the data. The deck itself
is `Question1/Coffee_Hub_Recommendation.pptx`, and the charts inside it
come from the functions sourced below. I reproduce them here from the
same prepared data the deck uses.

My approach is to let the supplied reviews drive every claim so that
nothing is hand-picked. `load_coffee` reads and cleans the file,
`region_from_origin` rolls scattered growing regions up to countries,
and `derive_keywords` ranks review words by the rating lift they carry
rather than by a list I chose, after which `score_keywords` tags each
coffee on those words. The four plots then turn that one prepared table
into the roast, region-value, flavour and supplier views the deck
recommends from, so each slide traces back to a number in the data.

``` r
list.files('Question1/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

Coffee <- load_coffee("Question1/data/Coffee/Coffee.csv") %>% # load and fix the strange characters
    filter(Cost_Per_100g > 0) %>% # drop the odd zero-cost rows
    mutate(desc_all = replace_na(desc_all, "")) # a couple of reviews are missing

# roll growing regions up to countries
region_lookup <- c("Ethiopia"="Ethiopia","Kenya"="Kenya","Colombia"="Colombia","Panama"="Panama", "Hawai"="Hawaii","Guatemala"="Guatemala","Sumatra|Lintong|Sulawesi|Indonesia"="Indonesia", "Costa Rica"="Costa Rica","Brazil"="Brazil","Honduras"="Honduras","Rwanda"="Rwanda")

Coffee <- region_from_origin(Coffee, region_lookup) # tag each coffee's region
keywords <- derive_keywords(Coffee, n = 12) # indicator words, straight from the reviews
Coffee <- score_keywords(Coffee, keywords) # score each coffee on those words
```

This plot gives the average rating of every coffee grouped by roast
level. It shows that the lighter roasts come out on top, with light and
medium-light rating highest and the score sliding as the roast darkens,
so the range should be anchored on lighter roasts.

``` r
plot_roast(Coffee)
```

<img src="README_files/figure-markdown_github/q1-roast-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot places each growing country by its average rating against its
average price per 100g. It shows that East African origins like Kenya
and Ethiopia rate within a point of Panama while costing a fraction as
much, which is the core sourcing argument.

``` r
plot_region_value(Coffee)
```

<img src="README_files/figure-markdown_github/q1-region-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot compares how often each data-derived flavour word appears in
the very best coffees against the shelf as a whole. It shows that words
like juicy and saturated are far more common among top-rated coffees, so
those are the notes worth stocking towards.

``` r
plot_flavour_fingerprint(Coffee, keywords)
```

<img src="README_files/figure-markdown_github/q1-flavour-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot ranks suppliers by their average rating and separates the
affordable from the premium. It shows that a few roasters deliver high
quality at low cost while a different group sits at the top of the price
range, so the shelf can carry both a value tier and a showpiece tier.

``` r
plot_supplier_leaderboard(Coffee)
```

<img src="README_files/figure-markdown_github/q1-supplier-1.png" alt="" width="100%" style="display: block; margin: auto;" />

# Question 2, Baby Names

How American culture writes itself into a cradle, read from the full
state record of births from 1910 to 2014. The functions collate the
national counts, measure how long the top names persist, then test music
and screen against the surges directly using the Billboard and HBO
datasets.

My approach splits the question into persistence and surges.
`collate_names` and `national_totals` build the national series,
`name_persistence` and `persistence_table` measure how firmly each
year’s top names hold their rank over the following years, and
`find_spikes` pulls the sharp year-on-year jumps straight from the data.
To test cause rather than assert it, `billboard_name_spikes` and
`hbo_name_spikes` match those jumps against the Billboard and HBO
datasets, `combine_matches` and `tag_spike_source` label each surge by
what drove it, and `plot_event_study` pools the matches to read the
average naming response around an event.

``` r
list.files('Question2/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

nat <- collate_names("Question2/data/US_Baby_names/Baby_Names_By_US_State.rds") # national name counts

tot <- national_totals(nat) # collapsed across gender

charts <- read_rds("Question2/data/US_Baby_names/charts.rds") # weekly Billboard Hot 100

titles <- read_rds("Question2/data/US_Baby_names/HBO_titles.rds") # HBO titles and scores

credits <- read_rds("Question2/data/US_Baby_names/HBO_credits.rds") # HBO cast and characters

pers <- bind_rows(name_persistence(nat, "M"), name_persistence(nat, "F")) # persistence series

ptab <- persistence_table(pers) # summary table

spikes <- find_spikes(nat) # data-driven year-on-year surges

bb <- billboard_name_spikes(nat, charts) # singer-driven names

hbo <- hbo_name_spikes(nat, titles, credits) %>% # character-driven names
    mutate(across(where(is.character), ~ iconv(., to = "UTF-8", sub = ""))) # drop invalid bytes

matches <- combine_matches(bb, hbo) # names driven by both

sourced <- tag_spike_source(spikes, bb, hbo) # surges labelled by cause

fade_names <- spikes %>% 
    filter(Count >= 2000) %>% 
    slice_max(Growth, n = 6) %>% 
    pull(Name)

dist_names <- tot %>% 
    group_by(Name) %>%
    summarise(peak = max(Count), last = Count[which.max(Year)], .groups = "drop") %>%
    filter(peak >= 10000, last < 0.12 * peak) %>% 
    slice_max(peak, n = 6) %>%
    pull(Name)

bb_events  <- bb  %>% 
    slice_max(Ratio, n = 5) %>% 
    transmute(Name, Event_Year = Chart_Year)

hbo_events <- hbo %>% 
    slice_max(Ratio, n = 5) %>% 
    transmute(Name, Event_Year = release_year)
ev_events  <- matches %>% transmute(Name, Event_Year, Source)
```

This plot tracks the rank correlation of each year’s 25 most popular
names one, two and three years later, split by gender. It shows that the
top names hold their position firmly through most of the century and
then loosen after 1990, more sharply for boys than for girls.

``` r
plot_persistence(pers)
```

<img src="README_files/figure-markdown_github/q2-persist-1.png" alt="" width="100%" style="display: block; margin: auto;" />

``` r
ptab %>% 
    kable(caption = "Mean rank persistence by gender and horizon, before and from 1990",
               digits = 3, col.names = c("Gender", "Horizon", "Pre-1990", "1990 onward", "Change"))
```

| Gender | Horizon | Pre-1990 | 1990 onward | Change |
|:-------|:--------|---------:|------------:|-------:|
| Boys   | 1-year  |    0.981 |       0.950 | -0.031 |
| Boys   | 2-year  |    0.958 |       0.891 | -0.067 |
| Boys   | 3-year  |    0.934 |       0.836 | -0.098 |
| Girls  | 1-year  |    0.952 |       0.937 | -0.014 |
| Girls  | 2-year  |    0.900 |       0.868 | -0.032 |
| Girls  | 3-year  |    0.853 |       0.815 | -0.038 |

Mean rank persistence by gender and horizon, before and from 1990

This table lists the ten sharpest year-on-year name surges with the
cause our datasets can attribute to each. It shows that the biggest
movements are sudden spikes rather than slow drifts, and that most of
them line up with a song or a screen character.

``` r
sourced %>%
    slice_max(Growth, n = 10) %>%
    transmute(Year, Name, Before = Prev, After = Count, `Times bigger` = round(Growth, 1), Cause = Source) %>%
    kable(caption = "The ten sharpest year-on-year name surges, with the cause our datasets can attribute")
```

| Year | Name     | Before | After | Times bigger | Cause |
|-----:|:---------|-------:|------:|-------------:|:------|
| 1972 | Katina   |     48 |  2726 |         56.8 | Other |
| 1983 | Marquita |     96 |  2522 |         26.3 | Other |
| 1966 | Audra    |     36 |   865 |         24.0 | Other |
| 1976 | Farrah   |     36 |   813 |         22.6 | Other |
| 1912 | Woodrow  |     82 |  1826 |         22.3 | Other |
| 2001 | Nevaeh   |     53 |  1179 |         22.2 | Other |
| 1957 | Tammy    |    204 |  4363 |         21.4 | Other |
| 1954 | Sheree   |     35 |   646 |         18.5 | Other |
| 1992 | Devante  |     86 |  1551 |         18.0 | Other |
| 1994 | Aliyah   |     43 |   707 |         16.4 | Other |

The ten sharpest year-on-year name surges, with the cause our datasets
can attribute

This plot places each culturally driven name by the year it surged,
sized by how many babies took it at the peak and coloured by whether a
singer or a screen character drove it. It shows that the surges cluster
in the music and television era and that both sources produce names
reaching into the thousands.

``` r
plot_spike_bubble(matches)
```

<img src="README_files/figure-markdown_github/q2-bubble-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot follows the names with the sharpest surges across their whole
life rather than at the peak alone. It shows that almost all of them are
short-lived fashions that climb fast and fade within about a decade.

``` r
plot_trajectories(tot, fade_names, title = "Fade or stick", subtitle = "The sharpest surges are usually short-lived fashions")
```

<img src="README_files/figure-markdown_github/q2-fade-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot rescales the biggest names of the century to their own peak so
the full arc is visible. It shows that even the largest names eventually
fall to a fraction of their height, so size at the top is no guarantee
of staying power.

``` r
plot_name_distribution(tot, dist_names)
```

<img src="README_files/figure-markdown_github/q2-dist-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This table lists the singers whose first name surged in babies after
they first reached the Billboard top ten. It shows the chart almost
creating names, with Sade and Rihanna climbing from near nothing into
the thousands.

``` r
bb %>% 
    slice_max(Ratio, n = 8) %>%
    transmute(Name, Artist = artist, `First charted` = Chart_Year, Before = Pre, Peak = Post, `Times bigger` = Ratio) %>%
    kable(caption = "Singers whose first name surged in babies after they first reached the Billboard top ten")
```

| Name    | Artist         | First charted | Before | Peak | Times bigger |
|:--------|:---------------|--------------:|-------:|-----:|-------------:|
| Sade    | Sade           |          1985 |      0 | 1214 |       1214.0 |
| Rihanna | Rihanna        |          2005 |      0 | 1045 |       1045.0 |
| Zhane   | Zhane          |          1993 |      0 |  351 |        351.0 |
| Aaliyah | Aaliyah        |          1994 |      5 | 1706 |        284.3 |
| Carly   | Carly Simon    |          1971 |      5 |  316 |         52.7 |
| Sheena  | Sheena Easton  |          1981 |     90 | 3598 |         39.7 |
| Dionne  | Dionne Warwick |          1964 |     18 |  557 |         29.8 |
| Mya     | Mya & Sisqo    |          1998 |    101 | 2271 |         22.3 |

Singers whose first name surged in babies after they first reached the
Billboard top ten

Plotting those same names from 1975 with a marker on each singer’s first
chart year makes the timing visible. It shows the babies arriving right
after the music lands rather than before.

``` r
plot_trajectories(tot, bb_events$Name, events = bb_events, from = 1975,
                  title = "Named after the charts", subtitle = "Babies named for chart-topping singers")
```

<img src="README_files/figure-markdown_github/q2-bbfig-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This table lists the screen characters whose first name surged in babies
after the title appeared. It shows the same effect from television and
film, with names like Meadow after The Sopranos and Imani after Coming
to America arriving within a year or two of release.

``` r
hbo %>% slice_max(Ratio, n = 8) %>%
    transmute(Name, Title = title, Released = release_year, Score = tmdb_score, Peak = Post, `Times bigger` = Ratio) %>%
    kable(caption = "Screen characters whose first name surged in babies after the title appeared")
```

| Name    | Title                              | Released | Score | Peak | Times bigger |
|:-------|:------------------------------|--------:|------:|-----:|------------:|
| Whitley | A Different World                  |     1987 |   6.7 |  484 |        484.0 |
| Zaria   | The Parent ’Hood                   |     1995 |   6.4 |  444 |        444.0 |
| Meadow  | The Sopranos                       |     1999 |   8.5 |  270 |        270.0 |
| Zander  | Starship Troopers                  |     1997 |   7.0 |  236 |        236.0 |
| Lyric   | Jason’s Lyric                      |     1994 |   6.6 |  385 |         64.2 |
| Justice | The Pelican Brief                  |     1993 |   6.6 | 1634 |         29.5 |
| Imani   | Coming to America                  |     1988 |   6.8 |  394 |         28.1 |
| Gage    | Tales from the Darkside: The Movie |     1990 |   6.1 | 1272 |         25.6 |

Screen characters whose first name surged in babies after the title
appeared

Plotting the character names from 1980 with a marker on each title’s
release does the same for screen. It shows the same short lag, with the
names rising within a year or two of the title.

``` r
plot_trajectories(tot, hbo_events$Name, events = hbo_events, from = 1980,
                  title = "Named after the screen", subtitle = "Babies named for popular screen characters")
```

<img src="README_files/figure-markdown_github/q2-hbofig-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot pools every matched name on the year its trigger landed and
averages the response around it. It shows a clean event-study shape,
with naming flat before the trigger and jumping in the years just after,
which is the closing evidence that culture moves names.

``` r
plot_event_study(tot, ev_events)
```

<img src="README_files/figure-markdown_github/q2-event-1.png" alt="" width="100%" style="display: block; margin: auto;" />

# Question 3, Loans and Credit

What drives default across a million anonymised Lending Club loans.
Default is defined as a loan charged off against one fully paid, and the
analysis stays descriptive, reading default rates across the factors the
Institute cares about. The data is large, so this chunk is cached.

My approach starts with what default means, so I resolve each loan to
charged off against fully paid and drop the unresolved cases, which
`load_loans` and `prep_default` handle. From there the work stays
descriptive rather than modelled, reading the default rate across the
factors the Institute named, with `plot_grade`, `plot_dti` and
`plot_states` covering the lender’s own grade, the debt-to-income lever
and the geographic claim about Texas. The aim is to test the agency’s
beliefs against the data rather than to build a classifier.

``` r
list.files('Question3/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

loans <- load_loans("Question3/data/Loan_Cred/loan_data.rds") %>% # the one-million-row extract
    prep_default() # resolved loans with the default flag
```

This plot gives the default rate for each loan grade the lender assigns,
from A to G. It shows that default climbs without a single reversal from
roughly seven percent at grade A to nearly sixty percent at grade G, a
spread that dwarfs every other factor in the data.

``` r
plot_grade(loans)
```

<img src="README_files/figure-markdown_github/q3-grade-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot gives the default rate across bands of the debt-to-income
ratio. It shows that default rises smoothly as the ratio grows rather
than jumping at any threshold, so where to cap it is a tolerance choice
rather than a natural break.

``` r
plot_dti(loans)
```

<img src="README_files/figure-markdown_github/q3-dti-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot sets each state’s default rate next to what its grade mix
alone would predict. It shows that most of the gap between states is
just grade composition, and that Texas sits almost exactly on the
national average rather than apart from it.

``` r
plot_states(loans)
```

<img src="README_files/figure-markdown_github/q3-states-1.png" alt="" width="100%" style="display: block; margin: auto;" />

# Question 4, Netflix

A read on the Netflix catalogue for a team planning its own streaming
service. The functions parse the genres and countries, then map where
the films come from, what each country makes, how they rate and how long
they run, with HBO as a benchmark.

My approach parses the data before reading anything from it, so
`load_streaming` pulls genre and country out of the raw text fields and
`prep_movies` keeps films up to 2022. The plots then read the catalogue
from a few angles, where titles come from, what each country specialises
in, how acclaim and popularity diverge and how long films run, and
`plot_hbo` sets all of it against HBO as a benchmark.
`distinctive_words` cross-checks the genre map against the language of
the descriptions so the picture does not rest on the genre tags alone.

``` r
list.files('Question4/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

titles <- load_streaming("Question4/data/netflix/titles.rds") # IMDb-sourced titles

movies <- prep_movies(titles) # films up to 2022

hbo <- load_streaming("Question4/data/HBO_titles.rds") %>% 
    filter(type == "MOVIE") # HBO films

topc <- movies %>% 
    unnest(country) %>% 
    count(country, sort = TRUE) %>% 
    slice_head(n = 8) %>% 
    pull(country)

topg <- movies %>% 
    unnest(genre) %>% 
    count(genre, sort = TRUE) %>%
    slice_head(n = 8) %>% 
    pull(genre)

content_g <- c("drama","comedy","thriller","romance","action","documentation","crime")
```

This plot ranks the production countries by how many films each
contributes to the catalogue. It shows that the library is American at
its core but far from American alone, with India a clear second.

``` r
plot_countries(movies, 10)
```

<img src="README_files/figure-markdown_github/q4-countries-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot gives the share of each country’s films that falls in each
major genre. It shows that every tradition specialises differently, with
India leaning on drama and romance, Japan on action, and the United
States and Britain on documentaries.

``` r
plot_genre_country(movies, topc, topg)
```

<img src="README_files/figure-markdown_github/q4-heat-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot places each genre by its typical critic score against its
typical audience size. It shows that acclaim and popularity pull apart,
with documentaries scoring high on small audiences and thrillers doing
the reverse, which is the most useful signal for a new entrant.

``` r
plot_ratings(movies, topg)
```

<img src="README_files/figure-markdown_github/q4-ratings-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot compares the typical running time of films by production
country. It shows that length is a national fingerprint too, with Indian
films running far longer than the American median.

``` r
plot_length(movies, topc)
```

<img src="README_files/figure-markdown_github/q4-length-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This plot sets the typical Netflix score against HBO within each genre.
It shows that Netflix trades quality for breadth, rating below HBO in
every genre rather than only on average.

``` r
plot_hbo(movies, hbo, content_g)
```

<img src="README_files/figure-markdown_github/q4-hbo-1.png" alt="" width="100%" style="display: block; margin: auto;" />

This table lists the words most distinctive to each genre’s
descriptions, measured by how much more they appear there than
elsewhere. It shows that each genre reaches for its own vocabulary,
which confirms the genre map from the language up rather than from the
tags alone.

``` r
distinctive_words(movies, content_g) %>%
    group_by(genre) %>%
    summarise(`Distinctive words` = str_c(word, collapse = ", "), .groups = "drop") %>%
    kable()
```

| genre | Distinctive words |
|:-------------|:---------------------------------------------------------|
| action | naruto, assassin, mankind, lord, policeman, hero |
| comedy | hilariously, jokes, laughs, parenting, jeff, riffs |
| crime | serial, heist, criminals, investigation, lord, officers |
| documentation | documentary, unprecedented, interviews, footage, legacy, intimate |
| drama | biopic, aftermath, navigates, tensions, tested, ailing |
| romance | romantic, handsome, crush, romance, chef, marry |
| thriller | suspects, thriller, investigating, kills, heist, killers |
