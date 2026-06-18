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

The first chart sets the roast strategy. Light and medium-light roasts
rate highest, so they should anchor the range.

``` r
plot_roast(Coffee)
```

<img src="README_files/figure-markdown_github/q1-roast-1.png" alt="" width="100%" style="display: block; margin: auto;" />

The second shows where the value sits. East African beans rate within a
point of Panama at a fraction of the price, which is the core sourcing
argument.

``` r
plot_region_value(Coffee)
```

<img src="README_files/figure-markdown_github/q1-region-1.png" alt="" width="100%" style="display: block; margin: auto;" />

The flavour fingerprint uses the data-derived words, showing which ones
are far more common among the very best coffees than across the shelf as
a whole.

``` r
plot_flavour_fingerprint(Coffee, keywords)
```

<img src="README_files/figure-markdown_github/q1-flavour-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Finally the supplier leaderboard separates the value champions from the
premium showpieces.

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

Persistence first. Each year’s 25 most popular names are tracked one,
two and three years on, and the rank correlation holds firmly through
most of the century before loosening after 1990, more for boys than for
girls.

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

The sharpest movements are spikes rather than drifts, and many sit on a
clear cultural event, which the surge table labels by the cause the
datasets can attribute.

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

The bubble chart sets these out by name and year, sized by babies at the
peak and coloured by whether a singer or a screen character drove the
name.

``` r
plot_spike_bubble(matches)
```

<img src="README_files/figure-markdown_github/q2-bubble-1.png" alt="" width="100%" style="display: block; margin: auto;" />

A surge is not a lasting name. Plotting the sharpest surges over their
full life shows most are fashions that fade within a decade.

``` r
plot_trajectories(tot, fade_names, title = "Fade or stick", subtitle = "The sharpest surges are usually short-lived fashions")
```

<img src="README_files/figure-markdown_github/q2-fade-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Scaling the biggest names of the century to their own peak tells the
same story one name at a time, with even the giants all but vanishing.

``` r
plot_name_distribution(tot, dist_names)
```

<img src="README_files/figure-markdown_github/q2-dist-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Music tests cleanly. Reading the first name of every charting act and
asking whether babies followed shows the chart all but creating names,
with Sade and Rihanna running from almost nothing into the thousands.

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

``` r
plot_trajectories(tot, bb_events$Name, events = bb_events, from = 1975,
                  title = "Named after the charts", subtitle = "Babies named for chart-topping singers")
```

<img src="README_files/figure-markdown_github/q2-bbfig-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Screen does the same. Character names like Meadow after The Sopranos and
Imani after Coming to America arrive within a year or two of the title.

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

``` r
plot_trajectories(tot, hbo_events$Name, events = hbo_events, from = 1980,
                  title = "Named after the screen", subtitle = "Babies named for popular screen characters")
```

<img src="README_files/figure-markdown_github/q2-hbofig-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Pooling every matched name on the year its trigger landed gives the
average naming response, the event study that closes the question.

``` r
plot_event_study(tot, ev_events)
```

<img src="README_files/figure-markdown_github/q2-event-1.png" alt="" width="100%" style="display: block; margin: auto;" />

# Question 3, Loans and Credit

What drives default across a million anonymised Lending Club loans.
Default is defined as a loan charged off against one fully paid, and the
analysis stays descriptive, reading default rates across the factors the
Institute cares about. The data is large, so this chunk is cached.

``` r
list.files('Question3/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

loans <- load_loans("Question3/data/Loan_Cred/loan_data.rds") %>%   # the one-million-row extract
    prep_default()                                                  # resolved loans with the default flag
```

The grade the lender already assigns is the headline. Default climbs
without a reversal from the A grade to the G grade, a gap that dwarfs
every other factor.

``` r
plot_grade(loans)
```

<img src="README_files/figure-markdown_github/q3-grade-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Debt-to-income gives the Director a lever. Default rises smoothly with
the ratio, so a cap is a tolerance choice rather than a natural break.

``` r
plot_dti(loans)
```

<img src="README_files/figure-markdown_github/q3-dti-1.png" alt="" width="100%" style="display: block; margin: auto;" />

States differ, but Texas sits squarely at the national average rather
than apart from it.

``` r
plot_states(loans)
```

<img src="README_files/figure-markdown_github/q3-states-1.png" alt="" width="100%" style="display: block; margin: auto;" />

# Question 4, Netflix

A read on the Netflix catalogue for a team planning its own streaming
service. The functions parse the genres and countries, then map where
the films come from, what each country makes, how they rate and how long
they run, with HBO as a benchmark.

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

The catalogue is American at its core but far from American alone, with
India a clear second.

``` r
plot_countries(movies, 10)
```

<img src="README_files/figure-markdown_github/q4-countries-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Each tradition makes something different, with India on drama and
romance, Japan on action, and the United States and Britain on
documentaries.

``` r
plot_genre_country(movies, topc, topg)
```

<img src="README_files/figure-markdown_github/q4-heat-1.png" alt="" width="100%" style="display: block; margin: auto;" />

The most useful finding for an investor is that acclaim and popularity
pull apart, with documentaries scoring high on small audiences and
thrillers doing the reverse.

``` r
plot_ratings(movies, topg)
```

<img src="README_files/figure-markdown_github/q4-ratings-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Length is a fingerprint too, with India running far longer than the
American median.

``` r
plot_length(movies, topc)
```

<img src="README_files/figure-markdown_github/q4-length-1.png" alt="" width="100%" style="display: block; margin: auto;" />

Set against HBO, Netflix trades quality for breadth, scoring lower in
every genre.

``` r
plot_hbo(movies, hbo, content_g)
```

<img src="README_files/figure-markdown_github/q4-hbo-1.png" alt="" width="100%" style="display: block; margin: auto;" />

The description text confirms the genre map, since each genre reaches
for its own vocabulary.

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
