# Coffee Hub: builds the recommendation deck end to end, entirely in R.
# Sources the code/ functions, runs the analysis, and writes the .pptx with officer.

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, officer, flextable, stringi, ggrepel, scales, maps)

list.files("code/", full.names = TRUE, recursive = TRUE) %>%
    .[grepl("\\.R$", .)] %>% as.list() %>% walk(~source(.))

# Palette
ESP <- "#2E1D14"; BROWN <- "#7B4B2A"; ACC <- "#C75B39"; INK <- "#3A2A1E"; MUT <- "#8A7560"

# ---- Data and analysis ------------------------------------------------------

Coffee <- load_coffee("data/Coffee/Coffee.csv")    # load, fixing strange characters

Coffee <- load_coffee("data/Coffee/Coffee.csv") %>%   # load, fixing strange characters
    filter(Cost_Per_100g > 0) %>%                     # drop the odd zero-cost rows
    mutate(desc_all = replace_na(desc_all, ""))       # a couple of reviews are missing, treat as empty text

region_lookup <-
    c("Ethiopia"="Ethiopia","Kenya"="Kenya","Colombia"="Colombia","Panama"="Panama",
      "Hawai"="Hawaii","Guatemala"="Guatemala","Sumatra|Lintong|Sulawesi|Indonesia"="Indonesia",
      "Costa Rica"="Costa Rica","Brazil"="Brazil","Honduras"="Honduras","Rwanda"="Rwanda")

Coffee <- region_from_origin(Coffee, region_lookup) # tag each coffee's origin region

# ---- Derive the indicator words from the reviews (nothing hand-picked) -------

keywords <- derive_keywords(Coffee, n = 12) # top words by rating lift
n_words  <- length(keywords)

# Honest validation: derive the words on one half, score and correlate on the
# other, so the rating link is not a circular artefact of the same data.
set.seed(42)
hold        <- sample(c(TRUE, FALSE), nrow(Coffee), replace = TRUE)
kw_train    <- derive_keywords(Coffee[!hold, ], n = 12)
test_scored <- score_keywords(Coffee[hold, ], kw_train)
cor_hold    <- round(cor(test_scored$Student_Score, test_scored$Rating, use = "complete.obs"), 2)

Coffee <- score_keywords(Coffee, keywords) # final indicator score on all coffees

# ---- Compute every figure the slides quote ----------------------------------

n_coffees <- nrow(Coffee)
rat_lo    <- as.integer(min(Coffee$Rating)); rat_hi <- as.integer(max(Coffee$Rating))

roast_tab <- Coffee %>% group_by(roast) %>% summarise(r = mean(Rating), .groups = "drop")
light_r   <- roast_tab$r[roast_tab$roast == "Light"]
medl_r    <- roast_tab$r[roast_tab$roast == "Medium-Light"]

region_tab <- Coffee %>% filter(Region != "Other") %>%
    group_by(Region) %>% summarise(r = mean(Rating), cost = mean(Cost_Per_100g), k = n(), .groups = "drop") %>%
    filter(k >= 20)
rg  <- function(R, col) region_tab[[col]][region_tab$Region == R]   # pull one region's value

sup <- Coffee %>% group_by(roaster) %>%
    summarise(r = mean(Rating), cost = mean(Cost_Per_100g), k = n(), .groups = "drop") %>%
    filter(k >= 20, r >= 93)
value_sup <- sup %>% arrange(cost) %>% slice_head(n = 3)            # cheapest high-raters
prem_sup  <- sup %>% filter(!roaster %in% value_sup$roaster) %>%    # top-rated, excluding the value names
    arrange(desc(r)) %>% slice_head(n = 3)


# Flavour fingerprint shares, for the two most distinguishing markers:
fp <- tibble(word = keywords,
             top = map_dbl(keywords, ~mean(str_detect(str_to_lower(Coffee$desc_all[Coffee$Rating >= 95]), .x))),
             all = map_dbl(keywords, ~mean(str_detect(str_to_lower(Coffee$desc_all), .x)))) %>%
    mutate(gap = top - all) %>% arrange(desc(gap))


# The four figures (the plot functions return ggplots):

g_roast    <-
    plot_roast(Coffee)   # rating by roast

origin_map_gg <-
    plot_origin_map(Coffee) # world map

g_region   <-
    plot_region_value(Coffee)  # rating vs cost by region

g_flavour  <-
    plot_flavour_fingerprint(Coffee, keywords) # fed the derived words

g_supplier <-
    plot_supplier_leaderboard(Coffee)   # suppliers by quality and price

# ---- Text helpers -----------------------------------------------------------

wlist <- function(k) str_c(head(keywords, k), collapse = ", ")     # the derived words, as text
money <- function(x) sprintf("$%.2f", x)

starter <- tribble(
    ~Shelf,            ~`Origin & roast`,         ~`Why it earns its place`,                                              ~`Buy from`,
    "Everyday hero",   "Kenya, light",            sprintf("%.1f rating at about %s, juicy and bright", rg("Kenya","r"), money(rg("Kenya","cost"))),     "Kakalove, JBC",
    "Deep bench",      "Ethiopia, medium-light",  sprintf("%.1f rating, the widest choice (%d coffees)", rg("Ethiopia","r"), as.integer(rg("Ethiopia","k"))),       "JBC, Red Rooster",
    "Value surprise",  "Indonesia, medium-light", sprintf("%.1f rating at the lowest cost, %s", rg("Indonesia","r"), money(rg("Indonesia","cost"))),    "Kakalove, Red Rooster",
    "Premium pour",    "Panama, light",           sprintf("%.1f showpiece at %s for the feature menu", rg("Panama","r"), money(rg("Panama","cost"))),    "Hula Daddy, Dragonfly")

# ---- Styling helpers --------------------------------------------------------

kicker <- function(t) fpar(ftext(toupper(t), fp_text(color = ACC, bold = TRUE, font.size = 12, font.family = "Calibri")))
head_t <- function(t) fpar(ftext(t, fp_text(color = ESP, bold = TRUE, font.size = 30, font.family = "Cambria")))
lead   <- function(b, body, sz = 15)                                  # bold lead-in then body text
    fpar(ftext(b, fp_text(color = ESP, bold = TRUE, font.size = sz, font.family = "Calibri")),
         ftext(body, fp_text(color = INK, font.size = sz, font.family = "Calibri")),
         fp_p = fp_par(padding.bottom = 8, line_spacing = 1.15))

# A flextable styled for the deck:
style_ft <- function(ft, header_fill = BROWN){
    ft %>% flextable::font(part = "all", fontname = "Calibri") %>%
        fontsize(part = "all", size = 12) %>%
        bg(part = "header", bg = header_fill) %>% color(part = "header", color = "white") %>%
        bold(part = "header") %>% align(part = "all", align = "left", j = NULL) %>%
        border_outer(part = "all", border = fp_border(color = "antiquewhite3", width = 1)) %>%
        border_inner(part = "all", border = fp_border(color = "antiquewhite3", width = 1)) %>%
        padding(part = "all", padding = 5)
}

# ---- Build the deck -------------------

doc <- read_pptx()  # start from the default template
blank <- "Blank" # layout I place everything onto

## Slide 1: Title
doc <-
    add_slide(doc,
              layout = blank,
              master = "Office Theme")

doc <-
    ph_with(doc,
            kicker("Data-Driven Coffee Procurement Strategy"),
            location = ph_location(0.5, 1.6, 9, 0.4))

doc <-
    ph_with(doc,
            fpar(ftext("Stocking The Neelsie Coffee Hub",
                       fp_text(color = ESP, bold = TRUE,
                               font.size = 48,
                               font.family = "Cambria"))),
            location = ph_location(0.5, 2.1, 9, 1.2))

doc <-
    ph_with(doc,
            fpar(ftext("This presentation explores optimal coffee selections, regional sourcing strategies, and preferred suppliers. The findings indicate the best options by evaluating a database of expert-reviewed coffees alongside the specific preferences of Stellenbosch University students.",
                       fp_text(color = INK, font.size = 16, font.family = "Calibri"))),
            location = ph_location(0.5, 4, 8.2, 1.2))

doc <-
    ph_with(doc,
            fpar(ftext("Prepared for the Neelsie Coffee Project by Diaz Sangeve   \u00B7  Student Number - 24991422",
                       fp_text(color = MUT, font.size = 11, font.family = "Calibri"))),
            location = ph_location(0.5, 6.7, 9, 0.4))

## Slide 2: Approach
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("Methodological Approach"), location = ph_location(0.5, 0.30, 9, 0.3))
doc <- ph_with(doc, head_t("Data Aggregation and Evaluation Strategy"), location = ph_location(0.5, 0.55, 9, 0.7))

stat_hdr <- c(format(n_coffees, big.mark = ","), "3", sprintf("%d-%d", rat_lo, rat_hi), as.character(n_words))
stat_df  <- as_tibble(setNames(list("Coffees Reviewed", "Expert Reviews Per Coffee",
                                    "Rating Range", "Indicator Words Derived"), stat_hdr))
stat_ft <- flextable(stat_df) %>%
    style_ft() %>% align(part="all", align="center") %>%
    fontsize(part="header", size=22) %>% color(part="header", color=ACC) %>%
    bg(part="header", bg="white") %>% bg(part="body", bg="antiquewhite3") %>% width(width = 2.25)
doc <- ph_with(doc, stat_ft, location = ph_location(0.5, 1.5, 9, 1.2))

doc <- ph_with(doc, block_list(
    lead("Data Attributes. ", "The dataset records the roaster, bean origin, roast strength, cost per 100g, an expert rating and three tasting reviews for each coffee."),
    lead("Indicator Words. ", sprintf("The entrepreneur mentioned a student survey of favourite-coffee words but did not supply the list, so we derive the indicators from the reviews. Every review word is ranked by how much its presence lifts the expert rating, and the top %d are kept.", n_words)),
    lead("Validation. ", sprintf("Deriving those words on one half of the coffees and scoring the other half, the count of indicator words tracks the expert rating at a held-out correlation of %.2f, so the signal is real and not a circular artefact.", cor_hold))),
    location = ph_location(0.5, 3.1, 9, 3.8))

## Slide 3: Roast
doc <- slide_chart_text(doc, "Recommendation 1 \u00B7 Roast", "Prioritise Lighter Roasts", g_roast,
                        list(
                            lead("Lighter roasts win. ", sprintf("Light and medium-light coffees average %.1f and %.1f, ahead of medium and dark. Light roasting preserves the origin character that the expert reviews consistently reward.", light_r, medl_r)),
                            lead("Optimal inventory strategy. ", "The core range should be built from light and medium-light beans, with a small dark-roast selection kept for specific requests.")),
                        chart_left = 5.0)

## Slide 4-5: Region
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("Sourcing"), location = ph_location(left = 0.5, top = 0.30, width = 9, height = 0.3))
doc <- ph_with(doc, head_t("Where the best beans are grown"), location = ph_location(left = 0.5, top = 0.55, width = 9, height = 0.7))
doc <- ph_with(doc, origin_map_gg, location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5.0))

doc <- slide_chart_text(doc, "Recommendation 2 \u00B7 Sourcing Region", "Where the value lives", g_region,
                        list(
                            lead("East Africa is the sweet spot. ", sprintf("Kenya averages %.1f and Ethiopia %.1f, within a point of Panama's %.1f, yet they cost about %s per 100g against Panama's %s. Ethiopia also offers the widest choice, with %d coffees.", rg("Kenya","r"), rg("Ethiopia","r"), rg("Panama","r"), money(rg("Kenya","cost")), money(rg("Panama","cost")), as.integer(rg("Ethiopia","k")))),
                            lead("Lead with Kenya and Ethiopia. ", "Make them the backbone of the menu, with a small premium shelf of Panama and Hawaii as showpieces.")),
                        chart_left = 0.5)

## Slide 6: Flavour
doc <- slide_chart_text(doc, "Recommendation 3 \u00B7 Flavour Profile", "What the best cups taste like", g_flavour,
                        list(
                            lead("The best cups share a profile. ", sprintf("Ranking review words by rating lift, the strongest markers are %s. Among coffees rating 95 and up, reviews are about %.1f times as likely to read %s and %.1f times as likely to read %s.", wlist(6), fp$top[1]/fp$all[1], fp$word[1], fp$top[2]/fp$all[2], fp$word[2])),
                            lead("Buy for these markers. ", sprintf("This juicy, tropical, floral-resinous signature is the washed East African profile. When choosing between lots, let words like %s break the tie.", wlist(3)))),
                        chart_left = 5.0)

## Slide 7: Suppliers
doc <- slide_chart_text(doc, "Recommendation 4 \u00B7 Preferred Suppliers", "Strategic Procurement Options", g_supplier,
                        list(
                            lead("Value champions. ", sprintf("%s (%.1f at %s), %s (%.1f at %s) and %s (%.1f at %s) pair high scores with low prices across deep ranges, so they can anchor the everyday menu.",
                                                              value_sup$roaster[1], value_sup$r[1], money(value_sup$cost[1]),
                                                              value_sup$roaster[2], value_sup$r[2], money(value_sup$cost[2]),
                                                              value_sup$roaster[3], value_sup$r[3], money(value_sup$cost[3]))),
                            lead("Premium showpieces. ", sprintf("%s (%.1f, top-rated), %s (%.1f) and %s (%.1f) bring the standout cups for the feature shelf.",
                                                                 prem_sup$roaster[1], prem_sup$r[1], prem_sup$roaster[2], prem_sup$r[2], prem_sup$roaster[3], prem_sup$r[3]))),
                        chart_left = 0.5)

## Slide 8: starter range table
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("Putting it together"), location = ph_location(0.5, 0.30, 9, 0.3))
doc <- ph_with(doc, head_t("A starter range for the shop"), location = ph_location(0.5, 0.55, 9, 0.7))
range_ft <- flextable(starter) %>% style_ft() %>%
    width(j = 1, width = 1.8) %>% width(j = 2, width = 2.4) %>% width(j = 3, width = 3.6) %>% width(j = 4, width = 1.7) %>%
    bg(i = seq(1, nrow(starter), 2), bg = "#F6EFE7", part = "body")
doc <- ph_with(doc, range_ft, location = ph_location(0.5, 1.6, 9, 4))
doc <- ph_with(doc, fpar(ftext("Ratings and prices are catalogue averages across the reviewed coffees", fp_text(italic = TRUE, color = MUT, font.size = 11, font.family = "Calibri"))),
               location = ph_location(0.5, 6.7, 9, 0.3))

## Slide 9: the buying recipe
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("The buying recipe"), location = ph_location(0.5, 0.40, 9, 0.3))
doc <- ph_with(doc, head_t("Five moves to a shelf that pours above its price"), location = ph_location(0.5, 0.70, 9, 1.0))
recipe <- list(
    lead("1.  Roast.  ", "Build the core range from light and medium-light beans, which rate highest and keep the origin character.", 16),
    lead("2.  Region.  ", "Lead with Kenya and Ethiopia, 93-plus cups at about a fifth of premium prices.", 16),
    lead("3.  Flavour.  ", sprintf("Favour lots that read %s, the markers the data ties to the very best.", wlist(4)), 16),
    lead("4.  Suppliers.  ", sprintf("Anchor on %s, %s and %s for value, with %s and %s as showpieces.",
                                     value_sup$roaster[1], value_sup$roaster[2], value_sup$roaster[3], prem_sup$roaster[1], prem_sup$roaster[2]), 16),
    lead("5.  Price.  ", "Most of the quality sits near $6 to $7 per 100g, so a strong everyday menu need not be expensive.", 16))
doc <- ph_with(doc, do.call(block_list, recipe), location = ph_location(0.5, 2.1, 9, 4.8))

print(doc, target = "Coffee_Hub_Recommendation.pptx")
