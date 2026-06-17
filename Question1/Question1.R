# Coffee Hub: builds the recommendation deck end to end, entirely in R.
# Sources the code/ functions, runs the analysis, and writes the .pptx with officer.

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, officer, flextable, stringi, ggrepel, scales)

list.files("code/", full.names = TRUE, recursive = TRUE) %>%
    .[grepl("\\.R$", .)] %>% as.list() %>% walk(~source(.))

# ---- Data and analysis ------------------------------------------------------

Coffee <- load_coffee("data/Coffee/Coffee.csv")    # load, fixing strange characters

student_words <-
    c("sweet","chocolate","aroma","structure","finish","mouthfeel","toned","savory",  # student
      "syrupy","tart","rich","juicy","acidity","zest","bright","crisp","balanced","cocoa",  # survey
      "velvety","delicate","floral","smooth","silky","honey","citrus","fruit","spice",   # flavour
      "resonant","plush","deep","fresh","almond","berry","vanilla","nutty","caramel")   # words

region_lookup <-
    c("Ethiopia"="Ethiopia","Kenya"="Kenya","Colombia"="Colombia","Panama"="Panama",  # origin
      "Hawai"="Hawaii","Guatemala"="Guatemala","Sumatra|Lintong|Sulawesi|Indonesia"="Indonesia",
      "Costa Rica"="Costa Rica","Brazil"="Brazil","Honduras"="Honduras","Rwanda"="Rwanda")

Coffee <-
    Coffee %>%
    score_keywords(student_words) %>%   # add the student match score
    region_from_origin(region_lookup) %>%   # tag each coffee's region
    filter(Cost_Per_100g > 0)     # drop the odd zero-cost rows

# The four figures (the plot functions return ggplots):
g_roast    <-
    plot_roast(Coffee)   # rating by roast

g_region   <-
    plot_region_value(Coffee)  # rating vs cost by region

g_flavour  <-
    plot_flavour_fingerprint(Coffee,  # what the best cups taste like
                c("juicy","syrupy","floral","bright","balanced","rich","fruit","chocolate","crisp","cocoa"))

g_supplier <-
    plot_supplier_leaderboard(Coffee)   # suppliers by quality and price

# The starter-range table shown on the shortlist slide:
starter <- tribble(
    ~Shelf,            ~`Origin & roast`,         ~`Why it earns its place`,                      ~`Buy from`,
    "Everyday hero",   "Kenya, light",            "93.8 rating at about $7, juicy and bright",     "Kakalove, JBC",
    "Deep bench",      "Ethiopia, medium-light",  "93.1 rating, floral, the widest choice",        "JBC, Red Rooster",
    "Value surprise",  "Indonesia, medium-light", "93.0 rating at the lowest cost, $5.30",         "Kakalove, Red Rooster",
    "Premium pour",    "Panama or Hawaii, light", "94+ showpiece for the feature menu",            "Hula Daddy, Dragonfly")

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
doc <-
    add_slide(doc,
              layout = blank,
              master = "Office Theme")

doc <-
    ph_with(doc,
            kicker("Methodological Approach"),
            location = ph_location(0.5, 0.30, 9, 0.3))

doc <-
    ph_with(doc, head_t("Data Aggregation and Evaluation Strategy"),
            location = ph_location(0.5, 0.55, 9, 0.7))

stat_ft <-
    flextable(tibble(`2,095`="Coffees Reviewed",
                     `3`="Expert Reviews Per Coffee",
                     `84-98`="Rating Range",
                     `36`="Student Flavour Keywords")) %>%
    style_ft() %>%
    align(part="all", align="center") %>%
    fontsize(part="header", size=22) %>%
    color(part="header", color=ACC) %>%
    bg(part="header", bg="white") %>%
    bg(part="body", bg="antiquewhite3") %>%
    width(width = 2.25)

doc <-
    ph_with(doc,
            stat_ft,
            location = ph_location(0.5, 1.5, 9, 1.2))

doc <-
    ph_with(doc, block_list(
        lead("Data Attributes. ", "The dataset contains information on the roaster, bean origin, roast strength, cost per unit, expert ratings, and comprehensive tasting reviews for each coffee."),
        lead("Evaluation Metric. ", "We score each coffee on how many of the student survey words appear in its reviews. That score tracks the expert rating at a correlation of 0.40, so the local vocabulary is a real signal, not noise."),
        lead("Research Objectives. ", "The analysis identifies the optimal roast levels, regions, flavour profiles, and suppliers to maximise value and quality for the proposed coffee shop.")),
        location = ph_location(0.5, 3.1, 9, 3.8))

## Slide 3
doc <-
    slide_chart_text(doc, "Recommendation 1 \u00B7 Roast", "Prioritise Lighter Roasts", g_roast,
                        list(
                            lead("Lighter roasts win. ", "Light and medium-light coffees attain average scores of 93.5 and 93.2 respectively. These scores surpass those of medium and dark variants. Light roasting preserves the origin characteristics that expert reviews consistently reward."),
                            lead("Optimal inventory strategy. ", "The core product range should focus on light and medium-light beans. A minimal selection of dark-roast options can be retained to satisfy specific consumer requests, variety always wins.")),
    chart_left = 5.0)

## Slide 4
doc <-
    slide_chart_text(doc, "Recommendation 2 \u00B7 Sourcing Region", "Where the value lives", g_region,
                        list(
                            lead("East Africa provides the highest marginal benefit. ", "Kenya averages 93.8 and Ethiopia 93.1, within a point of Panama's 94.3, yet they cost about $7 per 100g against Panama's $31. Ethiopia also offers the widest choice, with 355 coffees in the catalogue."),
                            lead("Strategic sourcing allocation. ", "Kenya and Ethiopia should form the primary supply base. Premium options from Panama and Hawaii can be stocked in limited quantities as exclusive offerings.")),
    chart_left = 0.5)

## Slide 5
doc <-
    slide_chart_text(doc, "Recommendation 3 \u00B7 Flavour Profile", "Defining High-Quality Characteristics", g_flavour,
                     list(
                         lead("The best cups share a profile. ", "Among coffees rating 95 and up, reviews are twice as likely to read juicy and far more likely to read syrupy, floral and bright. Crisp and plain chocolate mark the everyday cup."),
                         lead("Buy for the words students use. ", "That juicy, syrupy, floral signature is the washed East African profile. When choosing between lots, let these notes break the tie.")),
    chart_left = 5.0)

## Slide 6
doc <-
    slide_chart_text(doc, "Recommendation 4 \u00B7 Preferred Suppliers", "Strategic Procurement Options", g_supplier,
                     list(
                         lead("Value champions. ", "Kakalove Cafe (94.2 at $6.50), JBC Coffee Roasters (93.6 at $6.40) and Red Rooster (94.0 at $5.70) carry deep, high-scoring ranges at low prices."),
                         lead("Premium showpieces. ", "Hula Daddy Kona (95.1, top-rated), Dragonfly (94.2, richest profile) and Bird Rock (94.2) for the feature shelf.")),
    chart_left = 0.5)

## Slide 7: starter range table
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("Putting it together"), location = ph_location(0.5, 0.30, 9, 0.3))
doc <- ph_with(doc, head_t("A starter range for the shop"), location = ph_location(0.5, 0.55, 9, 0.7))
range_ft <- flextable(starter) %>% style_ft() %>%
    width(j = 1, width = 1.8) %>% width(j = 2, width = 2.4) %>% width(j = 3, width = 3.6) %>% width(j = 4, width = 1.7) %>%
    bg(i = seq(1, nrow(starter), 2), bg = "#F6EFE7", part = "body")
doc <- ph_with(doc, range_ft, location = ph_location(0.5, 1.6, 9, 4))
doc <- ph_with(doc, fpar(ftext("Ratings and prices are catalogue averages across the reviewed coffees", fp_text(italic = TRUE, color = MUT, font.size = 11, font.family = "Calibri"))),
               location = ph_location(0.5, 6.7, 9, 0.3))

## Slide 8: the buying recipe
doc <- add_slide(doc, layout = blank, master = "Office Theme")
doc <- ph_with(doc, kicker("The buying recipe"), location = ph_location(0.5, 0.40, 9, 0.3))
doc <- ph_with(doc, head_t("Five moves to a shelf that pours above its price"), location = ph_location(0.5, 0.70, 9, 1.0))
recipe <- list(
    lead("1.  Roast.  ", "Build the core range light and medium-light. They rate highest and keep the origin character.", 16),
    lead("2.  Region.  ", "Lead with Kenya and Ethiopia for 93+ cups at about a fifth of premium prices.", 16),
    lead("3.  Flavour.  ", "Choose lots that read juicy, syrupy, floral and bright, the mark of the very best.", 16),
    lead("4.  Suppliers.  ", "Anchor on Kakalove, JBC and Red Rooster for value, with Hula Daddy and Dragonfly as showpieces.", 16),
    lead("5.  Price.  ", "Most quality sits near $6 to $7 per 100g, so a strong everyday menu need not be expensive.", 16))
doc <- ph_with(doc, do.call(block_list, recipe), location = ph_location(0.5, 2.1, 9, 4.8))

print(doc, target = "Coffee_Hub_Recommendation.pptx")                 # write the deck
