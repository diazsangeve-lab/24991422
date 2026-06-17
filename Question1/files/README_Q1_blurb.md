# Question 1 — Coffee Hub

The deliverable is a PowerPoint report, `Coffee_Hub_Recommendation.pptx`, advising
the entrepreneur on which roast, region, flavour cues and suppliers to stock.

## Approach
The reviews carry the signal, so the work centres on reading them. `load_coffee()`
reads the csv as UTF-8 and transliterates the smart quotes and accents that break
in Excel, then joins the three review columns into one searchable field.
`score_keywords()` counts how many of the Stellenbosch student survey words appear
in each coffee's reviews, and that score tracks the expert rating at a correlation
of about 0.40, which is what justifies using the local vocabulary as a quality cue.

From there the analysis is functional throughout. `region_from_origin()` tags each
coffee with a broad origin region, and four plot functions (`plot_roast`,
`plot_region_value`, `plot_flavour_fingerprint`, `plot_supplier_leaderboard`)
return the ggplots. `build_deck.R` is the single end-to-end script: it sources the
`code/` folder, runs the pipeline, builds the plots and writes the PowerPoint with
`officer`, so the whole deliverable is reproducible from R alone.

## What the data says
- Lighter roasts rate highest, so the core range should be light and medium-light.
- Kenya and Ethiopia reach 93+ ratings at about a fifth of Panama's price, so they
  are the value backbone, with Panama and Hawaii kept as premium showpieces.
- The very best coffees read juicy, syrupy, floral and bright, which is the washed
  East African signature and matches the student vocabulary.
- Kakalove Cafe, JBC and Red Rooster are the value champion suppliers, with Hula
  Daddy, Dragonfly and Bird Rock as premium names.
