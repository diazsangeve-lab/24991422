slide_chart_text <- function(doc, kick, title, gg, blocks, # a reusable chart + text slide
                             chart_left = 5.0){  # function parameter for chart alignment
    doc <-
        add_slide(doc, layout = blank, master = "Office Theme")  # insert a blank slide template

    doc <-
        ph_with(doc, kicker(kick), location = ph_location(0.5, 0.30, 9, 0.3))  # place kicker text component

    doc <-
        ph_with(doc, head_t(title), location = ph_location(0.5, 0.55, 9, 0.7))  # place title component

    doc <-
        ph_with(doc, gg, location = ph_location(chart_left, 1.5, 4.6, 4.4))  # place the ggplot object

    text_left <- if(chart_left > 2) 0.5 else 5.3  # text opposite the chart

    doc <-
        ph_with(doc, do.call(block_list, blocks), location = ph_location(text_left, 1.6, 4.2, 5.2))  # render text blocks into placeholders
    doc
}