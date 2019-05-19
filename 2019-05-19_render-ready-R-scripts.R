
#' ---
#' title: "Render-ready R scripts"
#' author: "Nayef Ahmad"
#' date: "May 19th, 2019"
#' ---
 
#' Here's some prose in a very special comment. The way to
#' get this to display as nice human-readable text is to
#' start the comment with `#'` rather than just `#`

#' Let's summarize the built-in dataset `VADeaths`.

#' 
#' Note that pressing return at the end of one of these lines will 
#' ensure the next line is also human-readable text. 
#' 
#' Reference: https://happygitwithr.com/r-test-drive.html 
#' 
## here is a regular code comment, that will remain as such
summary(VADeaths)

#' Here's some more prose. I can use usual markdown syntax to make things
#' **bold** or *italics*. Let's use an example from the `dotchart()` help to
#' make a Cleveland dot plot from the `VADeaths` data. I even bother to name
#' this chunk, so the resulting PNG has a decent name.
#+ dotchart
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")
