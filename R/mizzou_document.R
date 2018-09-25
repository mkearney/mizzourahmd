#' mizzou document
#'
#' Render mizzou_document HTML
#'
#' @param fig_width Width in inches of plot
#' @param fig_height Height in inches
#' @param df_print Defaults to "kable
#' @param css Optional
#' @param keep_md Defaults to FALSE
#' @param mathjax Whether ot use mathjax.
#' @param self_contained Defaults to TRUE
#' @param ... Other args to output_format
#' @return html rendered document
#' @export
mizzou_document <- function (fig_width = 8, fig_height = 6,
                             df_print = "kable", css = NULL,
                             keep_md = FALSE,
                             self_contained = TRUE,
                             mathjax = NULL, ...) {
  dev <- "png"
  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates",
      "mizzou_document", "resources", "style.css",
      package = "mizzourahmd")
  }
  pre_knit <- NULL
  rmarkdown::output_format(knitr = NULL, pandoc = NULL, df_print = df_print,
    pre_knit = pre_knit, keep_md = keep_md,
    base_format = rmarkdown::html_document(fig_width = fig_width,
      fig_height = fig_height, dev = dev, fig_retina = 2,
#      section_divs = FALSE, fig_caption = TRUE,
      css = css, theme = "default", highlight = "haddock",
      self_contained = self_contained, mathjax = mathjax,
      pandoc_args = "--tab-stop=2", ...))
}
