#' mizzou document
#'
#' Render mizzou_document HTML
#'
#' @param fig_width Width in inches of plot
#' @param fig_height Height in inches
#' @param df_print Defaults to "kable
#' @param css Optional
#' @param keep_md Defaults to FALSE
#' @param mathjax Whether to use mathjax.
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

#' todo document
#'
#' Render a nice-looking TO DO list in HTML
#'
#' @param fig_width Width in inches of plot
#' @param fig_height Height in inches
#' @param css Optional
#' @param keep_md Defaults to FALSE
#' @param self_contained Defaults to TRUE
#' @param ... Other args to output_format
#' @return html rendered document
#' @export
todo_document <- function (fig_width = 8,
                           fig_height = 6,
                           css = NULL,
                           keep_md = FALSE,
                           self_contained = TRUE,
                           ...) {
  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates",
      "todo_document", "resources", "style.css",
      package = "mizzourahmd")
  }
  css <- c(css,
    "https://fonts.googleapis.com/css?family=Roboto:400,400i,700,700i,900",
    "https://cdn.rawgit.com/tonsky/FiraCode/1.205/distr/fira_code.css")
  pre_knit <- NULL
  op <- getOption("rmarkdown.df_print")
  on.exit(options(rmarkdown.df_print = op), add = TRUE)
  options(rmarkdown.df_print = FALSE)
  rmarkdown::output_format(knitr = NULL, pandoc = NULL,
    pre_knit = pre_knit_todo,
    post_knit = post_knit_todo,
    keep_md = keep_md,
    base_format = rmarkdown::html_document(
      fig_width = fig_width, fig_height = fig_height,
      css = css, theme = "default", highlight = NULL,
      self_contained = self_contained, mathjax = NULL,
      pandoc_args = "--tab-stop=2", ...))
}

pre_knit_todo <- function(input, ...) {
  #tmp <- sub("\\.Rmd$", "-tmp-todo.Rmd", input)
  tmp <- tempfile(fileext = ".Rmd")
  .todo <- new.env()
  assign(".tmp", tmp, envir = .todo)
  assign(".input", input, envir = .todo)
  assign(".todo", .todo, envir = .GlobalEnv)
  x <- tfse::readlines(input)
  writeLines(x, tmp)
  y <- filter_yaml(x)
  if (any(grepl("title:", y))) {
    title <- grep("^title:", y, value = TRUE)
    title <- sub("^title:\\s+", "", title)
    title <- gsub("^['\"]|['\"]$", "", title)
    if (nchar(title) > 0) {
      p2 <- paste0('<h2 class="todotitle">', title, '</h2>')
      ey <- grep("^---", x)[2]
      p1 <- x[c(1:ey)]
      p3 <- x[c(c(ey + 1):length(x))]
      x <- c(p1, p2, p3)
    }
  }
  ## strike through
  s <- grepl("^[-+] ~.*~$", x)
  x[s] <- sub("~", '<s><span class="todostrike">', x[s])
  x[s] <- sub("~$", "</span></s>", x[s])
  ## orangeish highlight
  h <- grepl("^[-+] \\*\\*.*\\*\\*$", x)
  x[h] <- sub("\\*\\*", '<span class="todohlo">', x[h])
  x[h] <- sub("\\*\\*$", "</span>", x[h])
  ## greenish highlight
  h <- grepl("^[-+] \\*.*\\*$", x)
  x[h] <- sub("\\*", '<span class="todohlg">', x[h])
  x[h] <- sub("\\*$", "</span>", x[h])
  writeLines(x, input)
}

filter_yaml <- function(x) {
  fl <- grep("^---", x)
  x[(fl[1] + 1):(fl[2] - 1)]
}


post_knit_todo <- function(metadata, input_file, runtime, ...) {
  .todo <- get(".todo", envir = .GlobalEnv)
  tmp <- get(".tmp", envir = .todo)
  input <- get(".input", envir = .todo)
  x <- tfse::readlines(tmp)
  writeLines(x, input_file)
  #unlink(tmp)
}

cat_lines <- function(...) {
  cat(paste(c(...), collapse = "\n"), fill = TRUE)
}
