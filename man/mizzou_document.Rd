% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mizzou_document.R
\name{mizzou_document}
\alias{mizzou_document}
\title{mizzou document}
\usage{
mizzou_document(fig_width = 8, fig_height = 6, df_print = "kable",
  css = NULL, keep_md = FALSE, self_contained = TRUE,
  mathjax = NULL, ...)
}
\arguments{
\item{fig_width}{Width in inches of plot}

\item{fig_height}{Height in inches}

\item{df_print}{Defaults to "kable}

\item{css}{Optional}

\item{keep_md}{Defaults to FALSE}

\item{self_contained}{Defaults to TRUE}

\item{mathjax}{Whether to use mathjax.}

\item{...}{Other args to output_format}
}
\value{
html rendered document
}
\description{
Render mizzou_document HTML
}
