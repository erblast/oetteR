% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_plot.R
\name{f_plot_col_vector74}
\alias{f_plot_col_vector74}
\title{generate a most distinctive color scale}
\usage{
f_plot_col_vector74(greys = T, reds = T, blues = T, greens = T,
  faint = T, only_unique = F)
}
\arguments{
\item{greys}{boolean, include grey colors, Default: TRUE}

\item{reds}{boolean, include red colors, Default: TRUE}

\item{blues}{boolean, include blue colors, Default: TRUE}

\item{greens}{boolean, include green colors, Default: TRUE}

\item{faint}{boolean, include faint colors, Default: TRUE}

\item{only_unique}{boolean, do not allow color repetitions, Default: FALSE}
}
\value{
vector with HEX colours
}
\description{
based on RColorBrewer colours of length 74
for RGB colors see {rapidtables}(https://www.rapidtables.com/web/color/index.html).
Basically strings a couple of RColorBrewer palettes together.
}
