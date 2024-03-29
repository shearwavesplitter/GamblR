% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimatespread.R
\name{espread}
\alias{espread}
\title{Estimate spread}
\usage{
espread(mlodds, spreadodds, oldspread = c(-3, 3), newspread = c(-3.5, 3.5),
  oddstype1 = default.GamblR.odds(), oddstype2 = default.GamblR.odds())
}
\arguments{
\item{mlodds}{Moneyline odds (vector of length two)}

\item{spreadodds}{Odds for the original spread (vector of length two)}

\item{oldspread}{The original points spread}

\item{newspread}{The 'new' spread to convert to}

\item{oddstype1}{Format of original odds (e.g. decimal, see Convert odds)}

\item{oddstype2}{Format of output odds (e.g. decimal, see Convert odds)}
}
\value{
A list containing containing the new effective odds and new points spread.
}
\description{
Estimate the effective odds for a different points spread
}
\details{
Extrapolates what the effective odds for a new point spread would be based on the original spread and the money line odds. Useful for deciding if a tip is worth tailing when your book has a different spread.
}
