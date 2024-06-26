% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{round_char}
\alias{round_char}
\title{Character-safe rounding}
\usage{
round_char(x, digits = 0, as_char = FALSE, ...)
}
\arguments{
\item{x}{a character vector.}

\item{digits}{integer indicating the number of decimal places.}

\item{as_char}{Whether the result should be formatted as a character with trailing 0s (if relevant)}

\item{...}{arguments to be passed to methods.}
}
\value{
The character vector or the rounded version if numeric.
}
\description{
Round a vector if it is numeric, but return the original vector if it is character.
}
\keyword{internal}
