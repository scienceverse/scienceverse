% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_id_idx}
\alias{get_id_idx}
\title{Get index and id}
\usage{
get_id_idx(study, id = NULL, section = "hypotheses")
}
\arguments{
\item{study}{A study list object with class scivrs_study}

\item{id}{The id for the section (index or character) if NULL, assigns to the last item in the list}

\item{section}{The section to search, c("hypotheses", "analyses", "data")}
}
\value{
A list of id and idx
}
\description{
Get the index and id from an item in the hypotheses, analyses, or data sections of a study object
}
\keyword{internal}
