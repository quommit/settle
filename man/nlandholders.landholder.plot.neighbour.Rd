% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LPN_survey.R
\name{nlandholders.landholder.plot.neighbour}
\alias{nlandholders.landholder.plot.neighbour}
\title{Landholders count}
\usage{
\method{nlandholders}{landholder.plot.neighbour}(object)
}
\arguments{
\item{object}{A \code{\link{survey}} object of landholder.plot.neighbour
  subclass.}
}
\value{
A non negative integer of length 1.
}
\description{
\code{nlandholders} gets the number of landholders in the survey object.
}
\details{
This method extracts the number of unique landholder names from the raw
hyper-table in the survey object.
}
\examples{
nlandholders(tagarina_croplands)
}

