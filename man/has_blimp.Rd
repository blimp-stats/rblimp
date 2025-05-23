% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_blimp.R
\name{has_blimp}
\alias{has_blimp}
\title{Does the system have blimp installed?}
\usage{
has_blimp()
}
\value{
a \code{logical}
}
\description{
Returns a logical if blimp is detected by \code{\link{detect_blimp}}
}
\details{
This function uses \code{\link{detect_blimp}} to determine if blimp is installed.
}
\examples{
# Detect if system has blimp
has_blimp()
}
\seealso{
\code{\link{detect_blimp}}
}
