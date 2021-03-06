% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_profile_functions.R
\name{lorentzianAmpl}
\alias{lorentzianAmpl}
\title{Lorentzian amplitude}
\usage{
lorentzianAmpl(x = 0.1, y0 = 0, A = 1, xc = 0, w = 1)
}
\arguments{
\item{x}{x value}

\item{y0}{constant background}

\item{A}{amplitude}

\item{xc}{peak center}

\item{w}{peak width}
}
\value{
y y value
}
\description{
Lorentzian amplitude function
}
\examples{
   \dontrun{lorentzianAmpl(x = 0.1, y0 = 0, A = 1, xc = 0, w = 1)}
}
\references{
PeakFit v4.11
}
\author{
K. Juraic
}
