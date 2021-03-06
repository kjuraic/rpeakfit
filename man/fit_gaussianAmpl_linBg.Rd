% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peakfit_automatic.R
\name{fit_gaussianAmpl_linBg}
\alias{fit_gaussianAmpl_linBg}
\title{Automatic Gaussian peakfit with linear background}
\usage{
fit_gaussianAmpl_linBg(x, y, bnd = c(NA, NA))
}
\arguments{
\item{x}{independent variable}

\item{y}{dependent variable}

\item{bnd}{boundaries of fitting interval}
}
\value{
fit fiting parameters
}
\description{
peakfit gaussian with automatic parameter and fit boundaries estimation
             linear background is automaticly subtracted
             peak function profile = gaussianAmplitude()
             for optimization it is used nls function
}
\examples{
\dontrun{fit_gaussianAmpl(x, y)}

}
\author{
K. Juraic
}
