#' Gaussian amplitud
#' @description Gaussian amplitude function
#' @author K. Juraic
#' @param x x value
#' @param p peak paramteres (p[1] = amplitude, p[2] = center, p[3] = width st.dev.)
#' @return y y value
#' @examples
#' gaussianAmpl(x = 0.1, p = c(1, 0, .5))
#'
gaussianAmpl <- function(x, p){
  y <- p[1] * exp(-.5 * ((x - p[2]) / p[3])^2)
  return(y)
}

#' Gaussian area
#' @description Gaussian area function
#' @author K. Juraic
#' @param x x value
#' @param p peak paramteres (p[1] = amplitude, p[2] = center, p[3] = width st.dev.)
#' @return y y value
#' @examples
#' gaussianArea(x = 0.1, p = c(1, 0, .5))
gaussianArea <- function(x, p){
  y = p[0] / (sqrt(2*pi) * p[3]) * exp(-.5 * ((x - p[2]) / p[3])^2)
  return(y)
}


multipeak(x, peakType)
