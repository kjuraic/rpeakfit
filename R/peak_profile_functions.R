#' Gaussian amplitud
#' @description Gaussian amplitude function
#' @author K. Juraic
#' @param x x value
#' @param y0 backgroubd
#' @param A amplitude
#' @param xc peak center
#' @param w peak width
#' @return y y value
#' @examples
#'    \dontrun{gaussianAmpl(x = 0.1, y0 = 0, A = 1, xc = 0, w = 1)}
gaussianAmpl <- function(x, y0, A, xc, w){
  y <- y0 + A * exp(-.5 * ((x - xc) / w) ^ 2)
  return(y)
}

#' Gaussian area
#' @description Gaussian area function
#' @author K. Juraic
#' @param x x value
#' @param y0 backgroubd
#' @param A amplitude
#' @param xc peak center
#' @param w peak width
#' @return y y value
#' @examples
#'    \dontrun{gaussianArea(x = 0.1, y0 = 0, A = 1, xc = 0, w = 1)}
gaussianArea <- function(x, y0, A, xc, w){
  y = y0 + A / (sqrt(2*pi) * w) * exp(-.5 * ((x - xc) / w) ^ 2)
  return(y)
}


