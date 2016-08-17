
#' Automatic Gaussian peakfit
#'
#' @description peakfit gaussian with automatic parameter and fit boundaries estimation
#'              peak function profile = gaussianAmplitude()
#'              for optimization it is used nls function
#' @author K. Juraic
#' @param x independent variable
#' @param y dependent variable
#' @param bnd boundaries of fitting interval
#' @return fit fiting parameters
#' @examples
#' \dontrun{fit_gaussianAmpl(x, y)}
#'
fit_gaussianAmpl <- function(x, y, bnd = c(NA, NA)){
  plot(x, y, type = 'o', pch = 16, cex = .5)
  if (is.na(bnd[1]) | is.na(bnd[2]))
    bnd <- locator(2)$x
  cat("X fitting interval: (", bnd[1], bnd[2],")\n")
  abline(v = bnd, col = 'green')
  xs <- x[x > bnd[1] & x < bnd[2] & !is.na(y)]
  ys <- y[x > bnd[1] & x < bnd[2] & !is.na(y)]
  points(xs, ys, type = 'l', col = 'green')
  pStart <- c(y0 = min(ys),
              A = max(ys) - min(ys),
              xc = xs[which.max(ys)],
              w = (max(xs) - min(xs))/4.)
  fit <- nls(ys ~ gaussianAmpl(xs,y0,A,xc,w),start = pStart)
  points(xs,predict(fit), col = 'red',type = 'l')
  print(coef(summary(fit)))
  return(fit)
}



#' Automatic Gaussian peakfit with linear background
#'
#' @description peakfit gaussian with automatic parameter and fit boundaries estimation
#'              linear background is automaticly subtracted
#'              peak function profile = gaussianAmplitude()
#'              for optimization it is used nls function
#' @author K. Juraic
#' @param x independent variable
#' @param y dependent variable
#' @param bnd boundaries of fitting interval
#' @return fit fiting parameters
#' @examples
#' \dontrun{fit_gaussianAmpl(x, y)}
#'
fit_gaussianAmpl_linBg <- function(x, y, bnd = c(NA, NA)){
  plot(x, y, type = 'o', pch = 16, cex = .5)
  if (is.na(bnd[1]) | is.na(bnd[2]))
    bnd <- locator(2)$x
  cat("X fitting interval: (", bnd[1], bnd[2],")\n")
  abline(v = bnd, col = 'green')
  xs <- x[x > bnd[1] & x < bnd[2] & !is.na(y)]
  ys <- y[x > bnd[1] & x < bnd[2] & !is.na(y)]
  x1 <- xs[1]
  y1 <- ys[1]
  x2 <- xs[length(xs)]
  y2 <- ys[length(ys)]
  yLin <- (y2 - y1) / (x2 - x1) * (xs - x1) + y1
  ys <- ys - yLin
  points(xs, yLin + ys, type = 'l', col = 'green')
  pStart <- c(y0 = min(ys),
              A = max(ys) - min(ys),
              xc = xs[which.max(ys)],
              w = (max(xs) - min(xs))/4.)
  tryCatch({
    fit <- NULL
    fit <- nls(ys ~ gaussianAmpl(xs,y0,A,xc,w),start = pStart)
    if (is.null(fit)) {
      fit <- NULL
    } else {
      points(xs, yLin + predict(fit), col = 'red', type = 'l')
      points(xs, yLin, type = 'l', col = 'yellow')
      print(coef(summary(fit)))
    }
  }, error = function(e) {
    fit <- NULL
  })
  return(list(fit = fit, data = c(data.frame(x = xs, y = ys))))
}


# fitGaussPeakLinBg -------------------------------------------------------
#' fit peak to gaussian with linear bacground
#' @author Krunoslav Juraic
#' @description Fit peak to gaussian function with linear background. Fitting
#'              interval should be defined by two end points which shoud be
#'              selected by two clicks at graph. Function will return fit.
#'              Function will return list with parameters of fit and data.frame
#'              with data (x,y) in interval used for fit (between two selected
#'              points). Linear background will be constructed as linear
#'              function thorought two end points.
#' @param x x data for fit
#' @param y y data for fit
#' @param range range of x data that will be used for graph drawing
#' @return list(fit, data)
#' @examples
#'  \dontrun{fitGaussPeakLinBg(x, y, range)}
fitGaussPeakLinBg <- function(x, y, range) {
  fit <- fit_gaussianAmpl_linBg(x = x[range], y = y[range])
  if (is.list(fit$fit)) {
    x_max <- round(coef(fit$fit)['xc'])
  } else {
    x_max <- fit$data$x[which.max(fit$data$y)]
  }
  abline(v = x_max, col='red')
  locator(1)
  return(x_max)
}

