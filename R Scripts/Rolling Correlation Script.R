# http://nakisa.org/rolling-regression-and-rolling-correlation/




#http://www.r-bloggers.com/new-r-code-for-moving-correlations/
# 2011-03-04
# v0.01

MovingCor <- function(x, y, window.size=21, method="pearson") {
  # Computes moving correlations between two vectors with symmetrical windows.
  #
  # Args:
  #   x: One of the two vectors whose correlation is to be calculated.
  #   y: The other vector. Note that it must be of the same length as x.
  #   window.size: The size of windows to be used for each calculated
  #                correlation. Note that if even numbers are chosen, the
  #                window will not be skewed as there will be one extra value
  #                on the upper-side of the window. Default size is 21.
  #   method: The method of correlation. May be: "pearson", "kendall", or
  #           "spearman". Default is "pearson".
  #
  # Returns:
  #   A vector of the moving correlations.
  n <- length(x)
  # Setup a few catches for error handling.
  if (TRUE %in% is.na(y) || TRUE %in% is.na(x)) {
    stop("Arguments x and y cannot have missing values.")
  }
  if (n &lt;= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  out <- rep(NA, round(window.size/2))  # Stuffing the returned vector.
  for (value in seq(from = 1, to = n - (window.size - 1))) {
    value.end &lt;- value + (window.size - 1)
    out <- append(out, cor(x[value:value.end],
                           y[value:value.end],
                           method = method))
  }
  out <- append(out, rep(NA, n - length(out)))  # Finish stuffing.
  return(out)
}