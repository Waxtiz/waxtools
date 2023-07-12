#' Scale values between 0 and 1
#'
#' This function scales the input values between 0 and 1.
#'
#' The scaling is done by subtracting the minimum value and dividing by the range (maximum - minimum).
#'
#' @param x Numeric vector of values to be scaled
#' @param ... Additional arguments to be passed to the min and max functions
#' @return Numeric vector of scaled values between 0 and 1
#' @examples
#' \dontrun{
#' # Example usage of the function
#' scale01(c(1, 2, 3, 4, 5))
#' }
#'
scale01 <- function(x, ...) {
	(x -min(x, ...)) / (max(x, ...) -min(x, ...))
}