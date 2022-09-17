
#' Normalise numerical vectors
#'
#' @description This function takes a numerical vector
#'  and computes the proportion. Missing values
#'  will be removed from the input vector but remain in the output vector.
#'
#' @param x A numerical vector with positive values (i.e. 0 or greater).
#' @examples
#' stack_normalise(c(10, 30, 40))
#' stack_normalise(c(75, 0, 5, 20, NA))
#' stack_normalise(c(NA, NA, 10))
#' @return A numerical vector of the same size with proportions.
#'  The sum of the numerical values in the return values should
#'  add up to 1.
#' @export
stack_normalise <- function(x) {
  if(!is.numeric(x)) stop("The input needs to be a numerical vector.")
  if(any(!is.na(x) && x < 0)) stop("All the values should be zero or greater.")
  x / sum(x, na.rm = TRUE)
}
