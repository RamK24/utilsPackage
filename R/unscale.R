#' Transforms a scaled Vector back to it's original form.
#'
#' This function unscales a vector that has been previously scaled using the `scale()` function.
#'
#' @param x A numeric vector that has been scaled.
#'
#' @return The unscaled vector.
#'
#' @details
#' This function retrieves the mean and standard deviation used for scaling from the attributes
#' of the input vector and then unscales the vector accordingly.
#'
#' @examples
#' # Example usage
#' # Example 1
#' scaled_vector = scale(1:10)
#' unscaled_vector = unscale(scaled_vector)
#' unscaled_vector
#'
#'# Example 2
#'x = c(2, 3, 4, 5)
#'attr(x, "scaled:center") = mean(x)
#'attr(x, "scaled:scale") = sd(x)
#'unscaled_vector = unscale(x)
#'unscaled_vector
#' @export
unscale = function(x){
  mean_ = attr(x, "scaled:center")
  sd_ = attr(x, "scaled:scale")
  unscaled_x = NULL
  if (!(is.null(mean_) | is.null(sd_))){
    unscaled_x = x * sd_ + mean_
  }
  else{
    return(x)
  }

  attr(unscaled_x, "scaled:center") = NULL
  attr(unscaled_x, "scaled:scale") = NULL
  return (unscaled_x)
}



