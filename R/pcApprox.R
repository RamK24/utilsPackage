#' Approximate data using principal components
#'
#' This function returns an approximation to the data based on the specified number of principal components.
#'
#' @param x A numeric vector or dataframe containing data.
#' @param npc An integer specifying the number of principal components.
#' @return A numeric vector or dataframe containing the approximation to the data.
#' @importFrom utilsPackage unscale
#' @importFrom stats prcomp
#' @importFrom tibble tibble
#' @export
#' @examples
#' set.seed(123)
#' x = tibble::tibble(x = c(1, 2, 3, 0, 7, 8, 9), y=c(21, 85.0, 13.2, 21, 99, 61, 10.8))
#' npc = 2
#' approximation <- pcApprox(x, npc)

pcApprox <- function(x, npc) {
  pca <- stats::prcomp(x, center = TRUE, scale=TRUE)
  pca$rotation <- pca$rotation[, 1:npc]
  x_hat <- scale(pca$x[, 1:npc] %*% t(pca$rotation))
  attr(x_hat, 'scaled:center') = pca$center
  attr(x_hat, 'scaled:scale') = pca$scale
  x_hat = unscale(x_hat)
  return(x_hat)
}
