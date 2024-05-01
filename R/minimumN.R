#' Calculate Minimum Sample Size for Two-sample t-test
#'
#'This function calculates the minimum sample size required for a two-sided one or two-sample t-test.
#' @param x1 Sample data of first group.
#' @param x2 Sample data of second group.
#'
#' @return The minimum sample size required for the t-test. for a two sample t-test, the returned value is sample size for each group.
#' @importFrom pwr pwr.t2n.test
#' @importFrom stats sd
#' @export
#' @examples
#' x1 = c(162, 123.5, 133.6, 178.2, 100, 125.6, 110, 105, 106)
#' x2 = c(99, 120.8, 105.3, 88, 87, 125, 92, 105, 107.8)
#' minimumN(x1, x2)
#' x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
#' minimumN(x1)
minimumN = function(x1,x2=NULL){

  min_n = 0
  if (is.null(x2)){
    n1 = length(x1)
    effect = mean(x1)/stats::sd(x1 - mean(x1))
    n2 = pwr::pwr.t2n.test(n1=n1, n2=NULL, d=effect, sig.level=0.05, power=0.8, alternative='two.sided')$n2
    min_n = min(n1, n2)
  }
  else{
    n1 = length(x1)
    n2 = length(x2)
    effect = abs(mean(x1) - mean(x2))/stats::sd(c(x1 - mean(x1), x2 - mean(x2)))
    n = pwr::pwr.t2n.test(n1=min(n1, n2), n2=NULL, d=effect, sig.level=0.05, power = 0.8, alternative='two.sided')$n2
    min_n = min(n, min(n1, n2))
  }
  return(ceiling(min_n))
}
