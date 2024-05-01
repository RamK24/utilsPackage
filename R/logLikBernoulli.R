
#'Calculate the Maximum Likelihood Estimate for Bernoulli Distribution
#'
#'This function calculates the MLE for Bernoulli Distribution using Grid search.
#' @param data A vector of binary outcomes (0s and 1s).
#'
#' @return The maximum likelihood estimate (MLE) for the parameter of the Bernoulli distribution based on grid search.
#' @export
#'
#' @examples
#' data <- c(1, 1, 0, 1, 1, 1, 1)
#' logLikBernoulli(data)

logLikBernoulli = function(data){

  params = seq(0.001, 0.9999, by=0.1)
  max_ll = -Inf
  max_p = 0
  n_successes = sum(data)
  n_failures = sum(!data)

  for (p in params){
    loglikelihood = n_successes* log(p) + n_failures * log(1 - p)

    ll = n_successes* log(p) + n_failures * log(1 - p)
    if (ll > max_ll){
      max_p = p
      max_ll = ll
    }
  }
  return (max_p)
}

