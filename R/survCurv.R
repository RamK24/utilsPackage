#' Plots a Survival Curve
#'
#' This function plots the survival curve using the provided survival times and event status.
#'
#' @param status A vector indicating the status of each individual (1 for event occurrence, 0 for censored).
#' @param time A vector of survival times.
#'
#' @details
#' Estimates survival using Kaplan-Meier.
#'
#'This function requires the ggplot2 and survival packages to be installed.
#'
#' @import ggplot2
#' @import survival
#'
#' @examples
#' # Example usage
#' status <- c(1, 1, 0, 1, 0)
#' time <- c(2, 4, 6, 8, 10)
#' survCurv(status, time)
#'
#' @export

survCurv = function(status,time)
{

  surv_object = Surv(time = time, event = status)
  fit <- survfit(surv_object ~ 1)
  ggplot2::ggplot() + ggplot2::geom_line(aes(x=fit$time, y = fit$surv), color='darkgreen')  +
    ggplot2::labs(x = 'time', y='survival') +
    ggplot2::theme_linedraw() +
    ggplot2::scale_x_continuous(breaks = seq(0, 25, 2)) +
    ggplot2::theme(text = element_text(size=12, family = "mono", face = "bold"))
}



