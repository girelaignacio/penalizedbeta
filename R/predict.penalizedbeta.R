#' Predict
#'
#' @param object a fitted object of class inheriting from "penalizedbeta".
#' @param newdata optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
#' @param ... further arguments passed to or from other methods.
#'
#' @return predictions
#' @export
#'
predict.penalizedbeta <- function(object, newdata = NULL, ...){
  newdata <- as.matrix(newdata)

  n <- dim(newdata)[1]; p <- dim(newdata)[2]

  beta_hat <-  object$coefficients$beta
  eta_hat <- cbind(1,as.matrix(newdata)) %*% beta_hat
  mu_hat <- stats::plogis(eta_hat)
  phi_hat <- object$coefficients$phi
  y_hat <- stats::plogis(eta_hat)
    #stats::rbeta(n, shape1 = mu_hat*phi_hat, shape2 = (1-mu_hat)*phi_hat)
  return(y_hat)
}
