#' Fit beta model with L1 penalization
#'
#' @param X matrix
#' @param y response vector
#' @param lambda penalization parameter
#' @param standarize logical.
#' @param ... other arguments
#'
#' @return fit
#' @export
#'
#' @examples
#'
#' library(penalizedbeta)
#'
#' y <- data$y
#' X <- data[,-1]
#'
#' n <- dim(X)[1]; p <- dim(X)[2]
#' split <- sample(x = c("train","test"), size = n,
#'                 prob = c(0.8,0.2), replace = TRUE)
#' Xtrain <- X[split == "train",] ; Xtest <- X[split == "test",]
#' ytrain <- y[split == "train"] ; ytest <- y[split == "test"]
#'
#' nlambda = 100
#' lambdas <-  seq(0.0001,0.1, length.out = nlambda)
#' s <- lambdas[1]
#'
#' fit <- betareg_lasso(Xtrain,ytrain,lambda = s)
#'
#' ypred_test <- predict(fit, newdata = Xtest)
#' mean((ytest-ypred_test)^2)

betareg_lasso <- function(X, y, lambda, standarize = TRUE, ...){

# check arguments ----------------------------------------------------------
## X: matrix
  X <- as.matrix(X)

## y: numeric vector with values between 0 and 1
  stopifnot("Response values out of bounds (less than 0 or greater than 1)" =
            !(any(y <= 0) | any(y>=1)))


# Prepare data ------------------------------------------------------------

  this.call <- match.call(expand.dots = FALSE)
  dim <- dim(X)


# Fit model ---------------------------------------------------------------
  fit <- fit_betareg(X,y,lambda = lambda, type = "L1") # lasso

## Parameters estimates
  coefficients = list(phi=fit[1], beta = fit[-1])


  output <- list(call = this.call,
                 dim = dim,
                 coefficients = coefficients)
  class(output) <- "penalizedbeta"


  return(output)
  }
