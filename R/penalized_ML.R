penalized_ML <- function(
    par,
    X,
    y,
    lambda,
    type) {
  # arguments-
  # par: parameters to be estimated
  # X: predictor matrix with intercept column
  # y: response
  # lambda: penalty coefficient
  # type: penalty approach

  # setup
  beta = par[-1]                               # coefficients
  phi = par[1]
  N = nrow(X)


  # linear predictor
  eta = X %*% beta                           # linear predictor
  mu = stats::plogis(eta)                           # logit link

  # calculate likelihood
  l = stats::dbeta(y, shape1 = mu*phi, shape2 = (1-mu)*phi, log = FALSE)

  #l[which(l == Inf)] <- 6e+300

  #pen_l = -sum(l) + alpha * (lambda * sum(abs(beta[-1]))) + (1-alpha) * (lambda * crossprod(beta[-1]))

  switch(
    type,
    'L1' = -sum(l) + lambda * sum(abs(beta[-1])),
    'L2' = -sum(l) + lambda * crossprod(beta[-1])
  )
}
