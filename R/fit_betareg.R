fit_betareg <- function(X,y, lambda, type){
  # arguments-
  # X: predictor matrix with intercept column
  # y: response
  # lambda: penalty coefficient

  #X = cbind(1, X)

  # initial values
  gaussianlasso <- glmnet::glmnet(X,y,lambda = lambda, alpha = 1)
  init <- c(1,as.vector(stats::coefficients(gaussianlasso)))
  #init  = c(1,solve(crossprod(X) + diag(lambda, ncol(X))) %*% crossprod(X,y))
  #init = c(1, rep(0.5, ncol(X)))
  names(init) = c('phi','intercept')

  fit_ml = stats::optim(
    par = init,
    fn  = penalized_ML,
    X   = X,
    y   = y,
    type = type,
    lambda = lambda,
    control = list(reltol = 1e-12)
  )
  fit_ml$par
}
