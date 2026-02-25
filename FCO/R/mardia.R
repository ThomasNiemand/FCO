#' Internal function inspired by mardiaKurtosis and mardiaSkew from semTools
#'
#' @param x ...
#' @return Mardia's skewness and kurtosis
#' @keywords internal
#' @noRd
mardia <- function(x, use = "everything") {
  #Combined function
  X <- as.matrix(scale(x, center = TRUE, scale = FALSE))
  n <- nrow(X)
  S <- cov(X, use = use)
  invS <- solve(S)
  XS <- X %*% invS
  Q <- rowSums(XS * X)
  b2p <- sum(Q^2, na.rm = TRUE) / n
  M <- XS %*% t(X)
  b1p <- sum(M^3, na.rm = TRUE) / (n^2)
  c(skewness = b1p, kurtosis = b2p)
}
