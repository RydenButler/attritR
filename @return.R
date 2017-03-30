
#' @return A numeric vector of the product of the response propensity score and treatment propensity score 
#'  \item{Y}{An object of responses which may or may not include NA}
#'  \item{D}{A numeric vector (binary; treatment = 1, contro = 0)} 
#'  \item{X}{A numeric vector of observed covariates}
#'  \item{Z}{A numeric vector of instrumental variable}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' @note 
#' @examples
#' D <- sample(c(0, 1), 100 , replace = T) # vector of treatment indicators
#' X <- rnorm(100, 0, 3) # vector of covariates
#' Z <- sample(1:5, 100, replace = T, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)) # vector of instruments
#' Y <- 5*D + 2*X + rnorm(100, 0, 3) # vector of Y values, related to D and X
#' calculateWeights(Y = Y, D = D, X = X, Z = Z)
