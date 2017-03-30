#' Calculating response propensity score and treatment propensity score.
#'
#' Estimates the weights for treatment and control group. Multiplies response propensity score
#' and treatment propensity score for treatment and control group in order to to reweigh the
#' estimated average treatment effect to account for different forms of attrition.
#' 
#'
#' @param Y Numeric outcome variable. Can contain missing values. 
#' @param D Treatment variable, being 1 for treatment and 0 for control group.
#' @param X Observed covariates.
#' @param Z Instrumental variable.
#' 
#' @details
#' The function estimates the response propensity score and the treatment 
#' propensity score to reweigh the observations and, thus, correct for non-random 
#' attrition. The response propensity score is defined as $p(W) = Pr(R=1 | \code{X},
#' \code{D}, \code{Z})$ where $R$ denotes to the binary response variable 
#' being 1 if \code{Y} is observed and 0 otherwise (attrition), and the treatment
#' propensity score, defined as $\pi(\code{X}, p(W)) = Pr(\code{D}=1 | \code{X}, p(W), R=1)$.
#' Response and treatment 
#' 
#' 
#'
#'
#' @return The product of the response propensity score and treatment propensity score 
#' for T=1 and T=0.
#'  \item{squares}{The sum of the squared values}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' @note 
#' @examples

#' @rdname calculateWeights
#' @export
# Note that glm() requires stats package
calculateWeights <- function(Y, X, D, Z) {
  # Calculate attrition indicator, R 
  R <- as.numeric(!is.na(Y))  # if Y != NA, R = 1, R=0 otherwise
  # Calculate p(W) - using the instrument
  p_w <- glm(R ~ D + X + Z, family = binomial(link = logit)) 
  # Predict values given the model
  p_w_fits <- predict(object = p_w,
                      newdata = data.frame(cbind(X, D, Z)),
                      type = 'response')
  # Calculate pi(X, p(W)) - using instrumented probabilities and covariates
  pi <- glm(D ~ X + runif(100,0,1), family = binomial(link = logit), maxit = 1000) 
  # Predict values given the model
  pi_fits <- predict(object = pi,
                     newdata = data.frame(cbind(X, p_w_fits)),
                     type = 'response')
  # Note treated respondents
  Treated <- which(D == 1)
  # Create empty vector of weights
  Weights <- rep(NA, length(pi_fits))
  # Fill in weights for treated respondents
  Weights[Treated] <- p_w_fits[Treated] * pi_fits[Treated]
  # Fill in weights for untreated respondents
  Weights[-Treated] <- p_w_fits[-Treated] * (1 - pi_fits[-Treated])
  return(Weights)
}

