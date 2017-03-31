#' Calculating weights for treatment and control group under non-random attrition
#'
#' \code{calculateWeights} estimates the weights for treatment and control group 
#' in order to obtain internally and externally valid estimates for the average treatment
#' effect in cases of attrition.
#' 
#' @param Y Numeric vector of outcome variable. Non-response should be coded as NA. 
#' @param D Numeric vector of treatment indicators.
#' @param X Numeric vector of observed covariates.
#' @param Z Numeric vector of instrumental variable.
#' @param method String indicating method of probability estimation. Can be glm or gam.
#' 
#' @details
#' The function estimates the response propensity score and the treatment 
#' propensity score to reweigh the observations and correct for attrition. 
#' The response propensity score is defined as $p(W) = Pr(R=1 | \code{X},
#' \code{D}, \code{Z})$ where $R$ denotes to the binary response variable 
#' being 1 if \code{Y} is observed and 0 otherwise (attrition), and the treatment
#' propensity score, defined as $\pi(\code{X}, p(W)) = Pr(\code{D}=1 | \code{X}, p(W), R=1)$.
#' Response and treatment propensity scores are multiplied for treatment and control group
#' in order to adjust for differences in the distributions of X and p(W) between 
#' treated and nontreated respondents. For more information on the method, 
#' see Huber (2012) below.
#' 
#' @references Huber (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A numeric vector of the product of the response propensity score and treatment propensity score 
#'  \item{Y}{An object of responses which may or may not include NA}
#'  \item{D}{A numeric vector (binary; treatment = 1, contro = 0)} 
#'  \item{X}{A numeric vector of observed covariates}
#'  \item{Z}{A numeric vector of instrumental variable}
#'  \item{method}{A string indicating the method of probability estimation}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname calculateWeights
#' @export

calculateWeights <- function(Y, X, D, Z, method = 'glm') {
  # Calculate attrition indicator, R 
  # R is test participation: if Y is NOT NA, R=1; if Y is NA, R=0
  R <- as.numeric(!is.na(Y))
  
  if(method == 'glm'){
    #### Calculate response propensity score p(W)
    # fitting generalized linear model: response propensity R regressed on treatment status,
    # observed covariates, and instrument
    p_w <- glm(formula = R ~ D + X + Z, family = binomial(link = logit)) 
    # Predict values given the model
    p_w_fits <- predict(object = p_w,
      newdata = data.frame(cbind(X, D, Z)),
      type = 'response')
    
    #### Calculate treatment propensity score pi(X, p(W))
    # fitting GLM: treatment propensity D regressed on instrumented probabilities 
    # and covariates
    pi <- glm(formula = D ~ X + runif(100,0,1), 
              family = binomial(link = logit), maxit = 1000) 
    # Predict values given the model
    pi_fits <- predict(object = pi,
                       newdata = data.frame(cbind(X, p_w_fits)),
                       type = 'response')
  } else {
    #### Calculate response propensity score p(W)
    # fitting generalized additive model: response propensity R regressed on treatment status,
    # observed covariates, and instrument
    p_w <- gam(formula = R ~ D + X + Z, family = binomial(link = logit)) 
    # Predict values given the model
    p_w_fits <- predict(object = p_w,
                        newdata = data.frame(cbind(X, D, Z)),
                        type = 'response')    
    
    #### Calculate treatment propensity score pi(X, p(W))
    # fitting GAM: treatment propensity D regressed on instrumented probabilities 
    # and covariates
    pi <- gam(formula = D ~ X + runif(100,0,1), 
              family = binomial(link = logit), maxit = 1000) 
    # Predict values given the model
    pi_fits <- predict(object = pi,
                       newdata = data.frame(cbind(X, p_w_fits)),
                       type = 'response')    
  }
  
  # Identify treated respondents
  Treated <- which(D == 1)
  # Create empty vector of weights
  Weights <- rep(NA, length(pi_fits))
  # Estimate weights for treated group
  Weights[Treated] <- p_w_fits[Treated] * pi_fits[Treated]
  # Estimate weights for control group
  Weights[-Treated] <- p_w_fits[-Treated] * (1 - pi_fits[-Treated])
  return(Weights)
}

