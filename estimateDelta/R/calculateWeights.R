# Note that glm() requires stats package
calculateWeights <- function(Y, X, D, Z) {
  # Calculate attrition indicator, R 
  R <- as.numeric(is.na(Y))
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

