library(stats)

# refer to EQ 7 in Huber2012
estimateDelta <- function(Y, X, R, D, Z) {
  # Input vector of outcomes, Y
  # Input dataframe of covariates, X
  # Input vector of responses, R (can deduce from Y == NA)
  # Input vector of treatment indicators, D
  # Input vector of instrument values, Z
  
  Data <- data.frame(cbind(Y,X,R,D,Z))
  # Calculate p(W); top pg. 457
  Data$p_w <- glm(R ~ D + X + Z, 
                  family = gaussian(link = logit), 
                  data = Data
                  )
  # Calculate pi(X, p(W)); bottom pg. 458
  pi <- glm(D ~  X + p_w, 
            family = gaussian(link = logit), 
            data = Data[Data$R == 1, ]
            )
  # Need to convert p_w and pi to fitted values via predict.glm()
  Treated <- (R*D*Y)/(p_w*pi)
  Untreated <- (R*(1-D)*Y)/(p_w*(1-pi))
  delta <- Treated - Untreated
  
  
  ### !!! Jacob claims we need not calculate delta
  ### We need to estimate p(W) and pi, and use the denominator to weight a regression
}