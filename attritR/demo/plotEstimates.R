simulateData <- function(N = 1000,
                         treatmentEffectR = c(-2,2), 
                         covariateEffectR = 1,
                         instrumentEffectR = 1,
                         treatmentEffectY = 1, 
                         covariateEffectY = 1,
                         interactionEffectY = .25
){
  X <- runif(n = N, min = -1, max = 1)
  Z <- runif(n = N, min = -1, max = 2)
  D <- rbinom(n = N, size = 1, prob = 0.5)
  # For attrition on unobservables
  UV <- mvrnorm(n = N, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
  U <- UV[ , 1]
  V <- UV[ , 2]
  # Counterfactual treatment effects
  YTreatment <- treatmentEffectY + covariateEffectY*X + interactionEffectY*D*X + U
  YControl <-  covariateEffectY*X + U
  # Counterfactual ATE | All
  ATE <- mean(YTreatment) - mean(YControl)
  
  Sims <- lapply(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), function(currentR) {
    # Realized treatment effects and attrition
    Y <- treatmentEffectY*D + covariateEffectY*X + interactionEffectY*D*X + U
    R <- currentR*D + covariateEffectR*X + instrumentEffectR*Z + V > 0
    Y[!R] <- NA
    # Combines realized data
    SimData <- data.frame(Y, D, X, Z)
    # ATE | Response
    ATR <- mean(Y[R & D]) - mean(Y[R & !D]) 
    return(list(ATR = ATR, SimData = SimData))
  }
  )
  ATR <- unlist(lapply(Sims, function(sim) sim$ATR))
  SimData <- lapply(Sims, function(sim) sim$SimData)
  return(list(ATE = ATE, ATR = ATR, SimData = SimData))
}

plotEstimates <- function(N = 1000,
                          treatmentEffectR = c(-2,2), 
                          instrumentEffectR = 0,
                          covariateEffectR = 1,
                          treatmentEffectY = 2, 
                          interactionEffectY = 5,
                          covariateEffectY = 1
){
  simulations <- simulateData(N = N,
                              treatmentEffectR = treatmentEffectR, 
                              instrumentEffectR = instrumentEffectR,
                              covariateEffectR = covariateEffectR,
                              treatmentEffectY = treatmentEffectY, 
                              interactionEffectY = interactionEffectY,
                              covariateEffectY = covariateEffectY)
  Estimates <- lapply(simulations$SimData, function(CurrentData){
    # OLS estimate among respondents
    OLS <-  summary(lm(Y ~ D + X, data = CurrentData))$coefficients[2,1:2]
    # bootstrapDelta estimates
    OurModel <- bootstrapDelta(Y ~ D + X,
                               instrumentFormula = ~ Z,
                               data = CurrentData,
                               effectType = 'All',
                               nBoots = 10)
    Est <- OurModel$MeanEst[2]
    SE <- OurModel$SE[2]
    return(list(OLSCoef = OLS[1], OLSSE = OLS[2], 
                ATECoef = Est, ATESE = SE))
  }
  )
  OLS <- unlist(lapply(Estimates, function(est) est$OLSCoef))
  OLSSE <- unlist(lapply(Estimates, function(est) est$OLSSE))
  Model <- unlist(lapply(Estimates, function(est) est$ATECoef))
  ModelSE <- unlist(lapply(Estimates, function(est) est$ATESE))
  
  plot(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
       y = OLS-simulations$ATE, type = 'p',
       ylim = c(min(c(min(OLS-simulations$ATE, na.rm = T)-0.5, 
                      min(Model-simulations$ATE, na.rm = T)-0.5, 
                      -1)), 
                max(c(max(OLS-simulations$ATE, na.rm = T)+0.5, 
                      max(Model-simulations$ATE, na.rm = T)+0.5, 
                      1))), 
       pch = 'O', cex = 0.5, las = 1,
       xlab = 'Treatment Effect on Attrition', ylab = 'Estimate - ATE')
  #points(x = 1:length(OLS), y = OLS-simulatedData$ATE+OLSSE*1.96, pch = '-')
  #points(x = 1:length(OLS), y = OLS-simulatedData$ATE-OLSSE*1.96, pch = '-')
  points(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
         y = Model-simulations$ATE, pch = 'X', cex = 0.5)
  points(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
         y = simulations$ATR-simulations$ATE, pch = 'N', cex = 0.5)
  #points(x = 1:length(OLS), y = Model-simulatedData$ATE+ModelSE*1.96, pch = 'x')
  #points(x = 1:length(OLS), y = Model-simulatedData$ATE-ModelSE*1.96, pch = 'x')
  abline(h=0)
}

# Run simulation
plotEstimates(treatmentEffectR = c(-5,10))
