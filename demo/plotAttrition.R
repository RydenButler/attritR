library(MASS)
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
    ATRSE <- sqrt(sd(Y[R & D])/length(Y[R & D]) + sd(Y[R & !D])/length(Y[R & !D]))
    return(list(ATR = ATR, ATRSE = ATRSE, SimData = SimData))
  }
  )
  ATR <- unlist(lapply(Sims, function(sim) sim$ATR))
  ATRSE <- unlist(lapply(Sims, function(sim) sim$ATRSE))
  SimData <- lapply(Sims, function(sim) sim$SimData)
  return(list(ATE = ATE, ATR = ATR, ATRSE = ATRSE, SimData = SimData))
}

plotAttrition <- function(N = 1000,
                          treatmentEffectR = c(-2,2), 
                          instrumentEffectR = 1,
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
                               effectType = 'Both',
                               nBoots = 10)
    PopEst <- OurModel$Means$Pop[2]
    RespEst <- OurModel$Means$Resp[2]
    PopSE <- OurModel$SD$Pop[2]
    RespSE <- OurModel$SD$Resp[2]
    return(list(OLSCoef = OLS[1], OLSSE = OLS[2], 
                PopCoef = PopEst, PopSE = PopSE,
                RespCoef = RespEst, RespSE = RespSE))
  }
  )
  OLS <- unlist(lapply(Estimates, function(est) est$OLSCoef))
  OLSSE <- unlist(lapply(Estimates, function(est) est$OLSSE))
  Pop <- unlist(lapply(Estimates, function(est) est$PopCoef))
  PopSE <- unlist(lapply(Estimates, function(est) est$PopSE))
  Resp <- unlist(lapply(Estimates, function(est) est$RespCoef))
  RespSE <- unlist(lapply(Estimates, function(est) est$RespSE))
  
  # Plot bias across attrition
  plot(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
       y = OLS-simulations$ATE, type = 'l',
       ylim = c(min(c(min(OLS-simulations$ATE, na.rm = T)-0.5, 
                      min(Pop-simulations$ATE, na.rm = T)-0.5, 
                      -1)), 
                max(c(max(OLS-simulations$ATE, na.rm = T)+0.5, 
                      max(Pop-simulations$ATE, na.rm = T)+0.5, 
                      1))), 
       col = rgb(165, 20, 23, max = 255), las = 1,
       xlab = 'Treatment Effect on Attrition', ylab = 'Estimate - ATE')
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
         y = Pop-simulations$ATE, col = rgb(0, 115, 96, max = 255))
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
         y = Resp-simulations$ATE, col = rgb(98, 36, 102, max = 255))
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
         y = simulations$ATR-simulations$ATE, col = rgb(209, 95, 39, max = 255))
  abline(h=0)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(OLS - simulations$ATE + OLSSE*1.96,
                rev(OLS - simulations$ATE - OLSSE*1.96)),
          col = rgb(165, 20, 23, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(Pop - simulations$ATE + PopSE*1.96,
                rev(Pop - simulations$ATE - PopSE*1.96)),
          col = rgb(0, 115, 96, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(Resp - simulations$ATE + RespSE*1.96,
                rev(Resp - simulations$ATE - RespSE*1.96)),
          col = rgb(98, 36, 102, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(simulations$ATR - simulations$ATE + simulations$ATRSE*1.96,
                rev(simulations$ATR - simulations$ATE - simulations$ATRSE*1.96)),
          col = rgb(209, 95, 39, alpha = 35, max = 255),
          border = NA)
  legend(x = 'topright', 
         legend = c('OLS', 'Naive', 'Population', 'Respondents'),
         lty = 1,
         col = c(rgb(165, 20, 23, max = 255), rgb(209, 95, 39, max = 255),
                 rgb(0, 115, 96, max = 255), rgb(98, 36, 102, max = 255)),
         lwd = 2, bty = 'n', cex = .5)
  # Plot treatment effect estimate across attrition
  plot(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
       y = OLS, type = 'l',
       ylim = c(min(c(min(OLS, na.rm = T)-0.5, 
                      min(Pop, na.rm = T)-0.5, 
                      -1)), 
                max(c(max(OLS, na.rm = T)+0.5, 
                      max(Pop, na.rm = T)+0.5, 
                      1))), 
       col = rgb(165, 20, 23, max = 255), las = 1,
       xlab = 'Treatment Effect on Attrition', ylab = 'Estimate')
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
        y = Pop, col = rgb(0, 115, 96, max = 255))
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
        y = Resp, col = rgb(98, 36, 102, max = 255))
  lines(x = seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), 
        y = simulations$ATR, col = rgb(209, 95, 39, max = 255))
  abline(h=simulations$ATE)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(OLS + OLSSE*1.96,
                rev(OLS - OLSSE*1.96)),
          col = rgb(165, 20, 23, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(Pop + PopSE*1.96,
                rev(Pop - PopSE*1.96)),
          col = rgb(0, 115, 96, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(Resp + RespSE*1.96,
                rev(Resp - RespSE*1.96)),
          col = rgb(98, 36, 102, alpha = 35, max = 255),
          border = NA)
  polygon(x = c(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1),
                rev(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1))),
          y = c(simulations$ATR + simulations$ATRSE*1.96,
                rev(simulations$ATR - simulations$ATRSE*1.96)),
          col = rgb(209, 95, 39, alpha = 35, max = 255),
          border = NA)
  legend(x = 'topright', 
         legend = c('OLS', 'Naive', 'Population', 'Respondents'),
         lty = 1,
         col = c(rgb(165, 20, 23, max = 255), rgb(209, 95, 39, max = 255),
                 rgb(0, 115, 96, max = 255), rgb(98, 36, 102, max = 255)),
         lwd = 2, bty = 'n', cex = .5)
}

# Run simulation
plotAttrition(treatmentEffectR = c(-5,10))
