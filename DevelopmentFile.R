library(devtools)
library(roxygen2)
library(MASS)

# DO NOT RUN THE FOLLOWING LINE
#### package.skeleton('estimateDelta')

# Compile latest package and re-documents
Current <- as.package('attritR')
load_all(Current)
document(Current)

# This will tak a moment ...
demo(plotEstimates)

### Check Proposition 4:
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

ObsData <- simulateData()$SimData[[1]]

# weights
Weights4 <- calculateWeights(modelData = ObsData[,1:3], 
                             instrumentData = ObsData[ , 4])

# delta
Delta4 <- estimateDelta(Y ~ D + X, 
                        instrumentFormula = ~ Z, 
                        data = ObsData)

# bootstrap
Boot4 <- bootstrapDelta(Y ~ D + X, 
                        instrumentFormula = ~ Z, 
                        data = ObsData,
                        effectType = 'Both',
                        nCores = 4)
Boot4$MeanEst

ATE(Y ~ D + X, 
        instrumentFormula = ~ Z,
        data = ObsData,
        effectType = 'Both',
        nCores = 4)

# difference of ~ 5 seconds per iteration: median 14.4;19.4
microbenchmark(bootstrapDelta(Y ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = ObsData,
                                       effectType = 'All',
                                       nCores = 4),
               bootstrapDelta(Y ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = ObsData,
                                       effectType = 'All',
                                       nCores = 1),
               times = 1)

test <- lineprof(bootstrapDelta(Y ~ Treatment + Covariate, 
                        instrumentFormula = ~ Instrument, 
                        data = ObsData,
                        effectType = 'All',
                        nCores = 4))
shine(test)


### Check Proposition 5:

# weights
Weights5 <- calculateWeights(modelData = FullData[,-ncol(FullData)], 
                             instrumentData = FullData[,ncol(FullData)])$AllWeights

# delta
Delta5 <- estimateDelta(Outcome ~ Treatment + Binary + Continuous, 
                       instrumentFormula = ~ Z1, 
                       data = FullData)$AllDelta

# bootstrap
Boot5 <- bootstrapDelta(Outcome ~ Treatment + Binary + Continuous, 
                        ~ Z1, 
                        FullData,
                        effectType = 'All'
                        )

### Check Proposition 6:

# weights
Weights6 <- calculateWeights(modelData = NoXData[,-ncol(NoXData)],
                             instrumentData = as.data.frame(NoXData[,ncol(NoXData)]))

# delta
Delta6 <- estimateDelta(Outcome ~ Treatment, 
                        instrumentFormula = ~ Z1, 
                        data = NoXData)

# bootstrap
Boot6 <- bootstrapDelta(Outcome ~ Treatment, 
                        ~ Z1, 
                        NoXData)

### Check GAM arguments
library(gam)
WeightsGAM <- calculateWeights(modelData = FullData[ , -ncol(FullData)], 
                               instrumentData = FullData[ , ncol(FullData)],
                               p_W_Formula = R ~ D + Binary + s(Continuous))

### Check top-level function
wrapperFunction(Outcome ~ Treatment + Binary + Continuous,
                ~ Z1,
                FullData,
                effectType = 'Respondent')
