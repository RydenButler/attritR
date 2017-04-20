library(devtools)
library(roxygen2)
library(MASS)

# DO NOT RUN THE FOLLOWING LINE
#### package.skeleton('estimateDelta')

# Compile latest package and re-documents
Current <- as.package('attritR')
load_all(Current)
document(Current)

# Simulate Data
simulateData <- function(treatmentBounds = c(-2,2), interactionEffect = 5, N = 1000){
  Sims <- lapply(seq(treatmentBounds[1], treatmentBounds[2], 0.1), function(D) {
    Covariate <- runif(n = N, min = -1, max = 1)
    Instrument <- runif(n = N, min = -1, max = 2)
    Treatment <- rbinom(n = N, size = 1, prob = 0.5)
    # For attrition on observables
    UV <- mvrnorm(n = N, mu = c(0,0), Sigma = matrix(c(1, 0, 0, 1), nrow = 2))
    U <- UV[ , 1]
    V <- UV[ , 2]
    # Counterfactual treatment effects
    YTreatment <- D + 1*Covariate + interactionEffect*Treatment*Covariate + U
    YControl <-  1*Covariate + U
    # Realized treatment effects and attrition
    Y <- D*Treatment + 1*Covariate + interactionEffect*Treatment*Covariate + U
    R <- D*Treatment + 1*Covariate + 0.5*Instrument + V > 0
    Y[!R] <- NA
    # Combines realized data
    SimData <- data.frame(Y, Treatment, Covariate, Instrument)
    # Treatment Effects
    ATE <- mean(YTreatment) - mean(YControl)
    ATR <- mean(Y[R & Treatment]) - mean(Y[R & !Treatment]) 
    return(list(ATE = ATE, ATR = ATR, SimData = SimData))
    }
    )
  ATE <- unlist(lapply(Sims, function(sim) sim$ATE))
  ATR <- unlist(lapply(Sims, function(sim) sim$ATR))
  SimData <- lapply(Sims, function(sim) sim$SimData)
  return(list(ATE = ATE, ATR = ATR, SimData = SimData))
}

plotEstimates <- function(simulatedData) {
  Estimates <- lapply(simulatedData$SimData, function(CurrentData){
    # OLS estimate among respondents
   OLS <-  summary(lm(Y ~ Treatment + Covariate, data = CurrentData))$coefficients[2,1:2]
   # bootstrapDelta estimates
   OurModel <- bootstrapDelta(Y ~ Treatment + Covariate,
                          instrumentFormula = ~ Instrument,
                          data = CurrentData,
                          effectType = 'Respondent',
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
  
  plot(x = seq(-2,2,.1), y = OLS-simulatedData$ATE, type = 'p', 
       ylim = c(-5,5), pch = 19, cex = 0.5, las = 1,
       xlab = 'Treatment Effect', ylab = 'Estimate - ATE')
  #points(x = 1:length(OLS), y = OLS-simulatedData$ATE+OLSSE*1.96, pch = '-')
  #points(x = 1:length(OLS), y = OLS-simulatedData$ATE-OLSSE*1.96, pch = '-')
  points(x = seq(-2,2,.1), y = Model-simulatedData$ATE, pch = 'X', cex = 0.5)
  #points(x = 1:length(OLS), y = Model-simulatedData$ATE+ModelSE*1.96, pch = 'x')
  #points(x = 1:length(OLS), y = Model-simulatedData$ATE-ModelSE*1.96, pch = 'x')
  abline(h=0)
}

# Run simulation
plotEstimates(simulateData())


### Check Proposition 4:
ObsData <- simulateData(treatmentBounds = c(-10,10))$SimData[[1]]

# weights
Weights4 <- calculateWeights(modelData = ObsData[,1:3], 
                             instrumentData = ObsData[ , 4])

# delta
Delta4 <- estimateDelta(Y ~ Treatment + Covariate, 
                        instrumentFormula = ~ Instrument, 
                        data = ObsData)

# bootstrap
Boot4 <- bootstrapDelta(Y ~ Treatment + Covariate, 
                        instrumentFormula = ~ Instrument, 
                        data = ObsData,
                        effectType = 'All',
                        nCores = 4)
Boot4$MeanEst

ATE(Y ~ Treatment + Covariate, 
        instrumentFormula = ~ Instrument,
        data = ObsData,
        effectType = 'All',
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
