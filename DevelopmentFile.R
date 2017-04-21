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
