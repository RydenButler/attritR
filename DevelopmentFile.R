library(devtools)
library(roxygen2)
library(MASS)

# DO NOT RUN THE FOLLOWING LINE
#### package.skeleton('estimateDelta')

# Compile latest package and re-documents
Current <- as.package('ATTR')
load_all(Current)
document(Current)

# Simulate Data
N <- 1000
TreatmentEffect <- 2
InteractionEffect <- -5
Covariate <- runif(n = N, min = -1, max = 1)
Instrument <- runif(n = N, min = -1, max = 2)
Treatment <- rbinom(n = N, size = 1, prob = 0.5)
# For attrition on observables
UV <- mvrnorm(n = N, mu = c(0,0), Sigma = matrix(c(1, 0, 0, 1), nrow = 2))
U <- UV[ , 1]
V <- UV[ , 2]

YTreatment <- TreatmentEffect + 1*Covariate + InteractionEffect*Treatment*Covariate + U
YControl <-  1*Covariate + U

Y <- TreatmentEffect*Treatment + 1*Covariate + InteractionEffect*Treatment*Covariate + U

R <- TreatmentEffect*Treatment + 1*Covariate + 0.5*Instrument + V > 0

Y[!R] <- NA

ObsData <- data.frame(Y, Treatment, Covariate, Instrument)

# Treatment Effects
ATE <- mean(YTreatment) - mean(YControl)
ATT <- mean(Y[R & Treatment]) - mean(Y[R & !Treatment])

# OLS estimates among respondents
lm(Y ~ Treatment + Covariate)

### Check Proposition 4:

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
                        effectType = 'All')
Boot4$MeanEst

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
