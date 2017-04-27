context("bootstrapDelta")

## Create simulation data:
library(MASS)
set.seed(12345)

Treatment <- rbinom(n = 1000, size = 1, prob = 0.5)
Covariate <- runif(n = length(Treatment), min = -1, max = 1)
Instrument <- runif(n = length(Treatment), min = -1, max = 2)

# For attrition on unobservables
unUV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
unU <- unUV[ , 1]
unV <- unUV[ , 2]

unY <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + unU
unR <- 0.5*Treatment + 0.5*Covariate + 1*Instrument + unV > 0
unY[!unR] <- NA

SimData <- data.frame(unY, Treatment, Covariate, Instrument)


## Functions for fitting and working with generalized additive model are required.
library(gam)

# Calculate weights for SimData:
Test.Weightlist <- calculateWeights(modelData = SimData,
                                    instrumentData = Instrument,
                                    p_W_Formula = R ~.,
                                    p_W_Method = binomial(link = logit),
                                    PiFormula = D ~.,
                                    PiMethod = binomial(link = logit))

# Check BoootstrapDelta
BootSim.both <- bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                               instrumentFormula = ~ Instrument, 
                               data = SimData,
                               effectType = 'Both') # default: effectType ='Population'
BootSim.both$MeanEst

# Bootstrap data: manual sampling of dataset with replacement
Sim.BootsList <- list()
for(i in 1:1000) {Sim.BootsList[[i]] = SimData[sample(nrow(SimData), 1000, replace  = T),]}


Sim.Estimates <- list()

Sim.Estimates$Resp <- sapply(
                      X = Sim.BootsList, 
                      FUN = function(x) estimateDelta(regressionFormula = 
                                            unY ~ Treatment + Covariate,
                                            instrumentFormula = ~ Instrument,
                                            data = x)$RespondentDelta$coefficients)

Sim.Estimates$Pop <- sapply(X = Sim.BootsList, 
                                      FUN = function(x) estimateDelta(regressionFormula = 
                                      unY ~ Treatment + Covariate,
                                      instrumentFormula = ~ Instrument,
                                      data = x)$PopulationDelta$coefficients)

    # Calculate results: mean, median, and standard errors, based on bootstrapped replications
    Sim.SEs <- lapply(Sim.Estimates, function(x) apply(x, 1, sd))
    Sim.Means <- lapply(Sim.Estimates, rowMeans)
    Sim.Medians <- lapply(Sim.Estimates, function(x) apply(x, 1, median))
    Sim.Quantiles <- lapply(Sim.Estimates, function(x) apply(x, 1, function(y) quantile(y, quantiles, na.rm=T)))

    # return list with mean, median, and standard error of estimated for treatment and control
    return(list(Sim.MeanEst = Sim.Means, 
                Sim.MedianEst = Sim.Medians, 
                Sim.SE = Sim.SEs,
                Sim.Quantiles = Sim.Quantiles,
                Sim.Matrix = Sim.Estimates)    )





#==================
# Unit testing:
#===================
test_that("bootstrapDelta returns right mean values", {
  # does the function produce right values?
  expect_equal(object = ,
               expected = 
  )
  expect_equal(object = ,
               expected = 
  )
})

# SAVED TESTS FOR LATER TESTS ---------------------------------------------------------------
## tests:
#test_that("bootstrapDelta returns an output of right class", {
#  # does the function produce an output of right class?
#  expect_is(object = , 
#            class = "list")})
#test_that("bootstrapDelta has right number of elements", {
#  # does the function produce ...?
#  expect_equal(object = , 
#               expected = )})
#test_that("bootstrapDelta detects error", {
#  # does the function detect error when it should?
#  expect_error()
#})
