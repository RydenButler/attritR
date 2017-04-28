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

# Bootstrap data: manual sampling of dataset with replacement
Sim.BootsList <- list()
for(i in 1:1000) {Sim.BootsList[[i]] = SimData[sample(x = nrow(SimData), 
                                                      size = 1000, 
                                                      replace  = TRUE),]}
# Calculate estimates
Sim.Estimates <- list()
Sim.Estimates$Resp <- sapply(X = Sim.BootsList, 
                             FUN = function(x) estimateDelta(regressionFormula = unY ~ Treatment + Covariate,
                                                             instrumentFormula = ~ Instrument,
                                                             data = x)$RespondentDelta$coefficients)
Sim.Estimates$Pop <- sapply(X = Sim.BootsList, 
                            FUN = function(x) estimateDelta(regressionFormula = unY ~ Treatment + Covariate,
                                      instrumentFormula = ~ Instrument,
                                      data = x)$PopulationDelta$coefficients)

# Calculate results: mean, median, and standard errors, based on bootstrapped replications
# create basic functions
SE.fun <- function(x) sd(x)/sqrt(length(x)) # standard error
Quantile.fun <- function(x) quantile(x, probs = c(0.05, 0.95), na.rm=TRUE) # quantile function

# apply to the list of simulation estimates
# mean
Sim.Means <- lapply(X = Sim.Estimates, FUN = rowMeans)
# median
Sim.Medians <- lapply(X = Sim.Estimates, 
                      FUN = function(x) apply(X = x, 
                                              MARGIN = 1, 
                                              FUN = median))
# standard error
Sim.SE <- lapply(X = Sim.Estimates, 
                 FUN = function(x) apply(X = x, 
                                         MARGIN = 1, 
                                         FUN = SE.fun))
# 5% and 95% quantile
Sim.Quantiles <- lapply(X = Sim.Estimates, 
                        FUN = function(x) apply(X = x, 
                                                MARGIN = 1, 
                                                FUN = Quantile.fun))

#=======================================================================
# Delete in the final version 
#=======================================================================
all.equal(Sim.Means$Pop, BootSim.both$MeanEst$Pop)
all.equal(Sim.Means$Resp, BootSim.both$MeanEst$Resp)
### "Mean relative difference: 0.003203344"
### "Mean relative difference: 0.001747106"

all.equal(Sim.Medians$Pop, BootSim.both$MedianEst$Pop)
all.equal(Sim.Medians$Resp, BootSim.both$MedianEst$Resp)
### "Mean relative difference: 0.003362914"
### "Mean relative difference: 0.00116525"

all.equal(Sim.SE$Pop, BootSim.both$SE$Pop)
all.equal(Sim.SE$Resp, BootSim.both$SE$Resp)
### "Mean relative difference: 30.01675"
### "Mean relative difference: 30.39107"

# FOR LATER USE: testing for Standard error 
expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                     instrumentFormula = ~ Instrument, 
                                     data = SimData,
                                     effectType = 'Both')$SE$Pop, # default: effectType = 'Population'
             expected = Sim.SE$Pop,
             tolerance = 40)
# Standard error
expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                     instrumentFormula = ~ Instrument, 
                                     data = SimData,
                                     effectType = 'Both')$SE$Resp,
             expected = Sim.SE$Resp,
             tolerance = 40)

all.equal(Sim.Quantiles$Pop, BootSim.both$Quantiles$Pop)
all.equal(Sim.Quantiles$Resp, BootSim.both$Quantiles$Resp)
### "Mean relative difference: 0.009784695"
### "Mean relative difference: 0.00814859"

all.equal(Sim.Estimates$Pop, BootSim.both$Matrix$Pop)
all.equal(Sim.Estimates$Resp, BootSim.both$Matrix$Resp)
### "Mean relative difference: 0.1318351"
### "Mean relative difference: 0.09498287"
#=======================================================================



#==================
# Unit testing:
#===================
test_that("bootstrapDelta returns right mean values for population", {
  # does the function produce right values?
  # Mean estimate for treatment and control among population
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$MeanEst$Pop, 
               expected = Sim.Means$Pop)
  # Median estimate
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$MedianEst$Pop,
               expected = Sim.Medians$Pop)
  # Quantiles
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$Quantiles$Pop,
               expected = Sim.Quantiles$Pop)
})

test_that("bootstrapDelta returns right mean values for respondents", {
  # does the function produce right values?
  # Mean estimate  for treatment and control among respondents
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$MeanEst$Resp,
               expected = Sim.Means$Resp)
  # Median estimate
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$MedianEst$Resp,
               expected = Sim.Medians$Resp)
  # Quantiles
  expect_equal(object = bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                       instrumentFormula = ~ Instrument, 
                                       data = SimData,
                                       effectType = 'Both')$Quantiles$Resp,
               expected = Sim.Quantiles$Resp)
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
