context("bootstrapDelta")

## Create simulation data:
library(MASS)
set.seed(12345)

Treatment <- rbinom(n = 1000, size = 1, prob = 0.5)
Covariate <- runif(n = length(Treatment), min = -1, max = 1)
Instrument <- runif(n = length(Treatment), min = -1, max = 2)

# For attrition on observables
UV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0, 0, 1), nrow = 2))
U <- UV[ , 1]
V <- UV[ , 2]

Y <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + U
R <- 1*Treatment + 1*Covariate + 0*Instrument + V > 0
Y[!R] <- NA

SimData.obs <- data.frame(Y, Treatment, Covariate, Instrument)

# For attrition on unobservables
unUV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
unU <- unUV[ , 1]
unV <- unUV[ , 2]

unY <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + unU
unR <- 0.5*Treatment + 0.5*Covariate + 1*Instrument + unV > 0
unY[!unR] <- NA

SimData.unobs <- data.frame(unY, Treatment, Covariate, Instrument)


## Functions for fitting and working with generalized additive model are required.
library(gam)

# Calculate weights for SimData.obs, SimData.unobs:
Test.Weightlist.obs <- calculateWeights(modelData = SimData.obs,
                                        instrumentData = Instrument,
                                        p_W_Formula = R ~.,
                                        p_W_Method = binomial(link = logit),
                                        PiFormula = D ~.,
                                        PiMethod = binomial(link = logit))

Test.Weightlist.unobs <- calculateWeights(modelData = SimData.unobs,
                                          instrumentData = Instrument,
                                          p_W_Formula = R ~.,
                                          p_W_Method = binomial(link = logit),
                                          PiFormula = D ~.,
                                          PiMethod = binomial(link = logit))


# Check observables
BootSim.obs.both <- bootstrapDelta(regressionFormula = Y ~ Treatment + Covariate, 
                           instrumentFormula = ~ Instrument, 
                           data = SimData.obs,
                           effectType = 'Both') # default: effectType ='Population'
BootSim.obs.both$MeanEst

Sim.regressionFormula.obs <- lm(Y ~ Treatment + Covariate)
Sim.regressionFormula.obs

# Check unobservables
BootSim.unobs.both <- bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                                     instrumentFormula = ~ Instrument, 
                                     data = SimData.unobs,
                                     effectType = 'Both') # default: effectType ='Population'
BootSim.unobs.both$MeanEst
Sim.regressionFormula.unobs <- lm(unY ~ Treatment + Covariate)
Sim.regressionFormula.unobs

Sim.instrumentFormula <- Instrument
Sim.p_W_Formula <- 
Sim.p_W_Method <- 

    
# Make cluster
Sim.nCores <- 1
Sim.varlist <- c('Sim.regressionFormula.obs', 
                 'Sim.instrumentFormula', 
                 'data',
                 'p_W_Formula',
                 'p_W_Method',
                 'PiFormula',
                 'PiMethod',
                 'nBoots',
                 'quantiles',
                 'effectType',
                 'estimateDelta',
                 'calculateWeights',
                 'probabilityFits',
                 'gam')
Sim.BootsCluster <- makeCluster(Sim.nCores)
clusterExport(cl = Sim.BootsCluster, 
              varlist = Sim.varlist, 
              envir = environment())

# Bootstrap data: random sampling of dataset with replacement
BootsList <- parLapply(Sim.BootsCluster, 
                       X = 1:nBoots, 
                       fun = function(x) SimData.obs[sample(x = nrow(SimData.obs),
                                                         size = nrow(SimData.obs),
                                                         replace = TRUE), ]
                       )
Sim.Estimates <- list()


Estimates <- if(effectType == 'Respondent' | effectType == 'Both'){
              Estimates$Resp <- parSapply(cl = Sim.BootsCluster, 
                                          X = BootsList, 
                                          FUN = function(x){
                                                estimateDelta(regressionFormula = regressionFormula,
                                                instrumentFormula = instrumentFormula,
                                                data = x})$RespondentDelta$coefficients
  )} if(effectType == 'Population' | effectType == 'Both'){
      Estimates$Pop <- parSapply(cl = Sim.BootsCluster, 
                                 X = BootsList, 
                                 FUN = function(x){
                                   estimateDelta(regressionFormula = regressionFormula,
                                   instrumentFormula = instrumentFormula,
                                   data = x})$PopulationDelta$coefficients)

    # Calculate results: mean, median, and standard errors, based on bootstrapped replications
    SEs <- lapply(Estimates, function(x) apply(x, 1, sd))
    Means <- lapply(Estimates, rowMeans)
    Medians <- lapply(Estimates, function(x) apply(x, 1, median))
    # Note that the bootstrapping results in some NA coefficient estimates (colinearity? too much missingness?)
    # As a result, na.rm is required here for the quantiles (though strangely, not for the other functions)
    # This is unnecessary with larger sample sizes, and may disappear with additional noise
    # All efforts should be taken to have na.rm == F, as wanton removal of NAs can gloss over major errors
    # If na.rm must be true, we should include a warning message if NAs are found in the CoefMatrix
    # Additionally we may want an error thrown if the number of NAs exceeds some tolerable threshold
    # Currently the NA problem appears more frequently (possibly exclusively) when calculating the ATE
    Quantiles <- lapply(Estimates, function(x) apply(x, 1, function(y) quantile(y, quantiles, na.rm=T)))
    # Stopping the cluster
    stopCluster(BootsCluster)
    # return list with mean, median, and standard error of estimated for treatment and control
    return(list(MeanEst = Means, 
                MedianEst = Medians, 
                SE = SEs,
                Quantiles = Quantiles,
                Matrix = Estimates)    )





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
