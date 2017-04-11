context("calculateWeights")
context("probabilityFits")

# context("Proposition1")
# context("Proposition2")
# context("Proposition3")


## Create simulation data, SimData:
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

SimData <- data.frame(Y,Treatment,Covariate)
SimInstrument <- data.frame(Instrument)

## Functions for fitting and working with generalized additive model are required.
library(gam)

## tests:
test_that("calculateWeights produce values within a right range", {
  # does the function produce values within a right range?
    expect_true(1 %in% findInterval(calculateWeights(modelData = SimData,
                                  instrumentData = SimInstrument)$RespondentWeights, 
                             c(0,1)))
    expect_true(1 %in% findInterval(calculateWeights(modelData = SimData,
                                  instrumentData = SimInstrument)$AllWeights, 
                             c(0.01, 0.99)))
})

test_that("calculateWeights produce an output of right class", {
  # does the function produce an output of right class?
  expect_is(calculateWeights(SimData, SimInstrument), "list")
  expect_is(calculateWeights(SimData, SimInstrument)$RespondentWeights, "numeric")
  expect_is(calculateWeights(SimData, SimInstrument)$AllWeights, "numeric")
})

test_that("calculateWeights have right number of rows/columns", {
  # does the function have right number of columns?
  expect_equal(length(calculateWeights(SimData, SimInstrument)), 
               expected = 2)
  expect_equal(length(calculateWeights(SimData, SimInstrument)$RespondentWeights), 
               expected = nrow(SimData))
  expect_equal(length(calculateWeights(SimData, SimInstrument)$AllWeights), 
               expected = nrow(SimData))
})

test_that("calculateWeights detect error", {
  # does the function detect error when it should?
  expect_error(calculateWeights(modelData = data.frame(Instrument), 
                                instrumentData = SimData))
  expect_error(calculateWeights(modelData = Instrument, 
                                instrumentData = SimData))
  expect_error(calculateWeights(modelData = SimData[1:(2/nrow(SimData)), ], 
                                instrumentData = SimInstrument))
  expect_error(calculateWeights(modelData = data.frame(Y),
                                instrumentData = SimInstrument))
# expect_error(calculateWeights(modelData = data.frame(Y, Treatment),
#                                instrumentData = SimInstrument)) ### Check: did not throw an error.
  expect_error(calculateWeights(modelData = data.frame(Y, Covariates),
                                instrumentData = SimInstrument))
})
