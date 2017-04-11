context("bootstrapDelta")

## Create simulation data:
# For attrition on unobservables
unUV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
unU <- unUV[ , 1]
unV <- unUV[ , 2]

unY <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + unU
unR <- 0.5*Treatment + 0.5*Covariate + 1*Instrument + unV > 0
unY[!unR] <- NA

# Check observables
BootSim <- bootstrapDelta(regressionFormula = Y ~ Treatment + Covariate, 
                          instrumentFormula = ~ Instrument, 
                          data = SimData)
BootSim$MeanEst
BootSim2 <- bootstrapDelta(regressionFormula = Y ~ Treatment + Covariate, 
                           instrumentFormula = ~ Instrument, 
                           data = SimData,
                           effectType = 'All')
BootSim2$MeanEst
lm(Y ~ Treatment + Covariate)

# Check unobservables
BootSim3 <- bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                           instrumentFormula = ~ Instrument, 
                           data = SimData)
BootSim4 <- bootstrapDelta(regressionFormula = unY ~ Treatment + Covariate, 
                           instrumentFormula = ~ Instrument, 
                           data = SimData,
                           effectType = 'All')
lm(unY ~ Treatment + Covariate)


## tests:
test_that("bootstrapDelta produce right values", {
  # does the function produce right values?
  expect_equal(object = ,
               expected = 
  )
  expect_equal(object = ,
               expected = 
  )
})

test_that("bootstrapDelta produce an output of right class", {
  # does the function produce an output of right class?
  expect_is(object = , 
            class = "list")
})

test_that("bootstrapDelta have right number of elements", {
  # does the function produce ...?
  expect_equal(object = , 
               expected = )

})

test_that("bootstrapDelta detect error", {
  # does the function detect error when it should?
  expect_error()
})
