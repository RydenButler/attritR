context("estimateDelta")

## Estimating delta without using the estimateDelta function:

### Create simulation data, SimData:
library(MASS)
set.seed(12345)

Treatment <- rbinom(n = 1000, size = 1, prob = 0.5)
Covariate <- runif(n = length(Treatment), min = -1, max = 1)
Instrument <- runif(n = length(Treatment), min = -1, max = 2)

# For attrition on unobservables
UV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
U <- UV[ , 1]
V <- UV[ , 2]

Y <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + U
R <- 1*Treatment + 1*Covariate + 0*Instrument + V > 0
Y[!R] <- NA

SimData <- data.frame(Y,Treatment,Covariate,Instrument)

## Functions for fitting and working with generalized additive model are required.
library(gam)


TestData <- SimData

# Calculate weights:
TestData[ ,1] <- ifelse(is.na(TestData[,1]) == TRUE, 0, 1) 
names(TestData)[1] <- c('Response')
TestData$p_W_Fits <- predict(object = glm(formula = Response ~ Treatment + Covariate + Instrument, 
                                          family = binomial(link = logit),
                                          data = TestData,
                                          maxit = 1000),
                             newdata = TestData,
                             type = 'response')
TestData[TestData$Treatment == 0, ]$p_W_Fits <- (1 - TestData)[TestData$Treatment==0, ]$p_W_Fits
Test.Pi_Fits <- predict(object = glm(formula = Treatment ~ Covariate + p_W_Fits,
                                     family = binomial(link = logit),
                                     data = TestData,
                                     maxit = 1000),
                        newdata = TestData,
                        type = 'response')
Test.Pi_Fits[TestData$Treatment== 0] <- (1 - Test.Pi_Fits[TestData$Treatment==0])
Test.AllWeights <- TestData$p_W_Fits * Test.Pi_Fits
Test.WeightList <- list(pW = TestData$p_W_Fits, 
                        Pi = Test.Pi_Fits,
                        pWxPi = Test.AllWeights)

# add estimated 'Pi'and 'pWxPi" to TestData
TestData$Pi <- Test.WeightList$Pi
TestData$pWxPi <- Test.WeightList$pWxPi
  
# Estimate Proposition 4:
Test.RespondentModel <- lm(formula = Y ~ Treatment + Covariate,
                           weights = 1/Pi,
                           data = TestData)

# Estimate Proposition 5:
Test.PopulationModel <- lm(formula = Y ~ Treatment + Covariate,
                    weights = 1/pWxPi,
                    data = TestData)

# Final output of estimateDelta:  
Test.Delta <- list(RespondentDelta = Test.RespondentModel,
                   PopulationDelta = Test.PopulationModel)


# Unit Testing ###############################################
# ============================================================

# test estimated values for RespondentDelta
test_that("estimateDelta returns correct values for RespondentDelta", {
  # test estimated intercept value
  expect_equal(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
                                    instrumentFormula = ~ Instrument, data = TestData)$RespondentDelta)[1],
               expected = unlist(Test.Delta$RespondentDelta)[1], tolerance = 0.00001)
  # test estimated treatment value
  expect_equal(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
    instrumentFormula = ~ Instrument, data = TestData)$RespondentDelta)[2],
    expected = unlist(Test.Delta$RespondentDelta)[2], tolerance = 0.00001)
  # test estimated covariate values
  expect_equal(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
    instrumentFormula = ~ Instrument, data = TestData)$RespondentDelta)[3],
    expected = unlist(Test.Delta$RespondentDelta)[3], tolerance = 0.00001)
})

# test estimated values for AllDelta
test_that("estimateDelta returns correct values for PopulationDelta", {
  # test estimated intercept value
  expect_equal(as.numeric(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
                                    instrumentFormula = ~ Instrument, 
                                    data = TestData)$PopulationDelta)[1]),
               expected = as.numeric(unlist(Test.Delta$PopulationDelta)[1]), tolerance = 0.00001)
  # test estimated treatment value
  expect_equal(as.numeric(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
                                    instrumentFormula = ~ Instrument, 
                                    data = TestData)$PopulationDelta)[2]),
               expected = as.numeric(unlist(Test.Delta$PopulationDelta)[2]), tolerance = 0.00001)
  # test estimated covariate value
  expect_equal(as.numeric(unlist(estimateDelta(regressionFormula = Y ~ Treatment + Covariate, 
                                               instrumentFormula = ~ Instrument, 
                                               data = TestData)$PopulationDelta)[3]),
               expected = as.numeric(unlist(Test.Delta$PopulationDelta)[3]), tolerance = 0.00001)
})


# STEPS BELOW SAVED FOT LATER TESTS ------------------------------------------
# 
# test_that("estimateDelta produce an output of right class", {
#   # does the function produce an output of right class?
#   expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                           instrumentFormula = ~ Instrument,
#                           data = SimData), 
#             class = "list")
#   expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                           instrumentFormula = ~ Instrument,
#                           data = SimData)$RespondentDelta, 
#             class = "lm")
#   expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                           instrumentFormula = ~ Instrument,
#                           data = SimData)$AllDelta, 
#             class = "lm")
# })
# 
# test_that("estimateDelta have right number of elements", {
#   # does the function produce both RespondentDelta and AllDelta?
#   expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                     instrumentFormula = ~ Instrument,
#                                     data = SimData)), 
#                expected = 2)
#   # does the function produce right number of elements: coefficients?  
#   expect_equal(length(coef(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                          instrumentFormula = ~ Instrument,
#                                          data = SimData)$RespondentDelta)), 
#                expected = ncol(SimData[,-ncol(SimData)]))
#   expect_equal(length(coef(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                          instrumentFormula = ~ Instrument,
#                                          data = SimData)$AllDelta)), 
#                expected = ncol(SimData[,-ncol(SimData)]))
#   # number of residuals/fitted values
#   expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                     instrumentFormula = ~ Instrument,
#                                     data = SimData)$RespondentDelta[[2]]),
#                expected = length(na.omit(SimData[,1])))
#   expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                     instrumentFormula = ~ Instrument,
#                                     data = SimData)$AllDelta[[2]]),
#                expected = length(na.omit(SimData[,1])))
#   expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                     instrumentFormula = ~ Instrument,
#                                     data = SimData)$RespondentDelta[[3]]),
#                expected = length(na.omit(SimData[,1])))
#   expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
#                                     instrumentFormula = ~ Instrument,
#                                     data = SimData)$AllDelta[[3]]),
#                expected = length(na.omit(SimData[,1])))
# })
# 
# test_that("estimateDelta detect error", {
#   # does the function detect error when it should?
#   expect_error(estimateDelta(regressionFormula = Y ~ Covariate, # Omit Treatment
#                              instrumentFormula = ~ Instrument,
#                              data = SimData))
# })
