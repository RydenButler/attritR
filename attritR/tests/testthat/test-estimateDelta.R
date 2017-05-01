context("estimateDelta")

data('SimulatedAttrition')

# Calculate weights
Weights <- calculateWeights(modelData = SimulatedAttrition[,1:3], 
                       instrumentData = SimulatedAttrition[ , 4])
  
# Estimate Proposition 4:
Test.RespondentModel <- lm(formula = Y ~ D + X,
                           weights = 1/Weights$Pi,
                           data = SimulatedAttrition)

# Estimate Proposition 5:
Test.PopulationModel <- lm(formula = Y ~ D + X,
                    weights = 1/Weights$pWxPi,
                    data = SimulatedAttrition)

# Unit Testing ###############################################
# ============================================================

# test estimates for Respondent ATE
test_that("estimateDelta returns correct values for RespondentDelta", {
  expect_equal(coef(estimateDelta(regressionFormula = Y ~ D + X, 
                                  instrumentFormula = ~ Z, 
                                  data = SimulatedAttrition)$RespondentDelta),
               expected = coef(Test.RespondentModel))
  })

# test estimates for Population ATE
test_that("estimateDelta returns correct values for PopulationDelta", {
  expect_equal(coef(estimateDelta(regressionFormula = Y ~ D + X, 
                                  instrumentFormula = ~ Z, 
                                  data = SimulatedAttrition)$PopulationDelta),
               expected = coef(Test.PopulationModel))
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
