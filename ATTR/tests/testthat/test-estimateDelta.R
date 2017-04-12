context("estimateDelta")


## Estimating delta without using the estimateDelta function:

# Extract model data given formula
TestData.full <- SimData
TestData.full <- model.frame(Y ~ Treatment + Covariate, TestData.full, na.action = NULL)
# Extract instrument data given formula
TestInstrumentData <- model.frame(~ Instrument, TestData.full, na.action = NULL)
  
# Calculate weights; add this to data b/c lm() won't recognize the object otherwise
Test.WeightList <- calculateWeights(modelData = TestData.full, 
                                    instrumentData = TestInstrumentData)
TestData.full <- TestData.full[!is.na(TestData.full[ , 1]), ]
TestData.full$Pi <- Test.WeightList$Pi
TestData.full$pWxPi <- Test.WeightList$pWxPi
  
# Estimate Proposition 4:
Test.RespondentModel <- lm(formula = Y ~ Treatment + Covariate,
                           weights = Pi,
                           data = TestData.full)
# Estimate Proposition 5:
Test.AllModel <- lm(formula = Y ~ Treatment + Covariate,
                    weights = pWxPi,
                    data = TestData.full)
# Final output of estimateDelta:  
Test.Delta <- list(RespondentDelta = Test.RespondentModel,
                   AllDelta = Test.AllModel)


## tests:
test_that("estimateDelta produce right values", {
  # does the function produce right values?
  expect_equal(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                             instrumentFormula = ~ Instrument,
                             data = SimData),
               expected = Test.Delta
  )
})

test_that("estimateDelta produce an output of right class", {
  # does the function produce an output of right class?
  expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                          instrumentFormula = ~ Instrument,
                          data = SimData), 
            class = "list")
  expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                          instrumentFormula = ~ Instrument,
                          data = SimData)$RespondentDelta, 
            class = "lm")
  expect_is(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                          instrumentFormula = ~ Instrument,
                          data = SimData)$AllDelta, 
            class = "lm")
})

test_that("estimateDelta have right number of elements", {
  # does the function produce both RespondentDelta and AllDelta?
  expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                    instrumentFormula = ~ Instrument,
                                    data = SimData)), 
               expected = 2)
  # does the function produce right number of elements: coefficients?  
  expect_equal(length(coef(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                         instrumentFormula = ~ Instrument,
                                         data = SimData)$RespondentDelta)), 
               expected = ncol(SimData[,-ncol(SimData)]))
  expect_equal(length(coef(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                         instrumentFormula = ~ Instrument,
                                         data = SimData)$AllDelta)), 
               expected = ncol(SimData[,-ncol(SimData)]))
  # number of residuals/fitted values
  expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                    instrumentFormula = ~ Instrument,
                                    data = SimData)$RespondentDelta[[2]]),
               expected = length(na.omit(SimData[,1])))
  expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                    instrumentFormula = ~ Instrument,
                                    data = SimData)$AllDelta[[2]]),
               expected = length(na.omit(SimData[,1])))
  expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                    instrumentFormula = ~ Instrument,
                                    data = SimData)$RespondentDelta[[3]]),
               expected = length(na.omit(SimData[,1])))
  expect_equal(length(estimateDelta(regressionFormula = Y ~ Treatment + Covariate,
                                    instrumentFormula = ~ Instrument,
                                    data = SimData)$AllDelta[[3]]),
               expected = length(na.omit(SimData[,1])))
})

test_that("estimateDelta detect error", {
  # does the function detect error when it should?
  expect_error(estimateDelta(regressionFormula = Y ~ Covariate, # Omit Treatment
                             instrumentFormula = ~ Instrument,
                             data = SimData))
})
