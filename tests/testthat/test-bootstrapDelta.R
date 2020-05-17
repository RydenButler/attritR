context("bootstrapDelta")

data('SimulatedAttrition')

# Run function to generate bootstraps and estimates
TestBoot <- bootstrapDelta(Y ~ D + X, 
                       instrumentFormula = ~ Z, 
                       data = SimulatedAttrition,
                       effectType = 'Both',
                       nCores = 4)

# Manually estimate delta from each bootstrapped dataset
Test.Estimates <- lapply(X = TestBoot$Data, 
                         FUN = function(x) estimateDelta(regressionFormula = Y ~ D + X,
                                                         instrumentFormula = ~ Z,
                                                         data = x))
# Calculate relevant statistics for respondent delta
Test.Respondent <- sapply(X = Test.Estimates,
                           FUN = function(x) coef(x$RespondentDelta))

Mean.Respondent <- rowMeans(Test.Respondent)

Median.Respondent <- apply(Test.Respondent, 1, median)

SE.Respondent <- apply(Test.Respondent, 1, sd)

Quant.Respondent <- apply(Test.Respondent, 1, function(x) quantile(x, c(0.05, 0.95)))

# Calculate relevant statistics for population delta
Test.Population <- sapply(X = Test.Estimates,
                           FUN = function(x) coef(x$PopulationDelta))

Mean.Population <- rowMeans(Test.Population)

Median.Population <- apply(Test.Population, 1, median)
  
SE.Population <- apply(Test.Population, 1, sd)

Quant.Population <- apply(Test.Population, 1, function(x) quantile(x, c(0.05, 0.95)))


#==================
# Unit testing:
#===================

# Unit testing for POPULATION estimates ---------------------------------
test_that("bootstrapDelta returns correct values for population", {
  expect_equal(TestBoot$Means$Pop, 
               expected = Mean.Population)
  expect_equal(TestBoot$Medians$Pop, 
               expected = Median.Population)
  expect_equal(TestBoot$SD$Pop, 
               expected = SE.Population)
  expect_equal(TestBoot$Quants$Pop, 
               expected = Quant.Population)
  })

# Unit testing for RESPONDENT estimates ---------------------------------

# Testing mean values
test_that("bootstrapDelta returns correct mean values for respondents", {
  expect_equal(TestBoot$Means$Resp, 
               expected = Mean.Respondent)
  expect_equal(TestBoot$Medians$Resp, 
               expected = Median.Respondent)
  expect_equal(TestBoot$SD$Resp, 
               expected = SE.Respondent)
  expect_equal(TestBoot$Quants$Resp, 
               expected = Quant.Respondent)
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
