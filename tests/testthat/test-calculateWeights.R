context("calculateWeights")
context("probabilityFits")

data("SimulatedAttrition")

# For testing probabilityFits function:
# Calculate predicted probabilities for fitted model
Response <- !is.na(SimulatedAttrition$Y)

Test.probabilityFits <- predict(object = glm(formula = Response ~ D + X + Z, 
                                             family = binomial(link = logit),
                                             data = SimulatedAttrition,
                                             maxit = 1000), type = 'response')

# Manually calculate weights

# pW: response propensity scores
# Regress 'Response(R)' on 'Covariate(X) + Instrument(Z)'; calculate fitted values
# gam == glm coefficients?
Test.pW <- predict(object = glm(formula = Response ~ D + X + Z, 
                                family = binomial(link = logit),
                                data = SimulatedAttrition,
                                maxit = 1000), 
                   type = 'response')
Test.pW[SimulatedAttrition$D != 1] <- (1 - Test.pW[SimulatedAttrition$D != 1])

# Pi: treatment propensity scores
# Regress 'Treatment(D)' on 'Covariates(X) + p_W_Fits(response propensity scores)'
Test.Pi <- predict(object = glm(formula = D ~ X + Test.pW, 
                                family = binomial(link = logit),
                                data = SimulatedAttrition,
                                maxit = 1000),
                   type = 'response')
Test.Pi[SimulatedAttrition$D != 1] <- (1 - Test.Pi[SimulatedAttrition$D != 1])

# Product of response propensity scores and treatment propensity scores
Test.pWxPi <- Test.pW* Test.Pi

# Unit Testing ###############################################
# ============================================================

# Test probabilityFits
test_that("probabilityFits returns correct values", {
  # testing for probabilityFits
  expect_equal(probabilityFits(formula = !is.na(Y) ~., 
                               modelData = SimulatedAttrition, 
                               method = binomial(link = logit)),
               Test.probabilityFits)
  })

test_that("calculateWeights returns correct values", {
  # testing for pW value
  expect_equivalent(calculateWeights(modelData = SimulatedAttrition[ , 1:3],
                                     instrumentData = SimulatedAttrition[ , 4])$pW,
                    expected = Test.pW)
  # testing Pi value
  expect_equal(calculateWeights(modelData = SimulatedAttrition[ , 1:3], 
                                instrumentData = SimulatedAttrition[ , 4])$Pi,
               expected = Test.Pi)
  # testing pWxPi value
  expect_equal(calculateWeights(modelData = SimulatedAttrition[ , 1:3], 
                                instrumentData = SimulatedAttrition[ , 4])$pWxPi,
               expected = Test.pWxPi)
  })



# SAVED TESTS FOR LATER TESTS ---------------------------------------------------------------
# 
# test_that("test that calculateWeights generates correct length of vectors", {
#   expect_equal(length(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$pW),
#     expected = length(Test.WeightList$pW))
#   expect_equal(length(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$Pi),
#     expected = length(Test.WeightList$Pi))
#   expect_equal(length(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$pWxPi),
#     expected = length(Test.WeightList$pW))
# })
# 
# test_that("calculateWeights produce values within a right range", {
#   # does the function produce values within a right range?
#     expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
#                                   instrumentData = SimData[,4])$pW, 
#                              c(0.00001, 0.99999)))
#     expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
#                                   instrumentData = SimData[,4])$Pi, 
#                              c(0.00001, 0.99999)))
#     expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
#                                                      instrumentData = SimData[,4])$pWxPi, 
#                                     c(0.000001, 0.999999)))
# })
# 
# test_that("calculateWeights produce an output of right class", {
#   # does the function produce an output of right class?
#   expect_is(calculateWeights(SimData[,1:3], SimData[,4]), "list")
#   expect_is(calculateWeights(SimData[,1:3], SimData[,4])$pW, "numeric")
#   expect_is(calculateWeights(SimData[,1:3], SimData[,4])$Pi, "numeric")
#   expect_is(calculateWeights(SimData[,1:3], SimData[,4])$pWxPi, "numeric")  
# })
# 
# test_that("calculateWeights have right number of rows/columns", {
#   # does the function have right number of columns?
#   expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])), 
#                expected = 3)
#   expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$pW), 
#                expected = nrow(SimData))
#   expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$Pi), 
#                expected = length(na.omit(SimData[,1])))
#   expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$pWxPi), 
#                expected = length(na.omit(SimData[,1])))  
# })
# 
# test_that("calculateWeights detect error", {
#   # does the function detect error when it should?
#   # case 1: incorrect number of subscripts on matrix
#   expect_error(calculateWeights(SimData[,1], SimData[,4])) # modelData = only Y
#   expect_error(calculateWeights(SimData[,c(1,3)], SimData[,4])) # modelData = (Y, Covariates)
#   expect_error(calculateWeights(SimData[,4], SimData[,1:3])) # modelData = instrument, InstumentData = model
#   # case 2: number of rows does not match
#   expect_error(calculateWeights(SimData[1:(2/length(SimData)),1:3], SimData[,4]))  
# })
