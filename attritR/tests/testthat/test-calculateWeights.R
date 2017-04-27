context("calculateWeights")
context("probabilityFits")
context("Proposition1")
context("Proposition2")
context("Proposition3")

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

## Calculate weights without using the calculateWeights function:
# Extract model data given formula
TestData <- SimData
TestData <- model.frame(Y ~ Treatment + Covariate, TestData, na.action = NULL)

# Extract instrument data given formula
Test.InstrumentData <- model.frame(~ Instrument, TestData, na.action = NULL)

# Recode Y as a form of R, binary response variable 
# which is 1 if Y is observed (non-attrition) and 0 otherwise (attrition)
TestData[ ,1] <- ifelse(is.na(TestData[,1]) == TRUE, 0, 1) 
names(TestData)[1] <- c('Response') # rename Y as Response

# pW: response propensity scores
# Regress 'Response(R)' on 'Covariate(X) + Instrument(Z)'; calculate fitted values
# gam == glm coefficients?
TestData$p_W_Fits <- predict(object = glm(formula = Response ~ Treatment + Covariate + Instrument, 
                                          # Calculate model and then fitted value
                                          family = binomial(link = logit),
                                          data = TestData,
                                          maxit = 1000),
                             newdata = TestData,
                             type = 'response')
TestData[TestData$Treatment != 1, ]$p_W_Fits <- (1 - TestData)[TestData$Treatment != 1, ]$p_W_Fits

# Pi: treatment propensity scores
# Regress 'Treatment(D)' on 'Covariates(X) + p_W_Fits(response propensity scores)'
Test.Pi_Fits <- predict(object = glm(formula = Treatment ~ Covariate + p_W_Fits, 
                                     # Calculate model and then fitted value
                                     family = binomial(link = logit),
                                     data = TestData, # Since default formula is D ~ ., we remove R
                                                      # conditioning on R=1
                                     maxit = 1000),
                        newdata = TestData,
                        type = 'response')
Test.Pi_Fits[TestData$Treatment != 1] <- (1 - Test.Pi_Fits[TestData$Treatment != 1])

# Product of response propensity scores and treatment propensity scores
Test.AllWeights <- TestData$p_W_Fits * Test.Pi_Fits

# Final output for calculateWeights function:
Test.WeightList <- list(pW = TestData$p_W_Fits, 
                        Pi = Test.Pi_Fits,
                        pWxPi = Test.AllWeights)
 
# For testing probabilityFits function:
# Calculate predicted probabilities for fitted model
Test.probabilityFits <- predict(object = gam(formula = Response ~., 
                                             family = binomial(link = logit),
                                             data = TestData,
                                             maxit = 1000),
                                newdata = TestData, type = 'response')


# Unit Testing ###############################################
# ============================================================

test_that("calculateWeights returns correct values", {
  # testing for pW value
  expect_equal(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$pW,
    expected = Test.WeightList$pW, tolerance = 0.00001)
  # testing Pi value
  expect_equal(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$Pi,
    expected = Test.WeightList$Pi, tolerance = 0.00001)
  # testing pWxPi value
  expect_equal(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4])$pWxPi,
    expected = Test.WeightList$pWxPi, tolerance = 0.00001)
})

test_that("probabilityFits returns correct values", {
  # testing for probabilityFits
  expect_equal(probabilityFits(formula = Response ~., 
                               modelData = TestData, 
                               method = binomial(link = logit)
                               ),
               Test.probabilityFits)
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
