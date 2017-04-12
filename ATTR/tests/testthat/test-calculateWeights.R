context("calculateWeights")



### Create simulation data, SimData:
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

SimData <- data.frame(Y,Treatment,Covariate,Instrument)

## Functions for fitting and working with generalized additive model are required.
library(gam)

## calculate weights without using the function:
TestData <- SimData
TestData[ ,1] <- as.numeric(!is.na(.subset2(TestData, 1))) # Recode Y as R
names(TestData)[1:2] <- c('R', 'D')
# pW: response propensity scores
# Regress R on X + Z; calculate fitted values
TestData$p_W_Fits <- predict(object = gam(formula = R ~ ., # Calculate model and then fitted value
                                          family = binomial(link = logit),
                                          data = TestData[ ,-2], # Since default formula is R ~ ., we remove D
                                          maxit = 1000),
                             newdata = TestData,
                             type = 'response')
TestData[TestData$D != 1, ]$p_W_Fits <- (1 - TestData)[TestData$D != 1, ]$p_W_Fits
# Pi: treatment propensity scores
# Regress D on X + Z
Test.Pi_Fits <- predict(object = gam(formula = D ~ ., # Calculate model and then fitted value
                                     family = binomial(link = logit),
                                     data = TestData[TestData$R == 1 , -1], # Since default formula is D ~ ., we remove R
                                                            # conditioning on R=1
                                     maxit = 1000),
                        newdata = TestData[TestData$R == 1 , -1],
                        type = 'response')
Test.Pi_Fits[TestData[TestData$R == 1, ]$D != 1] <- (1 - TestData$Pi_Fits)[TestData[TestData$R == 1, ]$D != 1]

# Product of response propensity scores and treatment propensity scores
Test.AllWeights <- TestData[TestData$R == 1, ]$p_W_Fits * Test.Pi_Fits

# Final output:
Test.weights <- list(pW = TestData$p_W_Fits, 
                     Pi = Test.Pi_Fits,
                     pWxPi = Test.AllWeights)
 



### Tests:        

test_that("calculateWeights produce right values", {
  # does the function produce right values?
  expect_equal(calculateWeights(modelData = SimData[,1:3], instrumentData = SimData[,4]),
               expected = Test.weights)
})

test_that("calculateWeights produce values within a right range", {
  # does the function produce values within a right range?
    expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
                                  instrumentData = SimData[,4])$pW, 
                             c(0.00001, 0.99999)))
    expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
                                  instrumentData = SimData[,4])$Pi, 
                             c(0.00001, 0.99999)))
    expect_true(1 %in% findInterval(calculateWeights(modelData = SimData[,1:3],
                                                     instrumentData = SimData[,4])$pWxPi, 
                                    c(0.000001, 0.999999)))
})

test_that("calculateWeights produce an output of right class", {
  # does the function produce an output of right class?
  expect_is(calculateWeights(SimData[,1:3], SimData[,4]), "list")
  expect_is(calculateWeights(SimData[,1:3], SimData[,4])$pW, "numeric")
  expect_is(calculateWeights(SimData[,1:3], SimData[,4])$Pi, "numeric")
  expect_is(calculateWeights(SimData[,1:3], SimData[,4])$pWxPi, "numeric")  
})

test_that("calculateWeights have right number of rows/columns", {
  # does the function have right number of columns?
  expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])), 
               expected = 3)
  expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$pW), 
               expected = nrow(SimData))
  expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$Pi), 
               expected = length(na.omit(SimData[,1])))
  expect_equal(length(calculateWeights(SimData[,1:3], SimData[,4])$pWxPi), 
               expected = length(na.omit(SimData[,1])))  
})

test_that("calculateWeights detect error", {
  # does the function detect error when it should?
  # case 1: incorrect number of subscripts on matrix
  expect_error(calculateWeights(SimData[,1], SimData[,4])) # modelData = only Y
  expect_error(calculateWeights(SimData[,c(1,3)], SimData[,4])) # modelData = (Y, Covariates)
  expect_error(calculateWeights(SimData[,4], SimData[,1:3])) # modelData = instrument, InstumentData = model
  # case 2: number of rows does not match
  expect_error(calculateWeights(SimData[1:(2/length(SimData)),1:3], SimData[,4]))  
})
