library(devtools)
library(roxygen2)

# DO NOT RUN THE FOLLOWING LINE
#### package.skeleton('estimateDelta')

# Compile latest package and re-document
Current <- as.package('estimateDelta')
load_all(Current)
document(Current)

# This function should add dependencies to DESCRIPTION, but it doesn't
#devtools::use_package('stats')

# Create vector of treatment indicators
D <- sample(c(0, 1), 100 , replace = T)
# A dataframe of covariates for later use 
X <- data.frame(Binary = sample(c(0, 1), 100, replace = T), 
                Continuous = rnorm(100, 0, 3))
# Create vector of instruments
Z <- data.frame(A = sample(1:5, 100, replace = T, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)))
# Create vector of Y values, related to D and X
Y <- 5*D + 1*X[,1] + 2*X[,2] + rnorm(100, 0, 3)
# Create probabilistic attrtion vector
Attrition <- rnorm(100, Z[,1], 5)
# Make sure that this isn't totally deterministic for Z == 5
order(Attrition, decreasing=TRUE)[1:15]
which(Z == 5)
# Change 10 largest attrition values in Y to NA
Y[order(Attrition, decreasing=TRUE)[1:15]] <- NA

# Bind vectors together into single dataset
MyData <- data.frame(cbind(Y, D, X, Z))



# Test calculateWeights
MyGLMWeights <- calculateWeights(modelData = MyData[,-ncol(MyData)], 
                                 instrumentData = Z)
# Test GAM weights
MyGAMWeights <- calculateWeights(modelData = MyData[,-ncol(MyData)], 
                                 instrumentData = Z, 
                                 method = 'gam')

# Test estimateDelta
estimateDelta(Y ~ D + Binary + Continuous, instrumentFormula = ~ A, data = MyData)

# Test bootstrapDelta
bootstrapDelta(Y ~ D + Binary + Continuous, ~ A, MyData)