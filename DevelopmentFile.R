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
# Create vector of covariates
# !!! We'll need to make this work with dataframes
X <- rnorm(100, 0, 3)
# Create vector of instruments
Z <- sample(1:5, 100, replace = T, prob = c(0.3, 0.2, 0.2, 0.2, 0.1))
# Create vector of Y values, related to D and X
Y <- 5*D + 2*X + rnorm(100, 0, 3)
# Create probabilistic attrtion vector
Attrition <- rnorm(100, Z, 5)
# Make sure that this isn't totally deterministic for Z == 5
order(Attrition, decreasing=TRUE)[1:15]
which(Z == 5)
# Change 10 largest attrition values in Y to NA
Y[order(Attrition, decreasing=TRUE)[1:15]] <- NA

# A dataframe of covariates for later use 
#X <- data.frame(Binary = sample(c(0, 1), 100, replace = T), 
#              Continuous = rnorm(100, 0, 3),
#              Factor = sample(c('a', 'b', 'c'), 100, replace = T))

# Test calculateWeights
MyGLMWeights <- calculateWeights(Y = Y, D = D, X = X, Z = Z)
MyGAMWeight <- calculateWeights(Y = Y, D = D, X = X, Z = Z, method = 'gam')
# Test weight in lm
summary(lm(Y ~ X + D, weights = MyWeights))


