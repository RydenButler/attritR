library(devtools)
library(roxygen2)

# DO NOT RUN THE FOLLOWING LINE
#### package.skeleton('estimateDelta')

# Compile latest package and re-document
Current <- as.package('ATTR')
load_all(Current)
document(Current)

# Create vector of treatment indicators
Treatment <- sample(c(0, 1), 5000 , replace = T)
# A dataframe of covariates for later use 
Covariates <- data.frame(Binary = sample(x = c(0, 1), 
                                         size = length(Treatment), 
                                         replace = T), 
                         Continuous = rnorm(n = length(Treatment), 
                                            mean = 10, 
                                            sd = 3))
# Create vector of instruments
Instrument <- data.frame(Z1 = sample(x = 1:5, 
                                     size = length(Treatment), 
                                     replace = T, 
                                     prob = c(0.3, 0.2, 0.2, 0.2, 0.1)))
# Create vector of Y values, related to D and X
Outcome <- 3*Treatment + 1*Covariates[,1] + 2*Covariates[,2]
# Create probabilistic attrition vector
Attrition <- sapply(1:length(Treatment), 
                    function(x) rbinom(n = 1, 
                                       size = 1, 
                                       prob = (5*(Treatment[x] + 1) - Instrument[[1]][x])/10))
Outcome[as.logical(Attrition)] <- NA
# MAR missingness in X
Covariates <- as.data.frame(lapply(Covariates, 
                                   function(x) x[sample(x = c(TRUE,NA), 
                                                        size = length(x),
                                                        prob = c(0.95, 0.05),
                                                        replace = TRUE)]))

# Bind vectors together into single dataset
NoXData <- data.frame(cbind(Outcome, Treatment, Instrument))
FullData <- data.frame(cbind(Outcome, Treatment, Covariates, Instrument))

lm(Outcome ~ Treatment + Binary + Continuous, data = FullData)

### Check Proposition 4:

# weights
Weights4 <- calculateWeights(modelData = FullData[,-ncol(FullData)], 
                             instrumentData = FullData[,ncol(FullData)])$ATTWeights

# delta
Delta4 <- estimateDelta(Outcome ~ Treatment + Binary + Continuous, 
                        instrumentFormula = ~ Z1, 
                        data = FullData)$ATT

# bootstrap
Boot4 <- bootstrapDelta(Outcome ~ Treatment + Binary + Continuous, 
                        ~ Z1, 
                        FullData)

### Check Proposition 5:

# weights
Weights5 <- calculateWeights(modelData = FullData[,-ncol(FullData)], 
                             instrumentData = FullData[,ncol(FullData)])$ATEWeights

# delta
Delta5 <- estimateDelta(Outcome ~ Treatment + Binary + Continuous, 
                       instrumentFormula = ~ Z1, 
                       data = FullData)$ATE

# bootstrap
Boot5 <- bootstrapDelta(Outcome ~ Treatment + Binary + Continuous, 
                            ~ Z1, 
                            FullData)

### Check Proposition 6:

# weights
Weights6 <- calculateWeights(modelData = NoXData[,-ncol(NoXData)],
                             instrumentData = as.data.frame(NoXData[,ncol(NoXData)]))

# delta
Delta6 <- estimateDelta(Outcome ~ Treatment, 
                        instrumentFormula = ~ Z1, 
                        data = NoXData)

# bootstrap
Boot6 <- bootstrapDelta(Outcome ~ Treatment, 
                        ~ Z1, 
                        NoXData)
