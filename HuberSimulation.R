library(MASS)

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
# For attrition on unobservables
unUV <- mvrnorm(n = 1000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
unU <- unUV[ , 1]
unV <- unUV[ , 2]

unY <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + unU
unR <- 0.5*Treatment + 0.5*Covariate + 1*Instrument + unV > 0
unY[!unR] <- NA

SimData <- data.frame(Y,Treatment,Covariate,Instrument)

# Check observables
BootSim <- bootstrapDelta(Y ~ Treatment + Covariate, 
                        ~ Instrument, 
                        SimData)
BootSim2 <- bootstrapDelta(Y ~ Treatment + Covariate, 
                          ~ Instrument, 
                          SimData,
                          effectType = 'All')
lm(Y ~ Treatment + Covariate)

# Check unobservables
BootSim3 <- bootstrapDelta(unY ~ Treatment + Covariate, 
                          ~ Instrument, 
                          SimData)
BootSim4 <- bootstrapDelta(unY ~ Treatment + Covariate, 
                           ~ Instrument, 
                           SimData,
                           effectType = 'All')
lm(unY ~ Treatment + Covariate)
