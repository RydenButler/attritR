library(MASS)

Treatment <- rbinom(n = 2000, size = 1, prob = 0.5)
Covariate <- runif(n = length(Treatment), min = -1, max = 1)
Instrument <- runif(n = length(Treatment), min = -1, max = 2)
# For attrition on observables
UV <- mvrnorm(n = 2000, mu = c(0,0), Sigma = matrix(c(1, 0, 0, 1), nrow = 2))
U <- UV[ , 1]
V <- UV[ , 2]

Y <- .5*Treatment + -2*Covariate + 1*Treatment*Covariate + U
TreatmentEffect<-mean(Y[Treatment==1])-mean(Y[Treatment==0])
R <- 3*Treatment + -4*Covariate + 2*Instrument + V > 0
table(R)
Y[!R] <- NA
table(is.na(Y))
# For attrition on unobservables
unUV <- mvrnorm(n = 2000, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
unU <- unUV[ , 1]
unV <- unUV[ , 2]
unY <- 1*Treatment + 1*Covariate + 0.25*Treatment*Covariate + unU
unR <- 0.5*Treatment + 0.5*Covariate + 1*Instrument + unV > 0
unY[!unR] <- NA
SimData <- data.frame(Y,Treatment,Covariate,Instrument)
summary(lm(Y~Treatment+Covariate, data=SimData))
TreatmentEffect

calculateWeights(SimData[,1:3],
                 SimData[,4])
estimateDelta(Y ~ Treatment + Covariate,
              ~ Instrument,
              SimData)


# Check observables
BootSim <- bootstrapDelta(Y ~ Treatment + Covariate, 
                        ~ Instrument, 
                        SimData)
BootSim$MeanEst
BootSim2 <- bootstrapDelta(Y ~ Treatment + Covariate, 
                          ~ Instrument, 
                          SimData,
                          effectType = 'All')
BootSim2$MeanEst
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
