library(devtools)
library(usethis)
library(roxygen2)
library(MASS)
library(causalweight)

# DO NOT RUN THE FOLLOWING LINES
# This creates a blank package template
#### package.skeleton('attritR')

# This simulates and saves the data used to test the package
# simulateData <- function(N = 1000,
#                          treatmentEffectR = c(-2,2), 
#                          covariateEffectR = 1,
#                          instrumentEffectR = 1,
#                          treatmentEffectY = 1, 
#                          covariateEffectY = 1,
#                          interactionEffectY = .25
# ){
#   X <- runif(n = N, min = -1, max = 1)
#   Z <- runif(n = N, min = -1, max = 2)
#   D <- rbinom(n = N, size = 1, prob = 0.5)
#   # For attrition on unobservables
#   UV <- mvrnorm(n = N, mu = c(0,0), Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))
#   U <- UV[ , 1]
#   V <- UV[ , 2]
#   # Counterfactual treatment effects
#   YTreatment <- treatmentEffectY + covariateEffectY*X + interactionEffectY*D*X + U
#   YControl <-  covariateEffectY*X + U
#   # Counterfactual ATE | All
#   ATE <- mean(YTreatment) - mean(YControl)
#   
#   Sims <- lapply(seq(treatmentEffectR[1], treatmentEffectR[2], 0.1), function(currentR) {
#     # Realized treatment effects and attrition
#     Y <- treatmentEffectY*D + covariateEffectY*X + interactionEffectY*D*X + U
#     R <- currentR*D + covariateEffectR*X + instrumentEffectR*Z + V > 0
#     Y[!R] <- NA
#     # Combines realized data
#     SimData <- data.frame(Y, D, X, Z)
#     # ATE | Response
#     ATR <- mean(Y[R & D]) - mean(Y[R & !D]) 
#     return(list(ATR = ATR, SimData = SimData))
#   }
#   )
#   ATR <- unlist(lapply(Sims, function(sim) sim$ATR))
#   SimData <- lapply(Sims, function(sim) sim$SimData)
#   return(list(ATE = ATE, ATR = ATR, SimData = SimData))
# }
# 
# SimulatedAttrition <- simulateData(treatmentEffectR = c(-1,0))$SimData[[1]]
# 
# save(SimulatedAttrition, file = 'attritR/data/SimulatedAttrition.RData')

# Compile latest package and re-documents
# setwd('~/Dropbox')
Current <- as.package('attritR')
load_all(Current)
document(Current)

# use_readme_rmd()




data("SimulatedAttrition")
# make intentionally irregular variable names
names(SimulatedAttrition) <- c('trump_words', 'Tment', 'covariate', 'money_inst')

regression_formula = trump_words ~ Tment + covariate
treatment = 'Tment'
instrument = NULL
data = SimulatedAttrition
effect_type  = 'population'
attrition_type = 'observable'
response_weight_formula = response ~ .
response_weight_method = 'ridge'
treatment_weight_formula = treatment ~ .
treatment_weight_method = 'ridge'
n_bootstraps = 10
quantiles = c(0.05, 0.95)
n_cores = 1

# Prop 3
out <- ipwlm(regression_formula = trump_words ~ Tment + covariate,
             treatment = 'Tment',
             instrument = NULL,
             data = SimulatedAttrition,
             effect_type  = 'population', # "respondent", "population"
             attrition_type = 'observable', # "treatment", "observable", "unobservable"
             response_weight_formula = response ~ .,
             response_weight_method = 'ridge',
             treatment_weight_formula = treatment ~ .,
             treatment_weight_method = 'ridge',
             n_bootstraps = 10,
             quantiles = c(0.05, 0.95),
             n_cores = 1)

out2 <- treatweight(y = SimulatedAttrition$trump_words, 
                    d = SimulatedAttrition$Tment, 
                    x = SimulatedAttrition$covariate, 
                    s = as.numeric(!is.na(SimulatedAttrition$trump_words)), 
                    z = NULL, 
                    boot = 10, trim = 0.00, selpop = FALSE)

out$coefficients
out2$effect

# Prop 4
out <- ipwlm(regression_formula = trump_words ~ Tment + covariate + money_inst,
    treatment = 'Tment',
    instrument = 'money_inst',
    data = SimulatedAttrition,
    effect_type  = 'respondent', # "respondent", "population"
    attrition_type = 'unobservable', # "treatment", "observable", "unobservable"
    response_weight_formula = response ~ .,
    response_weight_method = 'logit',
    treatment_weight_formula = treatment ~ .,
    treatment_weight_method = binomial(link = probit),
    n_bootstraps = 10,
    quantiles = c(0.05, 0.95),
    n_cores = 1)

out2 <- treatweight(y = SimulatedAttrition$trump_words, 
            d = SimulatedAttrition$Tment, 
            x = SimulatedAttrition$covariate, 
            s = as.numeric(!is.na(SimulatedAttrition$trump_words)), 
            z = SimulatedAttrition$money_inst, 
            boot = 10, trim = 0.00, selpop = TRUE)

out$coefficients
out2$effect

# Prop 5
out <- ipwlm(regression_formula = trump_words ~ Tment + covariate + money_inst,
             treatment = 'Tment',
             instrument = 'money_inst',
             data = SimulatedAttrition,
             effect_type  = 'population', # "respondent", "population"
             attrition_type = 'unobservable', # "treatment", "observable", "unobservable"
             response_weight_formula = response ~ .,
             response_weight_method = 'probit',
             treatment_weight_formula = treatment ~ .,
             treatment_weight_method = binomial(link = probit),
             n_bootstraps = 10,
             quantiles = c(0.05, 0.95),
             n_cores = 1)

out2 <- treatweight(y = SimulatedAttrition$trump_words, 
                    d = SimulatedAttrition$Tment, 
                    x = SimulatedAttrition$covariate, 
                    s = as.numeric(!is.na(SimulatedAttrition$trump_words)), 
                    z = SimulatedAttrition$money_inst, 
                    boot = 10, trim = 0.00, selpop = FALSE)

out$coefficients
out2$effect




# This will tak a moment ...
#demo(plotAttrition)
#demo(plotInteraction)

### Efficieny checks ###
library(microbenchmark)
# Check parallel bootstrap
# 4 core parallel is down to 6 seconds. sapplysin bootstrap works best without parallel
microbenchmark(bootstrapDelta(Y ~ D + X, 
                              instrumentFormula = ~ Z, 
                              data = SimulatedAttrition,
                              effectType = 'Both',
                              nCores = 4),
               bootstrapDelta(Y ~ D + X, 
                              instrumentFormula = ~ Z, 
                              data = SimulatedAttrition,
                              effectType = 'Both',
                              nCores = 1),
               times = 1)

test <- lineprof(bootstrapDelta(Y ~ Treatment + Covariate, 
                                instrumentFormula = ~ Instrument, 
                                data = ObsData,
                                effectType = 'Both',
                                nCores = 4))
shine(test)




