# To run this demonstration:
# 1) Read in probabilityFits function in calculateWeights.R
# 2) Read in SimData in HuberSimulation.R

modelData <- SimData[, 1:3]
instrumentData <- SimData[ , 4]
p_W_Formula = R ~ .
p_W_Method = binomial(link = logit)
PiFormula = D ~ .
PiMethod = binomial(link = logit)

# Testing P(W) Model Fits
GLM <- glm(R ~ D + Covariate + Instrument, family = binomial, 
           data = modelData)
GAM <- gam(R ~ D + Covariate + Instrument, family = binomial, data = modelData)
GAM_dot <- gam(R ~ ., family = binomial, data = data.frame(modelData, instrumentData))
rbind(coef(GLM), coef(GAM), coef(GAM_dot))

rbind(head(predict(GLM, type = 'response')), 
      head(predict(GAM, type = 'response')),
      head(predict(GAM_dot, type = 'response'))
)

modelData$p_W_Fits <- predict(GAM_dot, type = 'response')
### HERE'S THE TRICK I FORGOT TO MENTION
modelData[modelData$D != 1, ]$p_W_Fits <- (1 - modelData[modelData$D != 1, ]$p_W_Fits)

# Testing Pi Model Fits
GLM_pi <- glm(D ~ Covariate + p_W_Fits, family = binomial,
              data = modelData)
GAM_pi <- gam(D ~ Covariate + p_W_Fits, family = binomial,
              data = modelData)
GAM_dot_pi <- gam(D ~ ., family = binomial, data = modelData[,-1])
rbind(coef(GLM_pi), coef(GAM_pi), coef(GAM_dot_pi))

rbind(head(predict(GLM_pi, type = 'response')), 
      head(predict(GAM_pi, type = 'response')),
      head(predict(GAM_dot_pi, type = 'response'))
)
