estimateDelta <- function(regressionFormula, 
                          instrumentFormula, 
                          data, 
                          weightMethod = 'glm') {
  
  # Extract model data given formula
  ModelData <- model.frame(regressionFormula, data, na.action = NULL)
  # Extract instrument data given formula
  InstrumentData <- model.frame(instrumentFormula, data, na.action = NULL)
  
  # Calculate weights; add this to data b/c lm() won't recognize the object otherwise
  ModelData$IPWs <- calculateWeights(modelData = ModelData,
                                     instrumentData = InstrumentData,
                                     method = weightMethod)
  
  # Estimate model
  Model <- lm(formula = regressionFormula,
              weights = IPWs,
              data = data.frame(ModelData, InstrumentData))
  return(Model$coefficients)
}