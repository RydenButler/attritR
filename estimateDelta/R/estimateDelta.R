estimateDelta <- function(regressionFormula, 
                          instrument, 
                          data, 
                          weightMethod = 'glm') {
  
  # Extract model data given formula
  ModelData <- model.frame(regressionFormula, data, na.action = NULL)
  # Extract instrument data given formula
  InstrumentData <- model.frame(instrument, data, na.action = NULL)
  # Calculate weights; add this to data b/c lm() won't recognize the object otherwise
  data$IPWs <- calculateWeights(Y = ModelData[ , 1], 
                                D = ModelData[ , 2],
                                X = ModelData[ ,3:ncol(ModelData)],
                                Z = InstrumentData,
                                method = weightMethod)
  # Estimate model
  Model <- lm(formula = regressionFormula,
              weights = IPWs,
              data = data)
  return(Model)
}