estimateDelta <- function(regressionFormula, 
                          instrument, 
                          data, 
                          weightMethod = 'glm',
                          nBoots = 1000) {
  
  # Extract model data given formula
  ModelData <- model.frame(regressionFormula, data, na.action = NULL)
  # Extract instrument data given formula
  InstrumentData <- model.frame(instrument, data, na.action = NULL)
  
  # Calculate weights; add this to data b/c lm() won't recognize the object otherwise
  ModelData$IPWs <- calculateWeights(modelData = ModelData,
                                     instrumentData = InstrumentData,
                                     method = weightMethod)
  print(ModelData$IPWs)
  # Bootstrap data
  BootsList <- lapply(X = 1:nBoots, 
                      FUN = function(x) ModelData[sample(x = nrow(ModelData), 
                                                         size = 10,
                                                         replace = T), ])
  
  # Estimate model
  Model <- lm(formula = regressionFormula,
              weights = IPWs,
              data = data.frame(ModelData, InstrumentData))
  return(Model)
}