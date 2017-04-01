estimateDelta <- function(regressionFormula, 
                          instrument, 
                          data, 
                          weightMethod = 'glm') {
  
  ModelData <- model.frame(regressionFormula, data, na.action = NULL)
  InstrumentData <- model.frame(instrument, data, na.action = NULL)
  
  data$IPWs <- calculateWeights(Y = ModelData[ , 1], 
                                D = ModelData[ , 2],
                                X = ModelData[ ,3:ncol(ModelData)],
                                Z = InstrumentData,
                                method = weightMethod)

  Model <- lm(formula = regressionFormula,
              weights = IPWs,
              data = data)
  return(Model)
}