bootstrapDelta <- function(regressionFormula, 
                           instrumentFormula, 
                           data, 
                           weightMethod = 'glm',
                           nBoots = 1000) {
  # Bootstrap data
  BootsList <- lapply(X = 1:nBoots, FUN = function(x) data[sample(x = nrow(data),
                                                             size = nrow(data),
                                                             replace = T), ])
  # Estimate Delta
  CoefMatrix <- sapply(BootsList, 
                       function(x) estimateDelta(regressionFormula = regressionFormula,
                                                 instrumentFormula = instrumentFormula,
                                                 data = x,
                                                 weightMethod = weightMethod)
                       )
  
  # Calculate results
  SEs <- apply(CoefMatrix, 1, sd)
  Means <- rowMeans(CoefMatrix)
  Medians <- apply(CoefMatrix, 1, median)
  
  return(list(MeanEst = Means, MedianEst = Medians, SE = SEs))
  }