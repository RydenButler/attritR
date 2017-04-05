probabilityFits <- function(formula,
                            modelData,
                            method = binomial(link = logit)
                            ) {
  # Calculate model
  Model <- gam(formula = formula, 
               family = method,
               data = modelData,
               maxit = 1000)
  # Calculate fitted values
  Fits <- predict(object = Model,
                  newdata = modelData,
                  type = 'response')
  return(Fits)
}
