#' Estimating standard errors for ATEs given non-random attrition
#'
#' \code{bootstrapDelta} estimates standard errors of the average treatment effect (ATE) 
#' under conditions of non-random attrition.
#' 
#' @param regressionFormula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{regressionFormula}
#' is the model which will be used to estimate the ATE after accounting for non-random
#' attrition. Formula must be of the form Y ~ D + ..., where Y is the outcome, D is the treatment,
#' and ... represents any set of other covariates.
#' @param instrumentFormula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{instrumentFormula}
#' is the model which estimates the weights used in the model which estimates the ATE.
#' Formula must be of the form ~ Z1 + ..., where Z1 represents some instrumental variable,
#' and ... represents any other desired instrumental variables.
#' @param data A data frame which contains all variables to be used in both 
#' \code{regressionFormula} and \code{instrumentFormula}.
#' @param weightMethod An optional string indicating the estimation method to be used for
#' generating the weights.  The default method is generalized linear model ("glm").
#' @param nBoots Numeric value defining the number of bootstrap samples; the default number
#' of bootstrap replications is 1,000.
#' 
#' @details
#' The function estimates standard errors for the average treatment effect (ATE)
#' after accounting for non-random attrition. After drawing random samples with 
#' replacement from the provided data (by default, 1000 samples), the function 
#' estimates with the bootstrapped sample the mean value, median, and standard errors 
#' based on the bootstrapped replications. The function, thus, provides a measure of 
#' accuracy to the point estimates for the ATE.
#' 
#' @references Huber, Martin (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A list of three vectors for mean, median, and standard errors of the ATE.
#'  \item{MeanEst}{A numeric vector of mean values of the ATE for treatment 
#'  and control group.}
#'  \item{MedianEst}{A numeric vector of the median of the ATE for treatment 
#'  and control group.}
#'  \item{SE}{A numeric vector of standard errors of the ATE for treatment 
#'  and control group.}
#'  \item{Quantiles}{A matrix containing estimates that correspond to the quantile (row)
#'  and coefficient (column). By defualt it returns the 5\% and 95\% quantiles.}
#'  \item{Matrix}{A matrix containing the bootstrapped coefficient estimates, where each
#'  row corresponds to a covaraite, and each column corresponds to a bootstrap iteration.}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname bootstrapDelta
#' @export

bootstrapDelta <- function(regressionFormula, 
                           instrumentFormula, 
                           data,
                           p_W_Formula = p_W_Formula,
                           p_W_Method = p_W_Method,
                           PiFormula = PiFormula,
                           PiMethod = PiMethod,
                           nBoots = 1000,
                           quantiles = c(0.05, 0.95),
                           effectType = 'ATT'
                           ) {
  # Bootstrap data: random sampling of dataset with replacement
  BootsList <- lapply(X = 1:nBoots, 
                      FUN = function(x) data[sample(x = nrow(data),
                                                    size = nrow(data),
                                                    replace = T), ]
                      )

  if(effectType == 'ATT'){
    CoefMatrix <- sapply(BootsList, 
                         function(x) estimateDelta(regressionFormula = regressionFormula,
                                                   instrumentFormula = instrumentFormula,
                                                   data = x
                                                   )$ATT$coefficients
                         )
  } 
  if(effectType == 'ATE'){
    CoefMatrix <- sapply(BootsList, 
                         function(x) estimateDelta(regressionFormula = regressionFormula,
                                                   instrumentFormula = instrumentFormula,
                                                   data = x
                                                   )$ATE$coefficients
                         )
  } 
  # Option to evaluate both the ATT and ATE
  # This is incredibly slow, and should not be used for testing purposes until this function is optimizes
  # One way of streamlining this may be to bootstrap the ATE and ATT in different functions,
  # and have them both return in some umbrella function. This will remove the need for 
  # conditional statements, and perhaps will lead to more optimal memory allocation.
  if(effectType == 'Both'){
    ATTMatrix <- sapply(BootsList,
                        function(x) estimateDelta(regressionFormula = regressionFormula,
                                                  instrumentFormula = instrumentFormula,
                                                  data = x
                                                  )$ATT$coefficients
                        )
    ATEMatrix <- sapply(BootsList,
                        function(x) estimateDelta(regressionFormula = regressionFormula,
                                                  instrumentFormula = instrumentFormula,
                                                  data = x
                                                  )$ATE$coefficients
                        )
    
    ATTSEs <- apply(ATTMatrix, 1, sd)
    ATTMeans <- rowMeans(ATTMatrix)
    ATTMedians <- apply(ATTMatrix, 1, median)
    ATTQuantiles <- apply(ATTMatrix, 1, function(x) quantile(x = x, probs = quantiles, na.rm = T))
    
    ATESEs <- apply(ATEMatrix, 1, sd)
    ATEMeans <- rowMeans(ATEMatrix)
    ATEMedians <- apply(ATEMatrix, 1, median)
    ATEQuantiles <- apply(ATEMatrix, 1, function(x) quantile(x = x, probs = quantiles, na.rm = T))
    
    return(list(MeanATT = ATTMeans, 
                MedianATT = ATTMedians, 
                SE_ATT = ATTSEs,
                QuantilesATT = ATTQuantiles,
                MatrixATT = ATTMatrix,
                MeanATE = ATEMeans, 
                MedianATE = ATEMedians, 
                SE_ATE = ATESEs,
                QuantilesATE = ATEQuantiles,
                MatrixATE = ATEMatrix,
                )
           )
  }
  
  # Calculate results: mean, median, and standard errors, based on bootstrapped replications
  SEs <- apply(CoefMatrix, 1, sd)
  Means <- rowMeans(CoefMatrix)
  Medians <- apply(CoefMatrix, 1, median)
  # Note that the bootstrapping results in some NA coefficient estimates (colinearity? too much missingness?)
  # As a result, na.rm is required here for the quantiles (though strangely, not for the other functions)
  # This is unnecessary with larger sample sizes, and may disappear with additional noise
  # All efforts should be taken to have na.rm == F, as wanton removal of NAs can gloss over major errors
  # If na.rm must be true, we should include a warning message if NAs are found in the CoefMatrix
  # Additionally we may want an error thrown if the number of NAs exceeds some tolerable threshold
  Quantiles <- apply(CoefMatrix, 1, function(x) quantile(x = x, probs = quantiles, na.rm = T))
  # return list with mean, median, and standard error of estimated ATE for treatment and control
  return(list(MeanEst = Means, 
              MedianEst = Medians, 
              SE = SEs,
              Quantiles = Quantiles,
              Matrix = CoefMatrix
              )
         )
  }