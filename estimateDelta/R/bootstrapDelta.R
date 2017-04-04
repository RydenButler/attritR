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
#'  \item{vector1}{A numeric vector of mean values of the ATE for treatment 
#'  and control group.}
#'  \item{vector2}{A numeric vector of the median of the ATE for treatment 
#'  and control group.}
#'  \item{vector3}{A numeric vector of standard errors of the ATE for treatment 
#'  and control group.}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname bootstrapDelta
#' @export

bootstrapDelta <- function(regressionFormula, 
                           instrumentFormula, 
                           data, 
                           weightMethod = 'glm',
                           nBoots = 1000) {  # by default, number of bootstrap replications in 1,000
  # Bootstrap data: random sampling of dataset with replacement
  BootsList <- lapply(X = 1:nBoots, FUN = function(x) data[sample(x = nrow(data),
                                                             size = nrow(data),
                                                             replace = T), ])
  # Estimate ATE with bootstrapped data
  CoefMatrix <- sapply(BootsList, 
                       function(x) estimateDelta(regressionFormula = regressionFormula,
                                                 instrumentFormula = instrumentFormula,
                                                 data = x,
                                                 weightMethod = weightMethod)
                       )
  
  # Calculate results: mean, median, and standard errors, based on bootstrapped replications
  SEs <- apply(CoefMatrix, 1, sd)
  Means <- rowMeans(CoefMatrix)
  Medians <- apply(CoefMatrix, 1, median)
  # return list with mean, median, and standard error of estimated ATE for treatment and control
  return(list(MeanEst = Means, MedianEst = Medians, SE = SEs))
  }