#' Estimating standard errors for ATEs given non-random attrition
#'
#' \code{bootstrapDeltaP} estimates standard errors of the average treatment effect (ATE) in parallel
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

bootstrapDelta_P <- function(regressionFormula, 
                           instrumentFormula, 
                           data,
                           p_W_Formula = R ~ .,
                           p_W_Method = binomial(link = logit),
                           PiFormula = D ~ .,
                           PiMethod = binomial(link = logit),
                           nBoots = 1000,
                           quantiles = c(0.05, 0.95),
                           effectType = 'Respondent'
                           ) {
  # Identifying and setting number of cores
  #num_cores <- detectCores() - 1
  # Setting cluster of cores
  #cl <- makeCluster(num_cores)
  # for Windows computers, you need to explicitly identify which objects from the environment will
  # be used in your parallel functions
  #clusterExport(cl, c(data, regressionFormula, instrumentFormula))
  #setDefaultCluster(cl)
  
  # Bootstrap data: random sampling of dataset with replacement
  BootsList <- parLapply(X = 1:nBoots, 
                      fun = function(x) data[sample(x = nrow(data),
                                                    size = nrow(data),
                                                    replace = T), ]
                      )
  
  # Need to redefine exports for each new/updated export on Windows; if not loaded, need to load in
  # subsidiary functions in package and "gam" library
  #clusterExport(cl, c("data", "regressionFormula", "instrumentFormula", "BootsList", "estimateDelta",
  #                    "calculateWeights", "probabilityFits", "gam"))
  #setDefaultCluster(cl)
  
  if(effectType == 'Respondent'){
    CoefMatrix <- parSapply(cl=cl, BootsList, 
                         FUN = function(x) estimateDelta(regressionFormula = regressionFormula,
                                                   instrumentFormula = instrumentFormula,
                                                   data = x
                                                   )$RespondentDelta$coefficients
                         )
  } 
  if(effectType == 'All'){
    CoefMatrix <- parSapply(BootsList, 
                         function(x) estimateDelta(regressionFormula = regressionFormula,
                                                   instrumentFormula = instrumentFormula,
                                                   data = x
                                                   )$AllDelta$coefficients
                         )
  } 

  # This is incredibly slow, and should not be used for testing purposes until this function is optimizes
  # One way of streamlining this may be to bootstrap the Respondent and All in different functions,
  # and have them both return in some umbrella function. This will remove the need for 
  # conditional statements, and perhaps will lead to more optimal memory allocation.
  if(effectType == 'Both'){
    RespondentMatrix <- parSapply(BootsList,
                        function(x) estimateDelta(regressionFormula = regressionFormula,
                                                  instrumentFormula = instrumentFormula,
                                                  data = x
                                                  )$RespondentDelta$coefficients
                        )
    AllMatrix <- parSapply(BootsList,
                        function(x) estimateDelta(regressionFormula = regressionFormula,
                                                  instrumentFormula = instrumentFormula,
                                                  data = x
                                                  )$AllDelta$coefficients
                        )
    
    RespondentSEs <- parApply(RespondentMatrix, 1, sd)
    RespondentMeans <- rowMeans(RespondentMatrix)
    RespondentMedians <- parApply(RespondentMatrix, 1, median)
    RespondentQuantiles <- parApply(RespondentMatrix, 1, function(x) quantile(x = x, probs = quantiles))
    
    AllSEs <- parApply(AllMatrix, 1, sd)
    AllMeans <- rowMeans(AllMatrix)
    AllMedians <- parApply(AllMatrix, 1, median)
    AllQuantiles <- parApply(AllMatrix, 1, function(x) quantile(x = x, probs = quantiles))
    
    return(list(RespondentMean = RespondentMeans, 
                RespondentMedian = RespondentMedians, 
                RespondentSE = RespondentSEs,
                RespondentQuantiles = RespondentQuantiles,
                RespondentMatrix = RespondentMatrix,
                AllMean = AllMeans, 
                AllMedian = AllMedians, 
                AllSE = AllSEs,
                AllQuantiles = AllQuantiles,
                AllMatrix = AllMatrix,
                )
           )
  }
  
  # need to load "stats" library
  #clusterExport(cl, c("data", "regressionFormula", "instrumentFormula", "BootsList", "estimateDelta",
  #                    "calculateWeights", "probabilityFits", "gam", "CoefMatrix","sd",
  #                    "quantiles", "quantile"))
  #setDefaultCluster(cl)
  
  # Calculate results: mean, median, and standard errors, based on bootstrapped replications
  SEs <- parApply(cl=cl, X=CoefMatrix, MARGIN=1, FUN=function(x) sd(x))
  Means <- rowMeans(CoefMatrix)
  Medians <- parApply(cl=cl, X=CoefMatrix, MARGIN=1, FUN=function(x) median(x))
  # Note that the bootstrapping results in some NA coefficient estimates (colinearity? too much missingness?)
  # As a result, na.rm is required here for the quantiles (though strangely, not for the other functions)
  # This is unnecessary with larger sample sizes, and may disappear with additional noise
  # All efforts should be taken to have na.rm == F, as wanton removal of NAs can gloss over major errors
  # If na.rm must be true, we should include a warning message if NAs are found in the CoefMatrix
  # Additionally we may want an error thrown if the number of NAs exceeds some tolerable threshold
  # Currently the NA problem appears more frequently (possibly exclusively) when calculating the ATE
  Quantiles <- parApply(cl=cl, CoefMatrix, 1, function(x) quantile(x = x, probs = quantiles))
  # return list with mean, median, and standard error of estimated for treatment and control
  return(list(MeanEst = Means, 
              MedianEst = Medians, 
              SE = SEs,
              Quantiles = Quantiles,
              Matrix = CoefMatrix
              )
         )
  # Stopping the cluster
  stopCluster(clust)
  }