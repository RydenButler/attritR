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
#' @param p_W_Formula The regression formula (of class \code{formula}) for calculating the response propensity probabilities.
#' By default this formula is of the form R ~ ., where R represents the presence of a response 
#' and . represents treatment, desired covariates, and the instrument. Users are strongly discouraged 
#' from manipulating this formula.
#' @param p_W_Method The regression method for calculating the response propensity probabilities.
#'  By default this is binomial(link = logit). Other valid methods include those that function in the 
#'  family argument to the \code{gam} function.
#' @param PiFormula The regression formula (of class \code{formula}) for calculating the treatment propensity probabilities.
#' By default this formula is of the form D ~ ., where D represents treatment and . represents
#' desired covariates and the response propensity probabilities. Users are strongly discouraged from
#' manipulating this formula.
#' @param PiMethod The regression method for calculating the treatment propensity probabilities.
#' By default this is binomial(link = logit). Other valid methods include those that function in the 
#'  family argument to the \code{gam} or function.
#' @param nBoots Numeric value defining the number of bootstrap samples; the default number
#' of bootstrap replications is 1,000.
#' @param quantiles A vector of percentiles for which the bootstrapped quantiles will be returned.
#' By default the function returns the quantiles corresponding to the 5th and 95th percentiles.
#' @param effectType A string of either 'Population', 'Respondent', or 'Both', corresponding
#' to the desired ATE calculation. By default, the function calculates the ATE on the population.
#' @param nCores
#' 
#' @details
#' The function estimates standard errors for the average treatment effect (ATE)
#' after accounting for non-random attrition. After drawing random samples with 
#' replacement from the provided data (by default, 1000 samples), the function 
#' calculates the sample mean, median, and standard errors 
#' based on the bootstrapped estimates of the ATE.
#' 
#' @references Huber, Martin (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A list of six lists. If effectType = 'Both', each list contains two lists, otherwise
#' each element consists of only one list.
#'  \item{MeanEst}{A list containing a numeric vector of mean estimates for the effects of treatment and
#'  covariates of interest.}
#'  \item{MedianEst}{A list containing a numeric vector of median estimates for the effects of treatment and
#'  covariates of interest.}
#'  \item{SE}{A list containing a numeric vector of standard deviations for the estimated effects of treatment and
#'  covariates of interest.}
#'  \item{Quantiles}{A list containing a matrix of quantiles (rows) corresponding to the estimates
#'  for the treatment and covariate effects (columns). By defualt it returns quantiles corresponding to
#'  the 5th and 95th percentiles.}
#'  \item{Matrix}{A list containing a matrix of bootstrapped coefficient estimates, where each
#'  row corresponds to a covariate, and each column corresponds to a bootstrap iteration.}
#'  \item{Data}{A list containing a list of bootstrapped data frames.}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname bootstrapDelta
#' @import 'parallel'
#' @export

bootstrapDelta <- function(regressionFormula, 
                           instrumentFormula, 
                           data,
                           p_W_Formula = R ~ .,
                           p_W_Method = binomial(link = logit),
                           PiFormula = D ~ .,
                           PiMethod = binomial(link = logit),
                           nBoots = 1000,
                           quantiles = c(0.05, 0.95),
                           effectType = 'Population',
                           nCores = 1
) {
  
  # Make cluster
  BootsCluster <- makeCluster(nCores)
  clusterExport(BootsCluster, c('regressionFormula', 
                                'instrumentFormula', 
                                'data',
                                'p_W_Formula',
                                'p_W_Method',
                                'PiFormula',
                                'PiMethod',
                                'nBoots',
                                'quantiles',
                                'effectType',
                                'estimateDelta',
                                'calculateWeights',
                                'probabilityFits',
                                'gam'), 
                envir = environment())
  # Bootstrap data: random sampling of dataset with replacement
  BootsList <- parLapply(BootsCluster, X = 1:nBoots, 
                         fun = function(x) data[sample(x = nrow(data),
                                                       size = nrow(data),
                                                       replace = T), ]
  )
  
  Estimates <- parLapply(BootsCluster, BootsList, 
                         fun = function(x) estimateDelta(regressionFormula = regressionFormula,
                                                           instrumentFormula = instrumentFormula,
                                                           data = x))
  Means = list()
  Medians = list()
  SD = list()
  Quants = list()
  Matrix = list()
  
  if(effectType == 'Respondent' | effectType == 'Both'){
    # Extract coefs for respondent estimates
    Matrix$Resp <- sapply(Estimates, 
                             FUN = function(x) x$RespondentDelta$coefficients
                             )
    # Calculate relevant statistics
    Means$Resp <- rowMeans(Matrix$Resp)
    Medians$Resp <- apply(Matrix$Resp, 1, median)
    SD$Resp <- apply(Matrix$Resp, 1, sd)
    Quants$Resp <- apply(Matrix$Resp, 1, function(x) quantile(x, quantiles, na.rm = T))
  } 
  if(effectType == 'Population' | effectType == 'Both'){
    # Extract coefs for population estimates
    Matrix$Pop <- sapply(Estimates, 
                                   FUN = function(x) x$PopulationDelta$coefficients
                            )
    # Calculate relevant statistics
    Means$Pop <- rowMeans(Matrix$Pop)
    Medians$Pop <- apply(Matrix$Pop, 1, median)
    SD$Pop <- apply(Matrix$Pop, 1, sd)
    Quants$Pop <- apply(Matrix$Pop, 1, function(x) quantile(x, quantiles, na.rm = T))
  } 
  
  # Stopping the cluster
  stopCluster(BootsCluster)
  # return list with relevant statistics and data
  return(list(Means = Means, 
              Medians = Medians, 
              SD = SD,
              Quants = Quants,
              Matrix = Matrix,
              Data = BootsList
  )
  )
}