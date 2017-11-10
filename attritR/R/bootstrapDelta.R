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
#' @param prop A string of either 'Population', 'Respondent', or 'Both', corresponding
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
#' @return A list of six lists. If prop = 'Both', each list contains two lists, otherwise
#' each element consists of only one list.
#'  \item{Means}{A list containing a numeric vector of mean estimates for the effects of treatment and
#'  covariates of interest.}
#'  \item{Medians}{A list containing a numeric vector of median estimates for the effects of treatment and
#'  covariates of interest.}
#'  \item{SD}{A list containing a numeric vector of standard deviations for the estimated effects of treatment and
#'  covariates of interest.}
#'  \item{Quants}{A list containing a matrix of quantiles (rows) corresponding to the estimates
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
                           prop = 'All',
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
                                'prop',
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
  MinWeights = list()
  MaxWeights = list()
  
  if(prop == '1' | prop == 'All'){
    # Extract coefs for respondent estimates
    Matrix$Prop1 <- sapply(Estimates, 
                             FUN = function(x) x$Prop1$coefficients
                             )
    # Calculate relevant statistics
    Means$Prop1 <- rowMeans(Matrix$Prop1)
    Medians$Prop1 <- apply(Matrix$Prop1, 1, median)
    SD$Prop1 <- apply(Matrix$Prop1, 1, sd)
    Quants$Prop1 <- apply(Matrix$Prop1, 1, function(x) quantile(x, quantiles, na.rm = T))
    # Extract average minimum weight across all replications
    MinWeights$Prop1 <- mean(sapply(Estimates, FUN = function(x) min(x$pW_Obs_T)))
    # Extract average maximum weight across all replications
    MaxWeights$Prop1 <- mean(sapply(Estimates, FUN = function(x) max(x$pW_Obs_T)))
  } 
  if(prop == '2' | prop == 'All'){
    # Extract coefs for respondent estimates
    Matrix$Prop2 <- sapply(Estimates, 
                           FUN = function(x) x$Prop2$coefficients
    )
    # Calculate relevant statistics
    Means$Prop2 <- rowMeans(Matrix$Prop2)
    Medians$Prop2 <- apply(Matrix$Prop2, 1, median)
    SD$Prop2 <- apply(Matrix$Prop2, 1, sd)
    Quants$Prop2 <- apply(Matrix$Prop2, 1, function(x) quantile(x, quantiles, na.rm = T))
    # Extract average minimum weight across all replications
    MinWeights$Prop2 <- mean(sapply(Estimates, FUN = function(x) min(x$Pi_Obs_Resp)))
    # Extract average maximum weight across all replications
    MaxWeights$Prop2 <- mean(sapply(Estimates, FUN = function(x) max(x$Pi_Obs_Resp)))
  } 
  if(prop == '3' | prop == 'All'){
    # Extract coefs for respondent estimates
    Matrix$Prop3 <- sapply(Estimates, 
                           FUN = function(x) x$Prop3$coefficients
    )
    # Calculate relevant statistics
    Means$Prop3 <- rowMeans(Matrix$Prop3)
    Medians$Prop3 <- apply(Matrix$Prop3, 1, median)
    SD$Prop3 <- apply(Matrix$Prop3, 1, sd)
    Quants$Prop3 <- apply(Matrix$Prop3, 1, function(x) quantile(x, quantiles, na.rm = T))
    # Extract average minimum weight across all replications
    MinWeights$Prop3 <- mean(sapply(Estimates, FUN = function(x) min(x$pWxPi_Obs_T)))
    # Extract average maximum weight across all replications
    MaxWeights$Prop3 <- mean(sapply(Estimates, FUN = function(x) max(x$pWxPi_Obs_T)))
  } 
  if(prop == '4' | prop == 'All'){
    # Extract coefs for respondent estimates
    Matrix$Prop4 <- sapply(Estimates, 
                           FUN = function(x) x$Prop4$coefficients
    )
    # Calculate relevant statistics
    Means$Prop4 <- rowMeans(Matrix$Prop4)
    Medians$Prop4 <- apply(Matrix$Prop4, 1, median)
    SD$Prop4 <- apply(Matrix$Prop4, 1, sd)
    Quants$Prop4 <- apply(Matrix$Prop4, 1, function(x) quantile(x, quantiles, na.rm = T))
    # Extract average minimum weight across all replications
    MinWeights$Prop4 <- mean(sapply(Estimates, FUN = function(x) min(x$Pi_Resp)))
    # Extract average maximum weight across all replications
    MaxWeights$Prop4 <- mean(sapply(Estimates, FUN = function(x) max(x$Pi_Resp)))
  } 
  if(prop == '5' | prop == 'All'){
    # Extract coefs for respondent estimates
    Matrix$Prop5 <- sapply(Estimates, 
                           FUN = function(x) x$Prop5$coefficients
    )
    # Calculate relevant statistics
    Means$Prop5 <- rowMeans(Matrix$Prop5)
    Medians$Prop5 <- apply(Matrix$Prop5, 1, median)
    SD$Prop5 <- apply(Matrix$Prop5, 1, sd)
    Quants$Prop5 <- apply(Matrix$Prop5, 1, function(x) quantile(x, quantiles, na.rm = T))
    # Extract average minimum weight across all replications
    MinWeights$Prop5 <- mean(sapply(Estimates, FUN = function(x) min(x$pWxPi_Resp)))
    # Extract average maximum weight across all replications
    MaxWeights$Prop5 <- mean(sapply(Estimates, FUN = function(x) max(x$pWxPi_Resp)))
  } 
  
  # Stopping the cluster
  stopCluster(BootsCluster)
  # return list with relevant statistics and data
  return(list(Means = Means, 
              Medians = Medians, 
              SD = SD,
              Quants = Quants,
              MinWeights = MinWeights,
              MaxWeights = MaxWeights,
              Matrix = Matrix,
              Data = BootsList
  )
  )
}