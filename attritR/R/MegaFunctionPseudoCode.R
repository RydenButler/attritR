#' Estimating the average treatment effect given non-random attrition
#'
#' \code{ATE} estimates the average treatment effect under conditions of
#'  non-random attrition.
#' 
#' @param regressionFormula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{regressionFormula}
#' is the model which will be used to estimate the average treatment effect accounting for 
#' non-random attrition. Formula must be of the form Y ~ D + ..., where Y is the outcome, 
#' D is the treatment, and ... represents any additional covariates.  The first covariate
#' in the formula MUST be the treatment, D.
#' @param instrumentFormula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{instrumentFormula}
#' contains the instruments to be used to calculate the weights which account for non-random
#' atrrition.  Formula must be of the form ~ Z1 + ..., where Z1 represents
#' some instrumental variable, and ... represents any additional instruments.
#' @param data A data frame which contains all variables to be used in both 
#' \code{regressionFormula} and \code{instrumentFormula}.
#' @param p_W_Formula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{p_W_Formula}
#' is the model which estimates subjects' response propensity, which is used in generating
#' the weights which account for non-random attrition.  By default, the formula includes
#' all covariates included in \code{regressionFormula}.
#' @param p_W_Method A character string which indicates the the error distribution and 
#' link function to be used in the generalized additive model which estimates subjects'
#' response propensity.  By default, \code{p_W_Formula} is set to 
#' \code{binomial(link = logit)}, which fits a logistic regression model.  Any family
#' function that \code{gam} accepts can be specified here.
#' @param PiFormula An object of class \code{formula} (or one that can be coerced to
#' that class); a symbolic description of the model to be fitted.  \code{p_W_Formula}
#' is the model which estimates subjects' treatment propensity, which is used in generating
#' the weights which account for non-random attrition.  By default, the formula includes
#' all covariates included in \code{regressionFormula}.
#' @param PiMethod A character string which indicates the the error distribution and 
#' link function to be used in the generalized additive model which estimates subjects'
#' treatment propensity.  By default, \code{p_W_Formula} is set to 
#' \code{binomial(link = logit)}, which fits a logistic regression model.  Any family
#' function that \code{gam} accepts can be specified here.
#' @param nBoots Numeric value defining the number of bootstrap samples; the default number
#' of bootstrap replications is 1000.
#' @param quantiles Vector of two numeric values between 0 and 1 specifying the quantiles
#'  at which estimates of the average treatment effect will be returned.  
#'  By default, \code{quantiles} are set to 0.05 and 0.95, which corresponds with a 90\%
#'  confidence interval.
#' @param effectType Character string indicating the type of effect types to be calculated
#' and returned.  Valid inputs are 'Respondent,' which calculates the average treatment
#' effect only among respondents; 'Population,' which calculates the average treatment effect
#' among all subjects; and 'All,' which calculates both the average treatment effect only
#' only among respondents, and among all subjects.
#' @param nCores Numeric value indicating the number of cores to allocate to running the
#' function.  See \code{parallel} for details.
#' 
#' @details
#' The function estimates the average treatment effect (ATE) under non-random attrition,
#' and uses bootstrapping to estimate mean value, median, and standard errors 
#' based on the bootstrapped replications.
#' 
#' @references Huber, Martin (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A list of five elements containing the following:
#'  \item{Means}{A list containing the mean estimated coefficients for the regression
#'  used to estimate the average treatment effect.  List will contain the estimates
#'  specified by the option \code{effectType}.}
#'  \item{Medians}{A list containing the median estimated coefficients for the regression
#'  used to estimate the average treatment effect.  List will contain the estimates
#'  specified by the option \code{effectType}.}
#'  \item{SD}{A list containing the mean estimated standard errors of the estimates
#'  for the regression used to estimate the average treatment effect.  List will contain 
#'  the standard errors for the estimates specified by the option \code{effectType}.}
#'  \item{Quants}{A list containing the coefficient estimates for the regression
#'  used to estimate the average treatment effect that correspond with the quantiles
#'  specified by \code{quantile}.  List will contain the quantiles specified by the option
#'   \code{effectType}.}
#'  \item{Matrix}{A matrix containing the coefficient estimates from each bootstrapped
#'  sample.  Each row of the matrix corresponds with one of the coefficients estimated
#'  in the model, and the coefficient estimates in each column are the estimates obtained
#'  in the bootstrapped sample corresponding with the column index.}
#' @author Ryden Butler and David Miller. Special thanks to Jonas Markgraf and Hyunjoo Oh.
#' 
#' @rdname ATE
#' @import 'parallel'
#' @export
#' 

################################################################################
### Conditional Statements Determining Which Weights & Proposition are Computed
################################################################################
if(prop == '1' | prop == 'All'){
  # Calculate relevant statistics
  ATE$Prop1 <- PointEstimates$Prop1$coefficients[2]
  SD$Prop1 <- apply(UncertaintyEstimates[1:3, ], 1, sd)
  # Extract average minimum weight across all replications
  MinWeights$Prop1 <- min(PointEstimates$pW_Obs_T)
  # Extract average maximum weight across all replications
  MaxWeights$Prop1 <- max(PointEstimates$pW_Obs_T)
} 
if(prop == '2' | prop == 'All'){
  # Calculate relevant statistics
  ATE$Prop2 <- PointEstimates$Prop2$coefficients[2]
  SD$Prop2 <- apply(UncertaintyEstimates[4:6, ], 1, sd)
  # Extract average minimum weight across all replications
  MinWeights$Prop2 <- min(PointEstimates$Pi_Obs_Resp)
  # Extract average maximum weight across all replications
  MaxWeights$Prop2 <- max(PointEstimates$Pi_Obs_Resp)
} 
if(prop == '3' | prop == 'All'){
  # Calculate relevant statistics
  ATE$Prop3 <- PointEstimates$Prop3$coefficients[2]
  SD$Prop3 <- apply(UncertaintyEstimates[7:9, ], 1, sd)
  # Extract average minimum weight across all replications
  MinWeights$Prop3 <- min(PointEstimates$pWxPi_Obs_T)
  # Extract average maximum weight across all replications
  MaxWeights$Prop3 <- max(PointEstimates$pWxPi_Obs_T)
} 
if(prop == '4' | prop == 'All'){
  # Calculate relevant statistics
  ATE$Prop4 <- PointEstimates$Prop4$coefficients[2]
  SD$Prop4 <- apply(UncertaintyEstimates[10:12, ], 1, sd)
  # Extract average minimum weight across all replications
  MinWeights$Prop4 <- min(PointEstimates$Pi_Resp)
  # Extract average maximum weight across all replications
  MaxWeights$Prop4 <- max(PointEstimates$Pi_Resp)
} 
if(prop == '5' | prop == 'All'){
  # Calculate relevant statistics
  ATE$Prop5 <- PointEstimates$Prop5$coefficients[2]
  SD$Prop5 <- apply(UncertaintyEstimates[13:15, ], 1, sd)
  # Extract average minimum weight across all replications
  MinWeights$Prop5 <- min(PointEstimates$pWxPi_Resp)
  # Extract average maximum weight across all replications
  MaxWeights$Prop5 <- max(PointEstimates$pWxPi_Resp)
} 

################################################################################
###                       Calculate Weights
################################################################################

probabilityFits <- function(formula,
                            modelData,
                            method = binomial(link = logit)
) {
  # Predict probabilities for fitted model
  Fits <- predict(object = gam(formula = formula, 
                               family = method,
                               data = modelData,
                               maxit = 1000),
                  type = 'response')
  return(Fits)
}

modelData[ , 1] <- as.numeric(!is.na(.subset2(modelData, 1)))
# Rename to R and D for access below
names(modelData)[1:2] <- c('R', 'D')

# IMPORTANT NOTE:
### This modelData must be structured as columns of Y, D, X
### Otherwise the following calculations are incorrect

# Note: the p(W) and pi weights are calculated differently for each of the five 
# propositions, so we need to calculate each type of weight

# calculating response propensity on treatment and observables (Props 1 and 3)
# Regress R on D + X; calculate fitted values
p_W_Fits_NoInst <- probabilityFits(formula = p_W_Formula,
                                   # Since default formula is R ~ .
                                   modelData = modelData,
                                   method = p_W_Method
)

# calculating treatment propensity given observables for respondents only (Prop 2)
# Regress D on X; calculate fitted values
Pi_Fits_Obs_Resp <- probabilityFits(formula = PiFormula,
                                    # Since default formula is D ~ ., we remove R, while conditioning on R = 1
                                    modelData = modelData[modelData$R==1 , -1],
                                    method = PiMethod
)

# Treatment propensity scores
Pi_Fits_Obs_Resp[modelData[modelData$R==1, ]$D!=1] <-  (1 - Pi_Fits_Obs_Resp[modelData[modelData$R==1, ]$D!=1])

# Product of response propensity and treatment propensity given observables (Prop 3)

AllWeights_Obs <- p_W_Fits_NoInst[modelData$R==1]*Pi_Fits_Obs_Resp

# calculations of treatment and response propensity when incorporating the instrument
# (Props 4 and 5)

# Regress R on D + X + Z; calculate fitted values
modelData$p_W_Fits_Inst <- probabilityFits(formula = p_W_Formula,
                                           # Since default formula is R ~ .
                                           modelData = data.frame(modelData, instrumentData),
                                           method = p_W_Method
)

# pi (treatment propensity scores) estimated for respondents only
# (Prop 4)
# Regress D on X + p(W)
Pi_Fits_Resp <- probabilityFits(formula = PiFormula,
                                # Since default formula is D ~ ., we remove R while conditioning on R = 1
                                modelData = modelData[modelData$R==1 , -1],
                                method = PiMethod
)

# Treatment propensity scores
Pi_Fits_Resp[modelData[modelData$R==1, ]$D!=1] <-  (1 - Pi_Fits_Resp[modelData[modelData$R==1, ]$D!=1])

# Product of response propensity scores and treatment propensity scores when using
# treatment propensity score for respondents only
AllWeights_Resp <- modelData[modelData$R==1, ]$p_W_Fits_Inst * Pi_Fits_Resp


################################################################################
###                  Estimate n Bootstraps of Desired Estimator
################################################################################

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
UncertaintyEstimates <- parSapply(BootsCluster, 1:nBoots, 
                                  FUN = function(n) {bootstrap <- lm(formula = regressionFormula,
                                                                     weights = 1/AppropriateWeight,
                                                                     data = ModelData[!is.na(ModelData$Y),]
                                  )
                                  return(coef(bootstrap))})
stopCluster(cl = BootsCluster)


################################################################################
###                     Print Model Summary & Return Results
################################################################################

# Printing summary result tables

if(effectType == "Population"){
  ResultsPrint <- data.frame('Mean'=c(format(round(Results$Means$Pop[2], digit=3), nsmall=3)),
                             'Median'=c(format(round(Results$Medians$Pop[2], digit=3), nsmall=3)),
                             'SD'=c(format(round(Results$SD$Pop[2], digit=3), nsmall=3)),
                             'Lower 5%'=c(format(round(Results$Quants$Pop[1,2], digit=3), nsmall=3)),
                             'Upper 95%'=c(format(round(Results$Quants$Pop[2,2], digit=3), nsmall=3)),
                             row.names = c("Population"),
                             check.names = FALSE)
  cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
  print(ResultsPrint)
  invisible(Results)
} 
if(effectType == "Respondent"){
  ResultsPrint <- data.frame('Mean'=c(format(round(Results$Means$Resp[2], digit=3), nsmall=3)),
                             'Median'=c(format(round(Results$Medians$Resp[2], digit=3), nsmall=3)),
                             'SE'=c(format(round(Results$SD$Resp[2], digit=3), nsmall=3)),
                             'Lower 5%'=c(format(round(Results$Quants$Resp[1,2], digit=3), nsmall=3)),
                             'Upper 95%'=c(format(round(Results$Quants$Resp[2,2], digit=3), nsmall=3)),
                             row.names = c("Respondent"),
                             check.names = FALSE)
  cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
  print(ResultsPrint)
  invisible(Results)
}
if(effectType == "Both"){  ## to do: aligning columns consistently
  ResultsPrint <- data.frame('Mean'=c(format(round(Results$Means$Resp[2], digit=3), nsmall=3),
                                      format(round(Results$Means$Pop[2], digit=3), nsmall=3)),
                             'Median'=c(format(round(Results$Medians$Resp[2], digit=3), nsmall=3),
                                        format(round(Results$Medians$Pop[2], digit=3), nsmall=3)),
                             'SE'=c(format(round(Results$SD$Resp[2], digit=3), nsmall=3),
                                    format(round(Results$SD$Pop[2], digit=3), nsmall=3)),
                             'Lower 5%'=c(format(round(Results$Quants$Resp[1,2], digit=3), nsmall=3),
                                          format(round(Results$Quants$Pop[1,2], digit=3), nsmall=3)),
                             'Upper 95%'=c(format(round(Results$Quants$Resp[2,2], digit=3), nsmall=3),
                                           format(round(Results$Quants$Pop[2,2], digit=3), nsmall=3)),
                             row.names = c("Respondent","Population"),
                             check.names = FALSE)
  cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
  print(ResultsPrint)
  invisible(Results)
}
}