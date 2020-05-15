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
#' 

ATE <- function(outcomeFormula, 
                treatment,
                instrument,
                data,
                rpwFormula = R ~ .,
                rpwMethod = binomial(link = logit),
                tpwFormula = D ~ .,
                tpwMethod = binomial(link = logit),
                nBoots = 1000,
                quantiles = c(0.05, 0.95),
                effectType, # "respondent" or "population"
                attritionType, # "Treat" "Obs" or "Unobs"
                nCores = 1
) {
  
  ################################################################################
  ### Conditional Statements Determining Which Weights & Proposition are Computed
  ################################################################################
  
  # which proposition you are using depends on the inputs to effectType and attritionType
  
  # prop 0: attrition caused by treatment, ATE|R=1--effectType="respondent" attritionType="Treat" (we probably don't even want to try to incorporate functionality for this, since there is no weighting involved)
  # prop 1: attrition caused by treatment, ATE--effectType="population" attritionType="Treat"
  # prop 2: attrition caused by treatment and observables, ATE|R=1--effectType="respondent" attritionType="Obs"
  # prop 3: attrition caused by treatment and observables, ATE--effectType="population" attritionType="Obs"
  # prop 4: attrition caused by treatment, observables, and unobservables ATE|R=1--effectType="respondent" attritionType="Unobs"
  # prop 5: attrition caused by treatment, observables, and unobservables ATE--effectType="population" attritionType="Unobs"
  
  # this creates a new variable to indicate which observations are respondents/attritters
  
  data$R <- ifelse(!is.na(data[,as.character(outcomeFormula[2])]), 1 , 0)
  
  ################################################################################
  ###                       Calculate Weights
  ################################################################################
  
  probabilityFits <- function(formula,
                              data,
                              method = binomial(link = logit)
  ) {
    # Predict probabilities for fitted model
    Fits <- predict(object = gam(formula = formula, 
                                 family = method,
                                 data = data,
                                 maxit = 1000),
                    type = 'response')
    return(Fits)
  }
  
  if(effectType == "population"){
    
    data$rpw <- probabilityFits(formula = rpwFormula,
                                data = data,
                                method = rpwMethod
    )
    
    if(attritionType == "Treat" | attritionType == "Obs"){
      
      data$tpw <- probabilityFits(formula = tpwFormula,
                                  data = data,
                                  method = tpwMethod
      )
      
    }
    
    if(attritionType == "Unobs"){
      
      data$tpw <- probabilityFits(formula = update(tpwFormula, ~ . + rpw),
                                  data = data,
                                  method = tpwMethod
      )
      
    }
    
  }
  
  if(effectType=="respondent"){
    
    if(attritionType == "Treat" | attritionType == "Obs"){
      
      data$tpw <- probabilityFits(formula = tpwFormula,
                                  modelData = data,
                                  method = tpwMethod
      )
      
    }
    
    if(attritionType == "Unobs"){
      
      data$rpw <- probabilityFits(formula = rpwFormula,
                                  data = data,
                                  method = rpwMethod
      )
      
      data$tpw <- probabilityFits(formula = update(tpwFormula, ~ . + rpw),
                                  modelData = data,
                                  method = tpwMethod
      )
      
      # this is for proposition 4; the rpw is only used to calculate the tpw, so just get rid of it
      # after using it
      
      data$rpw <- NULL
      
    }
    
  }
  
  # need to reorient the tpw for units in control
  
  data$tpw[data[,treatment]!=1] <-  (1 - data$tpw[data[,treatment]!=1])
  
  ################################################################################
  ###                  Estimate Treatment Effect for Desired Estimator
  ################################################################################
  
  # if the effectType="population" there will be rpw in the data
  
  if("rpw" %in% colnames(data)){
    
    treatEff <- coef(lm(formula = outcomeFormula,
                        weights = 1/(rpw*tpw),
                        data = data
    ))[treatment]
    
  } else {
    
    treatEff <- coef(lm(formula = outcomeFormula,
                        weights = 1/tpw,
                        data = data
    ))[treatment]
    
  }
  
  
  ################################################################################
  ###                  Estimate n Bootstraps of Desired Estimator
  ################################################################################
  
  # Make cluster
  BootsCluster <- makeCluster(nCores)
  # Bootstrap data: random sampling of dataset with replacement
  clusterExport(BootsCluster, c('outcomeFormula',
                                'data',
                                'nBoots',
                                'treatment'),
                envir = environment())
  
  # in the bootstrapping, do we want bootstrapped data sets of the size equal to the number of respondents,
  # or of the full sample?  the former is implemented
  
  if("rpw" %in% colnames(data)){
    
    UncertaintyEstimates <- parSapply(BootsCluster, 1:nBoots,
                                      FUN = function(n) {bootstrap <- lm(formula = outcomeFormula,
                                                                         weights = 1/(rpw*tpw),
                                                                         data = data[sample(x = nrow(data[which(data$R==1),]),
                                                                                            size = nrow(data[which(data$R==1),]),
                                                                                            replace = T), ]
                                      )
                                      return(coef(bootstrap)[treatment])})
  } else {
    
    UncertaintyEstimates <- parSapply(BootsCluster, 1:nBoots,
                                      FUN = function(n) {bootstrap <- lm(formula = outcomeFormula,
                                                                         weights = 1/tpw,
                                                                         data = data[sample(x = nrow(data[which(data$R==1),]),
                                                                                            size = nrow(data[which(data$R==1),]),
                                                                                            replace = T), ]
                                      )
                                      return(coef(bootstrap)[treatment])})
    
  }
  
  stopCluster(cl = BootsCluster)
  
  
  ################################################################################
  ###                     Print Model Summary & Return Results
  ################################################################################
  
  # Printing summary result tables
  
  ResultsPrint <- data.frame('Estimate' = treatEff,
                             'SD' = sd(UncertaintyEstimates),
                             'CI Lower' = quantile(UncertaintyEstimates,probs=quantiles[1]),
                             'CI Upper' = quantile(UncertaintyEstimates,probs=quantiles[2]),
                             row.names = c("Treatment Effect"))
  cat('--- Treatment Effect Estimate from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
  print(ResultsPrint)
  # what we might want to add to the printed summary: number of observations, number attritted,
  # effectType, attritionType
  invisible()
  # what we might want in the outputted object: treatment effect, bootstrapped SE, bootstrapped CI,
  # uncertainty estimates, rpw/tpw (where applicable), effectType, attritionType, formulas
}
