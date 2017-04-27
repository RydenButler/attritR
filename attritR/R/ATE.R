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
#'  \item{MeanEst}{A list containing the mean estimated coefficients for the regression
#'  used to estimate the average treatment effect.  List will contain the estimates
#'  specified by the option \code{effectType}.}
#'  \item{MedianEst}{A list containing the median estimated coefficients for the regression
#'  used to estimate the average treatment effect.  List will contain the estimates
#'  specified by the option \code{effectType}.}
#'  \item{SE}{A list containing the mean estimated standard errors of the estimates
#'  for the regression used to estimate the average treatment effect.  List will contain 
#'  the standard errors for the estimates specified by the option \code{effectType}.}
#'  \item{Quantiles}{A list containing the coefficient estimates for the regression
#'  used to estimate the average treatment effect that correspond with the quantiles
#'  specified by \code{quantile}.  List will contain the quantiles specified by the option
#'   \code{effectType}.}
#'  \item{Matrix}{A matrix containing the coefficient estimates from each bootstrapped
#'  sample.  Each row of the matrix corresponds with one of the coefficients estimated
#'  in the model, and the coefficient estimates in each column are the estimates obtained
#'  in the bootstrapped sample corresponding with the column index.}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname bootstrapDelta
#' @import 'parallel'
#' @export

ATE <- function(regressionFormula, 
                            instrumentFormula, 
                            data,
                            p_W_Formula = R ~ .,
                            p_W_Method = binomial(link = logit),
                            PiFormula = D ~ .,
                            PiMethod = binomial(link = logit),
                            nBoots = 1000,
                            quantiles = c(0.05, 0.95),
                            effectType = 'All',
                            nCores = 1
                            ) {
  
  Results <- bootstrapDelta(regressionFormula, 
                            instrumentFormula, 
                            data,
                            p_W_Formula = p_W_Formula,
                            p_W_Method = p_W_Method,
                            PiFormula = PiFormula,
                            PiMethod = PiMethod,
                            nBoots = nBoots,
                            quantiles = quantiles,
                            effectType = effectType
                            )
  
  # Printing summary result tables
  if(effectType == "Population"){
    ResultsPrint <- data.frame('Mean'=c(format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3)),
                               'Median'=c(format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3)),
                               'SE'=c(format(round(Results$SE$Pop[2], digit=3), nsmall=3)),
                               'Lower 5%'=c(format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3)),
                               'Upper 95%'=c(format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3)),
                               row.names = c("Population"),
                               check.names = FALSE)
      cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
        'Summary:\n')
        # '          ', 'Mean ','Median','SE  ',
        #             paste0('lower',' ',signif(as.numeric(substr(quantiles[1],3,4))),'%'),
        #             paste0('upper',' ',signif(as.numeric(substr(quantiles[2],3,4))),'%\n'),
        # 'Population', format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3), 
        # format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3), 
        # format(round(Results$SE$Pop[2], digit=3), nsmall=3), 
        # format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3), 
        # format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3))
        print(ResultsPrint)
    invisible(Results)
  } 
  if(effectType == "Respondent"){
    ResultsPrint <- data.frame('Mean'=c(format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3)),
                               'Median'=c(format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3)),
                               'SE'=c(format(round(Results$SE$Resp[2], digit=3), nsmall=3)),
                               'Lower 5%'=c(format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3)),
                               'Upper 95%'=c(format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3)),
                               row.names = c("Respondent"),
                               check.names = FALSE)
    cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
      # '          ', 'Mean ','Median','SE  ',
      #             paste0('lower',' ',signif(as.numeric(substr(quantiles[1],3,4))),'%'),
      #             paste0('upper',' ',signif(as.numeric(substr(quantiles[2],3,4))),'%\n'),
      # 'Respondent',  format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3), 
      # format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3), 
      # format(round(Results$SE$Resp[2], digit=3), nsmall=3), 
      # format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3), 
      # format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3))
      print(ResultsPrint)
    invisible(Results)
  }
  if(effectType == "Both"){  ## to do: aligning columns consistently
    ResultsPrint <- data.frame('Mean'=c(format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3),
                                        format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3)),
                               'Median'=c(format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3),
                                          format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3)),
                               'SE'=c(format(round(Results$SE$Resp[2], digit=3), nsmall=3),
                                      format(round(Results$SE$Pop[2], digit=3), nsmall=3)),
                               'Lower 5%'=c(format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3),
                                            format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3)),
                               'Upper 95%'=c(format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3),
                                             format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3)),
                               row.names = c("Respondent","Population"),
                               check.names = FALSE)
    cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n')
    #   '          ', 'Mean ','Median','SE  ',
    #                   paste0('lower',' ',signif(as.numeric(substr(quantiles[1],3,4))),'%'),
    #                   paste0('upper',' ',signif(as.numeric(substr(quantiles[2],3,4))),'%\n'),
    #   'Respondent',  format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3), 
    #   format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3), 
    #   format(round(Results$SE$Resp[2], digit=3), nsmall=3), 
    #   format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3), 
    #   format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3),'\n',
    #   'Population', format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3), 
    #   format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3), 
    #   format(round(Results$SE$Pop[2], digit=3), nsmall=3), 
    #   format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3), 
    #   format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3))
    print(ResultsPrint)
    invisible(Results)
  }
}
  
  