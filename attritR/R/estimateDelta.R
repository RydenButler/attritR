#' Estimating the ATE given non-random attrition
#'
#' \code{estimateDelta} estimates the average treatment effect (ATE) under conditions of
#' non-random attrition.
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
#' 
#' @details
#' The function estimates the average treatment effect (ATE) after accounting for non-random
#' attrition, using the method presented by Huber (2012).  Given non-random attrition, if we
#' know the instrument(s) Z which are related to attrition, we can estimate weights to use
#' when estimating the ATE such that we can recover the ATE despite non-random attrition.
#' Defining the following variables as follows---R=whether the respondent responded (i.e.
#' 0=attrition), D=treatment assignment, Y=outcome, p(W)=response propensity score, and
#' pi=treatment propensity score---, we can estimate delta as:
#' \deqn{\Delta=E[\frac{R \cdot D \cdot Y}{p(W) \cdot pi(X,p(W))}] - E[\frac{R \cdot (1-D) \cdot Y}{p(W) \cdot (1-pi(X,p(W)))}]}
#' 
#' @references Huber, Martin (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A list containing two elements, each of which is an \code{lm} object 
#' containing estimates for the treatment effect and the effects of covariates included in
#' \code{regressionFormula}.
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname estimateDelta
#' @export

estimateDelta <- function(regressionFormula, 
                          instrumentFormula, 
                          data,
                          p_W_Formula = R ~ .,
                          p_W_Method = binomial(link = logit),
                          PiFormula = D ~ .,
                          PiMethod = binomial(link = logit)
                          ) {
  
  # Extract model data given formula
  ModelData <- model.frame(regressionFormula, data, na.action = NULL)
  # Extract instrument data given formula
  InstrumentData <- model.frame(instrumentFormula, data, na.action = NULL)
  

  # Calculate weights; add this to data b/c lm() won't recognize the object otherwise
  WeightList <- calculateWeights(modelData = ModelData,
                                 instrumentData = InstrumentData,
                                 p_W_Formula = p_W_Formula,
                                 p_W_Method = p_W_Method,
                                 PiFormula = PiFormula,
                                 PiMethod = PiMethod
                                 )

  ModelData$Pi <- WeightList$Pi
  ModelData$pWxPi <- WeightList$pWxPi

  # Estimate Proposition 4:
  RespondentModel <- lm(formula = regressionFormula,
                 weights = 1/Pi,
                 data = ModelData
                 )
  # Estimate Proposition 5:
  PopulationModel <- lm(formula = regressionFormula,
                 weights = 1/pWxPi,
                 data = ModelData
                 )
  
  return(list(RespondentDelta = RespondentModel,
              PopulationDelta = PopulationModel
              )
         )
}