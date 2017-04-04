#' Calculating weights for treatment and control group under non-random attrition
#'
#' \code{calculateWeights} estimates the weights for treatment and control group 
#' in order to obtain internally and externally valid estimates for the average treatment effect (ATE) 
#' in cases of attrition.
#' 
#' @param modelData A matrix with columns of \code{Y}, \code{D}, \code{X}, where 
#' \code{Y} indicates outcome variable which may or may not include NA (non-response), 
#' \code{D} indicates treatment indicator, either 1 (treatment) or 0 (non-treatment),
#' \code{X} indicates observed covariates, and \code{Z} indicates instrumental variable. 
#' \code{modelData} must structured with \code{Y}, \code{D}, \code{X}. 
#' Otherwise, the incorrect output will be produced.
#' @param instrumentData A matrix with a column of \code{Z}, indicating instrumental variable. 
#' \code{Z} is a predictor of response variable (R) of 1 if \code{Y} is observed and 0 otherwise (attribution).
#' It needs to contain at least one continuous element and not to have a direct effect on the outcome (\code{Y}).
#' @param method A string indicating the estimation method to be used for generating the weights, 
#' which can be either generalized linear model ("glm") or generalized additive model ("gam").
#' The default method is generalized linear model ("glm"), which allows for response variable (R)
#' to have error distribution other than normal distribution.
#'
#' 
#' @details
#' The function estimates the response propensity score and the treatment propensity score 
#' to reweigh the observations and correct for attrition. 
#' The response propensity score is defined as $p(W) = Pr(R=1 | \code{X},\code{D}, \code{Z})$ 
#' where $R$ denotes to the binary response variable, being 1 if \code{Y} is observed and 0 otherwise (attrition), 
#' and the treatment propensity score, defined as $\pi(\code{X}, p(W)) = Pr(\code{D}=1 | \code{X}, p(W), R=1)$.
#' Response and treatment propensity scores are multiplied for treatment and control group
#' in order to adjust for differences in the distributions of X and p(W) between treated and nontreated. 
#' For more information on the method, see Huber (2012) below.
#' 
#' @references Huber (2012): "Identification of Average Treatment Effects in 
#' Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A dataframe of estimated weights for treated and untreated(control) group.
#'  \item{column1}{A dataframe of estimated weights for treated and untreated group, 
#'  returned by products of predicted values which are calculated by \code{method}.}
#' @author Ryden Butler, David Miller, Jonas Markgraf, and Hyunjoo Oh
#' 
#' @rdname calculateWeights
#' @importFrom 'gam' gam
#' @export

calculateWeights <- function(modelData, instrumentData, method = 'glm') {
  # Recode Y as a form of R, which denotes a binary response variable i.e., test participation:
  # if Y is observed (non-attribution), 1; otherwise, 0.
  # In modelData, the first column has values of R which are recoded binary values of Y.
  modelData[ , 1] <- as.numeric(!is.na(modelData[ , 1]))
  # IMPORTANT NOTE:
  ### This modelData must be structured as columns of Y, D, X, Z
  ### Otherwise the following calculations are incorrect
  
  if(method == 'glm'){
    #### Calculate response propensity score, p(W)
    # fitting generalized linear model: 
    # response propensity R regressed on treatment status (D), observed covariates (X), and instrument (Z)
    p_w <- glm(formula = formula(paste(colnames(modelData)[1], '~ .')), 
               family = binomial(link = logit),
               data = data.frame(modelData, instrumentData)) 
    # Predict values given the model
    p_w_fits <- predict(object = p_w,
                        newdata = data.frame(modelData[, -1], instrumentData),
                        type = 'response')
    #### Calculate treatment propensity score pi(X, p(W))
    # fitting GLM: treatment propensity (D) regressed on instrumental variable (D) and covariates (X)
    pi <- glm(formula = formula(paste(colnames(data.frame(modelData[ , -1])), '~ .')), 
              family = binomial(link = logit), 
              maxit = 1000, 
              data = data.frame(modelData[ , -1], p_w_fits)) 
    # Predict values given the model
    pi_fits <- predict(object = pi,
                       newdata = data.frame(modelData[ , -(1:2)], p_w_fits),
                       type = 'response')

  } else {
    #### Calculate response propensity score, p(W)
    # fitting generalized additive model: 
    # response propensity (R) regressed on treatment status (D), observed covariates (X), and instrument (Z)
    p_w <- gam(formula = formula(paste(colnames(modelData)[1], '~ .')), 
               family = binomial(link = logit),
               data = data.frame(modelData, instrumentData))
    # Predict values given the model
    p_w_fits <- predict(object = p_w,
                        newdata = data.frame(modelData[, -1], instrumentData),
                        type = 'response')    
    
    #### Calculate treatment propensity score pi(X, p(W))
    # fitting GAM: treatment propensity (D) regressed on instrumental variable (D) and covariates (X)
    pi <- gam(formula = formula(paste(colnames(data.frame(modelData[ , -1])), '~ .')), 
              family = binomial(link = logit), 
              maxit = 1000,
              data = data.frame(modelData[ , -1], p_w_fits)) 
    # Predict values given the model
    pi_fits <- predict(object = pi,
                       newdata = data.frame(modelData[ , -(1:2)], p_w_fits),
                       type = 'response')    
  }

  # Estimate weights for treated group
  Weights <- p_w_fits * pi_fits
  # Estimate weights for control group
  Weights[which(modelData[ , 2] != 1)] <- (p_w_fits[which(modelData[ , 2] != 1)] * 
                                             (1 - pi_fits[which(modelData[ , 2] != 1)]))
  return(Weights)
}
