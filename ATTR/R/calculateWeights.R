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
#' @import 'gam'
#' @export

calculateWeights <- function(modelData, 
                             instrumentData, 
                             p_W_Formula = R ~ .,
                             p_W_Method = binomial(link = logit),
                             PiFormula = D ~ .,
                             PiMethod = binomial(link = logit)
                             ) {
  # Recode Y as R
  # R is test participation: if Y is observed, R=1; else R=0
  modelData[ , 1] <- as.numeric(!is.na(.subset2(modelData, 1)))
  # Rename to R and D for access below
  names(modelData)[1:2] <- c('R', 'D')
  
  # IMPORTANT NOTE:
  ### This modelData must be structured as columns of Y, D, X, Z
  ### Otherwise the following calculations are incorrect
  
  # Regress R on D + X + Z; calculate fitted values
  modelData$p_W_Fits <- probabilityFits(formula = p_W_Formula,
                                        modelData = data.frame(modelData, instrumentData),
                                        method = p_W_Method
                                        )
  # Regress D on X + Z
  Pi_Fits <- probabilityFits(formula = PiFormula,
                             # Since default formula is D ~ ., we remove R
                             modelData = modelData[ , -1],
                             method = PiMethod
                             )
  # Calculate fits for non-treated respondents
  Pi_Fits[which(modelData$D != 1)] <-  (1 - Pi_Fits[which(modelData$D != 1)])
  
  # Calculate weights for all subjects
  ATEWeights <- modelData$p_W_Fits * Pi_Fits

  return(list(ATTWeights = Pi_Fits,
              ATEWeights = ATEWeights
              )
         )
}
