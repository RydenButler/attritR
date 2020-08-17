#' Estimating treatment effects given non-random attrition
#'
#' \code{ipwlm} estimates the average treatment effect --- either among 
#' respondents (ATE|R = 1) or among the population (ATE) --- under various 
#' conditions of non-random attrition (attrition due to treatment alone, 
#' attrition due to treatment conditioned on observable confouding, or attrition 
#' due to treatment conditioned on observable and unobservable confounding).
#' 
#' @param regression_formula An object of class \code{formula} (or one that can 
#' be coerced to that class); a symbolic description of the model to be fitted.  
#' \code{regression_formula} is the model which will be used to estimate the 
#' average treatment effect accounting for non-random attrition. Formula must be 
#' of the form Y ~ D + ..., where Y is the outcome, D is the treatment, and ... 
#' represents any additional covariates, including an instrumental variable when 
#' weighting for attrition on unobservables.
#' @param treatment A string specifying the treatment variable in the regression.
#' The string must match the name of the treatment variable exactly as it is 
#' passed into \code{regression_formula}.
#' @param instrument A string specifying the instrumental variable in the 
#' regression, if one is included. The string must match the name of the 
#' treatment variable exactly as it is passed into \code{regression_formula}. If 
#' no instrumental variable is used, the argument input should remain \code{NULL}.
#' @param data A data.frame which contains all variables specified in
#' \code{regression_formula}.
#' @param effect_type A string specifying the type of treatment effect to be 
#' estimated. Input must be one of \code{"respondent"} or \code{"population"}, 
#' which refer to the average treatment effect among respondents (ATE|R = 1) and 
#' the average treatment effect among the population (ATE), respectively.
#' @param attrition_type A string specifying the assumed effect of attrition. 
#' Input must be one of \code{"treatment"}, \code{"observable"}, or 
#' \code{"unobservable"}. These inputs refer, in order, to attrition due to 
#' treatment alone, attrition due to treatment and observable confounders, and 
#' attrition due to treatment, observable confounders, and unobservable 
#' confounders. If \code{"unobservable"} is specified, an instrumental variable
#' must be included in \code{regression_formula} and its name must be noted in 
#' \code{instrument}.
#' @param response_weight_formula An object of class \code{formula} (or one that 
#' can be coerced to that class); a symbolic description of the model to be 
#' fitted. \code{response_weight_formula} specifies the model for estimating 
#' respondents' response propensity, which is used in weighting treatment effect
#' estimates that account for non-random attrition. By default, the formula 
#' includes all covariates on the right-hand side of \code{regression_formula}.
#' @param response_weight_method A character string specifying the model 
#' which estimates subjects' response propensity. By default, 
#' \code{response_weight_method} is set to \code{'logit'}, which 
#' fits a binomial logistic regression model. Other valid inputs include 
#' \code{'probit'}, which fits a binomial probit regression, and \code{'ridge'},
#' which fits a binomial logistic ridge regression. 
#' @param treatment_weight_formula An object of class \code{formula} (or one that 
#' can be coerced to that class); a symbolic description of the model to be 
#' fitted. \code{treatment_weight_formula} specifies the model for estimating 
#' respondents' treatment propensity, which is used in weighting treatment effect
#' estimates that account for non-random attrition. By default, the formula 
#' includes only the confounder covariates included on the right-hand side of 
#' \code{regression_formula}. When accounting for attrition on unobservables,
#' response propensity weights are also included on the right-hand side of the 
#' formula.
#' @param treatment_weight_method A character string specifying the model 
#' which estimates subjects' response propensity. By default, 
#' \code{treatment_weight_method} is set to \code{'logit'}, which 
#' fits a binomial logistic regression model. Other valid inputs include 
#' \code{'probit'}, which fits a binomial probit regression, and \code{'ridge'},
#' which fits a binomial logistic ridge regression. 
#' @param n_bootstraps Numeric value defining the number of non-parametric 
#' bootstrap samples to be computed. The default number of bootstrap 
#' replications is 1000.
#' @param quantiles Vector of two numeric values between 0 and 1 specifying the 
#' quantiles at which non-parametric bootstrapped confidence intervals are to be 
#' returned. By default, \code{quantiles} are set to 0.05 and 0.95, which 
#' corresponds with a 90\% confidence interval.
#' @param n_cores Numeric value indicating the number of cores to allocate to 
#' running the function.  See \code{parallel} for details.
#' 
#' @details
#' The function estimates the treatment effect given non-random attrition, and 
#' uses non-parametric bootstrapping to estimate uncertainty.
#' 
#' @references Huber, Martin (2012): "Identification of Average Treatment Effects 
#' in Social Experiments Under Alternative Forms of Attrition.", Journal of 
#' Educational and Behavioral Statistics, vol. 37, no. 3, 443-474.
#'
#' @return A list of five elements containing the following:
#'  \item{coefficients}{A vector containing the estimated treatment effect, 
#'  bootstrapped standard error, and a bootstrapped confidence interval.}
#'  \item{weights}{A data.frame containing the response propensity weights, the
#'  treatment propensity weights, and the inverse probability weights used to
#'  estimate the treatment effect.}
#'  \item{effect}{A string noting the \code{effect_type} specified for the given
#'  estimate.}
#'  \item{attrition}{A string noting the \code{attrition_type} specified for the 
#'  given estimate.}
#'  \item{formulae}{A list containing the precise formulae that specify the 
#'  treatment effect function, the response propensity function, and the treatment 
#'  propensity function.}
#' @author Ryden Butler and David Miller. Special thanks to Jonas Markgraf and Hyunjoo Oh.
#' 
#' @rdname ipwlm
#' @import 'parallel'
#' @import 'gam'
#' @import 'glmnet'
#' @export
#' 
#' 

ipwlm <- function(regression_formula, 
                treatment,
                instrument = NULL,
                data,
                effect_type,
                attrition_type, # "treatment", "observable", "unobservable"
                response_weight_formula = response ~ .,
                response_weight_method = 'logit',
                treatment_weight_formula = data[treatment] ~ .,
                treatment_weight_method = 'logit',
                n_bootstraps = 1000,
                quantiles = c(0.05, 0.95),
                n_cores = 1,
                coef_tol = 10^-5) {
  
  # prop 0: attrition caused by treatment (ATE|R = 1): effect_type = "respondent", attrition_type = "treatment"
  # prop 1: attrition caused by treatment (ATE): effect_type = "population", attrition_type = "treatment"
  # prop 2: attrition caused by treatment and observables (ATE|R = 1): effect_type = "respondent", attrition_type = "observable"
  # prop 3: attrition caused by treatment and observables (ATE): effect_type = "population", attrition_type = "observable"
  # prop 4: attrition caused by treatment, observables, and unobservables (ATE|R = 1): effect_type = "respondent", attrition_type = "unobservable"
  # prop 5: attrition caused by treatment, observables, and unobservables (ATE): effect_type = "population", attrition_type = "unobservable"

  # store data from model for manipulation
  internal_data <- model.frame(regression_formula, data, na.action = NULL)
  # make attrition indicator (non-response = 1; response = 0)
  internal_data$response <- as.numeric(!is.na(internal_data[ , 1]))
  # rename relevant variables
  names(internal_data)[1] <- "outcome"
  names(internal_data)[which(names(internal_data) == treatment)] <- "treatment"
  names(internal_data)[which(names(internal_data) == instrument)] <- "instrument"
  
  ### Calculate Weights for Relevant Estimator
  
  # exception handling for effect type inputs
  if(!(effect_type %in% c("population", "respondent"))){
    stop('effect_type must be either "population" or "respondent"')
  }
  # exception handling for attrition type inputs
  if(!(attrition_type %in% c("treatment", "observable", "unobservable"))){
    stop('attrition_type must be one of "treatment" or "observable" or "unobservable"')
  }
  # exception handling for unobservable weighting without instrument
  if(attrition_type == "unobservable" & is.null(instrument)){
    stop('weighting for attrition on unobservables requires specification on instrument')
  }
  # exception handling for response method type
  if(!(response_weight_method %in% c("logit", "probit", "ridge"))){
    stop('response model method must be one of "logit" or "probit" or "ridge"')
  }
  # exception handling for treatment method type
  if(!(treatment_weight_method %in% c("logit", "probit", "ridge"))){
    stop('treatment model method must be one of "logit" or "probit" or "ridge"')
  }
  
  # if estimating ATE or conditionining on unobservables, compute meaningful response weight ...
  if(effect_type == "population" | attrition_type == "unobservable"){
    if(response_weight_method != "ridge"){
      response_propensity <- gam(formula = response_weight_formula, 
                                 family = binomial(link = response_weight_method),
                                 data = internal_data[ , !(names(internal_data) == "outcome")],
                                 maxit = 1000)
      response_weights <- predict(object = response_propensity, type = "response")
      response_coefs <- coef(response_propensity)
    }
    if(response_weight_method == "ridge"){
      ridge_data_response <- data.matrix(
        model.frame(
          response_weight_formula, 
          internal_data[ , !(names(internal_data) == 'outcome')], 
          na.action = NULL)[ , -1, drop = FALSE])
      # automatically select lambda-optimized model
      response_propensity <- cv.glmnet(y = internal_data$response,
                                       x =  cbind(1, ridge_data_response),
                                    family = "binomial",
                                    alpha = 0)
      response_weights <- predict(response_propensity, 
                                  cbind(1, ridge_data_response), 
                                  type = "response")
      response_coefs <- coef(response_propensity)[-2, ]
    }
    response_weights <- response_weights/sum(response_weights)
    # if conditioning treatment propensity on response weights
    if(attrition_type == "unobservable"){
      internal_data$response_conditioning <- response_weights
    } else {
      internal_data$response_conditioning <- NULL
    }
  }
  # treatment weights for props 2-5; formula determined by attrition_type
  if(treatment_weight_method != "ridge"){
    treatment_propensity <- gam(formula = treatment_weight_formula,
                                family = binomial(link = treatment_weight_method),
                                data = internal_data[ , !(names(internal_data) %in% c("outcome", "instrument", "response"))],
                                maxit = 1000)
    treatment_weights <- predict(object = treatment_propensity, type = "response")
    treatment_coefs <- coef(treatment_propensity)
  }
  if(treatment_weight_method == "ridge"){
    ridge_data_treatment <- data.matrix(
      model.frame(
        treatment_weight_formula, 
        internal_data[ , !(names(internal_data) %in% c("outcome", "instrument", "response"))], 
        na.action = NULL)[ , -1, drop = FALSE])
    treatment_propensity <- cv.glmnet(y = internal_data$treatment,
                                      x =  cbind(1, ridge_data_treatment),
                                      family = "binomial",
                                      alpha = 0)
    treatment_weights <- predict(treatment_propensity, 
                                 cbind(1, ridge_data_treatment),
                                 type = "response")
    treatment_coefs <- coef(treatment_propensity)[-2, ]
  }
  
  # reorient the treatment_weight for units in control
  treatment_weights[internal_data$treatment != 1] <-  (1 - treatment_weights[internal_data$treatment != 1])
  treatment_weights <- treatment_weights/sum(treatment_weights)
  # if estimating ATE|R, multiplying by response weights is trivial
  if(effect_type == "respondent"){
    response_weights <- 1
  }
  # if disregarding treatment propensity
  if(attrition_type == "treatment"){
    treatment_weights <- 1
  }
  # add inverse probability weights to model data
  internal_data$final_weights <- 1/(treatment_weights * response_weights)
  
  ### Estimate Treatment Effect for Relevant Estimator
  treatment_effect_model <- lm(formula = outcome ~ treatment, 
                               weights = final_weights, 
                               data = internal_data)
  
  ### Estimate n Bootstraps of Relevant Estimator
  
  # Make cluster
  bootstrap_cluster <- makeCluster(n_cores)
  # Bootstrap data: random sampling of dataset with replacement
  clusterExport(bootstrap_cluster, c("internal_data",
                                     "n_bootstraps"),
                envir = environment())
  
  # DRM: in the bootstrapping, do we want bootstrapped data sets of the size equal to the number of respondents,
  # or of the full sample?  the former is implemented
  # RWB: I think we want the full sample. Saving your code as comment here in case I'm wrong
  # data = data[sample(x = nrow(data[which(data$R==1),]),
  #                    size = nrow(data[which(data$R==1),]),
  #                    replace = T), ]
  
  np_bootstrap <- parSapply(bootstrap_cluster, 1:n_bootstraps,
                            FUN = function(n) {
                              bootstrap <- lm(formula = outcome ~ treatment,
                                              weights = final_weights,
                                              data = internal_data[sample(nrow(internal_data), replace = T), ])
                              return(bootstrap$coefficients[[2]]) 
                              })
  
  stopCluster(cl = bootstrap_cluster)
  
  ### Print Model Summary & Return Results
  
  confidence_interval <- quantile(np_bootstrap, probs = quantiles, na.rm = T)
  
  # Printing summary result tables
  
  ResultsPrint <- data.frame("Estimate" = treatment_effect_model$coefficients[[2]],
                             "SE" = sd(np_bootstrap),
                             "CI Lower" = confidence_interval[1],
                             "CI Upper" = confidence_interval[2],
                             row.names = NULL)
  cat("--- Treatment Effect with Uncertainty Estimates from", n_bootstraps, "Non-Parametric Bootstraps ---\n",
      "Summary:\n")
  print(ResultsPrint)
  # what we might want to add to the printed summary: number of observations, number attritted,
  # effect_type, attrition_type
  return(list(coefficients = c("treatment" = treatment_effect_model$coefficients[[2]], 
                               "se" = sd(np_bootstrap), 
                               "lower_ci" = confidence_interval[1],
                               "upper_ci" = confidence_interval[2]),
              weights = data.frame(Response = response_weights,
                                   Treatment = treatment_weights,
                                   IPW = internal_data$final_weights),
              weight_models = list(response = response_coefs,
                                   treatment = treatment_coefs),
              effect = effect_type,
              attrition = attrition_type
              ))
}