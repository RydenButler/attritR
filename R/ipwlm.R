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
#' of the form Y ~ D | Z + D + X | X, where Y is the outcome, D is the 
#' treatment, Z is an instrumental variable and X represents any additional 
#' covariates. Symbolically, the regression equation is divided into its three
#' relevant parts. The first part describes the treatment effect equation, the 
#' second part describes the covariates used in modeling response propensity, and
#' the third part represents the covariates used in modeling treatment propensity.
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
#' @param response_weight_method A character string specifying the model 
#' which estimates subjects' response propensity. By default, 
#' \code{response_weight_method} is set to \code{'logit'}, which 
#' fits a binomial logistic regression model. Other valid inputs include 
#' \code{'probit'}, which fits a binomial probit regression, and \code{'ridge'},
#' which fits a binomial logistic ridge regression. 
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
#' @import 'Formula'
#' @import 'parallel'
#' @import 'gam'
#' @import 'glmnet'
#' @export
#' 
#' 

ipwlm <- function(regression_formula, 
                  data,
                  effect_type,
                  attrition_type, # "treatment", "observable", "unobservable"
                  response_weight_method = 'logit',
                  treatment_weight_method = 'logit',
                  n_bootstraps = 1000,
                  quantiles = c(0.05, 0.95),
                  n_cores = 1) {
  # prop 0: attrition caused by treatment (ATE|R = 1): effect_type = "respondent", attrition_type = "treatment"
  # prop 1: attrition caused by treatment (ATE): effect_type = "population", attrition_type = "treatment"
  # prop 2: attrition caused by treatment and observables (ATE|R = 1): effect_type = "respondent", attrition_type = "observable"
  # prop 3: attrition caused by treatment and observables (ATE): effect_type = "population", attrition_type = "observable"
  # prop 4: attrition caused by treatment, observables, and unobservables (ATE|R = 1): effect_type = "respondent", attrition_type = "unobservable"
  # prop 5: attrition caused by treatment, observables, and unobservables (ATE): effect_type = "population", attrition_type = "unobservable"
  
  regression_formula <- as.Formula(regression_formula)
  
  data$response <- as.numeric(!is.na(data[ , as.character(attributes(terms.formula(regression_formula))$variables[[2]])]))
  data$treatment <- data[ , attributes(terms.formula(formula(regression_formula, rhs = 1)))$term.labels]
  
  response_weight_formula <- update(formula(regression_formula, lhs = 0, rhs = 2), response ~ .)
  treatment_weight_formula <- update(formula(regression_formula, lhs = 0, rhs = 3),  treatment ~ .)

  ### Calculate Weights for Relevant Estimator
  
  # exception handling for effect type inputs
  if(!(effect_type %in% c("population", "respondent"))){
    stop('effect_type must be either "population" or "respondent"')
  }
  # exception handling for attrition type inputs
  if(!(attrition_type %in% c("treatment", "observable", "unobservable"))){
    stop('attrition_type must be one of "treatment" or "observable" or "unobservable"')
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
                                 data = data,
                                 maxit = 1000)
      response_weights <- predict(object = response_propensity, type = "response")
      response_coefs <- coef(response_propensity)
    }
    if(response_weight_method == "ridge"){
      ridge_data_response <- model.matrix(response_weight_formula,
                                          data = data)
      # automatically select lambda-optimized model
      response_propensity <- cv.glmnet(y = data$response,
                                       x =  ridge_data_response,
                                       family = "binomial",
                                       alpha = 0,
                                       nlambda = 1000)
      response_weights <- predict(response_propensity, 
                                  ridge_data_response, 
                                  type = "response")
      response_coefs <- coef(response_propensity)[-2, ]
    }
    response_weights <- response_weights/sum(response_weights)
    # if conditioning treatment propensity on response weights
    if(attrition_type == "unobservable"){
      data$response_conditioning <- as.vector(response_weights)
      treatment_weight_formula <- update(treatment_weight_formula, ~ . + response_conditioning)
    }
  }
  # treatment weights for props 2-5; formula determined by attrition_type
  if(treatment_weight_method != "ridge"){
    treatment_propensity <- gam(formula = treatment_weight_formula,
                                family = binomial(link = treatment_weight_method),
                                data = data,
                                maxit = 1000)
    treatment_weights <- predict(object = treatment_propensity, type = "response")
    treatment_coefs <- coef(treatment_propensity)
  }
  if(treatment_weight_method == "ridge"){
    ridge_data_treatment <- model.matrix(treatment_weight_formula,
                                         data = data)
    treatment_propensity <- cv.glmnet(y = data$treatment,
                                      x =  ridge_data_treatment,
                                      family = "binomial",
                                      alpha = 0,
                                      nlambda = 1000)
    treatment_weights <- predict(treatment_propensity, 
                                 ridge_data_treatment,
                                 type = "response")
    treatment_coefs <- coef(treatment_propensity)[-2, ]
  }
  
  # reorient the treatment_weight for units in control
  treatment_weights[data$treatment != 1] <-  (1 - treatment_weights[data$treatment != 1])
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
  data$final_weights <- as.vector(1/(treatment_weights * response_weights))
  
  ### Estimate Treatment Effect for Relevant Estimator
  treatment_effect_model <- lm(formula = formula(regression_formula, rhs = 1), 
                               weights = final_weights, 
                               data = data)
  
  ### Estimate n Bootstraps of Relevant Estimator
  
  # Make cluster
  bootstrap_cluster <- makeCluster(n_cores)
  # Bootstrap data: random sampling of dataset with replacement
  clusterExport(bootstrap_cluster, c("regression_formula",
                                     "data",
                                     "n_bootstraps"),
                envir = environment())
  clusterEvalQ(bootstrap_cluster, library("Formula"))
  
  # DRM: in the bootstrapping, do we want bootstrapped data sets of the size equal to the number of respondents,
  # or of the full sample?  the former is implemented
  # RWB: I think we want the full sample. Saving your code as comment here in case I'm wrong
  # data = data[sample(x = nrow(data[which(data$R==1),]),
  #                    size = nrow(data[which(data$R==1),]),
  #                    replace = T), ]
  
  np_bootstrap <- parSapply(bootstrap_cluster, 1:n_bootstraps,
                            FUN = function(n) {
                              bootstrap <- lm(formula = formula(regression_formula, rhs = 1),
                                              weights = final_weights,
                                              data = data[sample(nrow(data), replace = T), ])
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
                                   IPW = data$final_weights),
              weight_models = list(response = response_coefs,
                                   treatment = treatment_coefs),
              effect = effect_type,
              attrition = attrition_type
              ))
}