wrapperFunction <- function(regressionFormula, 
                            instrumentFormula, 
                            data,
                            p_W_Formula = R ~ .,
                            p_W_Method = binomial(link = logit),
                            PiFormula = D ~ .,
                            PiMethod = binomial(link = logit),
                            nBoots = 1000,
                            quantiles = c(0.05, 0.95),
                            effectType = 'Respondent'
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
  Output <- cat('--- ATE Results from ', nBoots, 'Bootstraps ---\n',
                'Mean  ', Results$MeanEst[2], '\n',
                'Median', Results$MedianEst[2], '\n',
                'SE    ', Results$SE[2])
}
  
  