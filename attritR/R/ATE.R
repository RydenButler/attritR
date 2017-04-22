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
  if(effectType == "All"){
      print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
        'Coefficients:\n',
        '         ', 'Mean       ','Median  ','SE      ','lower 95%','upper 95%\n',
        'All      ', Results$MeanEst[2],Results$MedianEst[2], Results$SE[2],Results$MeanEst[2]-(1.96*Results$SE[2]), Results$MeanEst[2]+(1.96*Results$SE[2])))
    invisible(Results)
  } 
  if(effectType == "Respondent"){
    print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Coefficients:\n',
      '          ', 'Mean       ','Median  ','SE      ','lower 95%','upper 95%\n',
      'Respondent', Results$MeanEst[2],Results$MedianEst[2], Results$SE[2],Results$MeanEst[2]-(1.96*Results$SE[2]), Results$MeanEst[2]+(1.96*Results$SE[2])))
    invisible(Results)
  }
  if(effectType == "Both"){  # this does not work yet.
    print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Coefficients:\n',
      '          ', 'Mean       ','Median  ','SE      ','\n',
      'Respondent', Results$RespondentMean[2],Results$RespondentMedian[2], Results$RespondentSE[2],'\n',
      'All       ', Results$AllMean[2], Results$AllMedian[2], Results$AllSE[2]))
      invisible(Results)
  }
}
  
  