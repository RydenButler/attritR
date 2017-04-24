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
      print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
        'Summary:\n',
        '          ', 'Mean       ','Median  ','SE      ','lower 95%','upper 95%\n',
        'Population', Results[["MeanEst"]]$Pop[2],Results[["MedianEst"]]$Pop[2],Results[["SE"]]$Pop[2],Results[["MeanEst"]]$Pop[2]-(1.96*Results[["SE"]]$Pop[2]),Results[["MeanEst"]]$Pop[2]-(1.96*Results[["SE"]]$Pop[2])))
    invisible(Results)
  } 
  if(effectType == "Respondent"){
    print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n',
      '          ', 'Mean       ','Median  ','SE      ','lower 95%','upper 95%\n',
      'Respondent',Results[["MeanEst"]]$Resp[2],Results[["MedianEst"]]$Resp[2],Results[["SE"]]$Resp[2],Results[["MeanEst"]]$Resp[2]-(1.96*Results[["SE"]]$Resp[2]),Results[["MeanEst"]]$Resp[2]-(1.96*Results[["SE"]]$Resp[2]))) 
    invisible(Results)
  }
  if(effectType == "Both"){  ## to do: get rid of "NULL" at end of output; aligning columns consistently
    print(cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n',
      '          ', 'Mean     ','Median  ','SE      ','lower 95%','upper 95%\n',
      'Respondent',  Results[["MeanEst"]]$Resp[2],Results[["MedianEst"]]$Resp[2],Results[["SE"]]$Resp[2],Results[["MeanEst"]]$Resp[2]-(1.96*Results[["SE"]]$Resp[2]),Results[["MeanEst"]]$Resp[2]-(1.96*Results[["SE"]]$Resp[2]),'\n',
      'Population', Results[["MeanEst"]]$Pop[2],Results[["MedianEst"]]$Pop[2],Results[["SE"]]$Pop[2],Results[["MeanEst"]]$Pop[2]-(1.96*Results[["SE"]]$Pop[2]),Results[["MeanEst"]]$Pop[2]-(1.96*Results[["SE"]]$Pop[2]),''))      
    invisible(Results)
  }
}
  
  