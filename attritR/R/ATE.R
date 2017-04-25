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
      cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
        'Summary:\n',
        '          ', 'Mean ','Median','SE  ','low95%','up95%\n',
        'Population', format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3), 
        format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3), 
        format(round(Results$SE$Pop[2], digit=3), nsmall=3), 
        format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3), 
        format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3))
    invisible(Results)
  } 
  if(effectType == "Respondent"){
    cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n',
      '          ', 'Mean ','Median','SE  ','low95%','up95%\n',
      'Respondent',  format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3), 
      format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3), 
      format(round(Results$SE$Resp[2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3))
    invisible(Results)
  }
  if(effectType == "Both"){  ## to do: aligning columns consistently
    cat('--- ATE Results from', nBoots, 'Bootstraps ---\n',
      'Summary:\n',
      '          ', 'Mean ','Median','SE  ','low95%','up95%\n',
      'Respondent',  format(round(Results$MeanEst$Resp[2], digit=3), nsmall=3), 
      format(round(Results$MedianEst$Resp[2], digit=3), nsmall=3), 
      format(round(Results$SE$Resp[2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Resp[1,2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Resp[2,2], digit=3), nsmall=3),'\n',
      'Population', format(round(Results$MeanEst$Pop[2], digit=3), nsmall=3), 
      format(round(Results$MedianEst$Pop[2], digit=3), nsmall=3), 
      format(round(Results$SE$Pop[2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Pop[1,2], digit=3), nsmall=3), 
      format(round(Results$Quantiles$Pop[2,2], digit=3), nsmall=3))
    invisible(Results)
  }
}
  
  