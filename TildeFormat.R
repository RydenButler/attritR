runRegression <- function(formula, treatment, instrument, data, method) {
  ModelData <- model.frame(formula, data)
  y <- ModelData[ , 1]
  return(ModelData)
}

A <- c(1,2,3)
B <- c(0,1,0)
C <- c(100, 200, 1000)
D <- c('a', 'b', 'c')
TestData <- data.frame(cbind(A,B,C,D), stringsAsFactors = F)

runRegression(A ~ D | C, B, TestData, NULL)
