#'@description This is the dynamic implementation of knapsack.
#'
#'@field x. Data frame.
#'@field W. Value.
#'@import methods
#'@return List
#'@export knapsack_dynamic 

knapsack_dynamic <- function(x, W){
  combs = matrix(-1, nrow = nrow(x)+1, ncol = W+1)
  combs[1, ] <- 0
  combs[ , 1] <- 0

  for (i in 2:nrow(x)) {
    for (j in 1:W) {
      if(x$w[i] > j){
        combs[i,j] <- combs[i-1,j]
      }else{
        combs[i,j] <- max(combs[i-1,j], combs[i-1, j-x$w[i]] + x$v[i]) 
      }         
    }
  }
  
  
  results = combs[nrow(x),W]
  
  elem <- c()
  
  j <- W
  i <- length(x$w)
  
  while ( i > 1) {
    if(combs[i, j] != combs[i-1, j]){
      elem <- append(elem, i)
      j <-  j - x$w[i]
    }
    i = i-1
  }
  
  return(list(value = round(results) , elements = elem))
}
