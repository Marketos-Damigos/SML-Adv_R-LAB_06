#' knapsack_dynamic 
#'
#'@description This is the dynamic implementation of knapsack.
#'@usage knapsack_dynamic (x, W)
#'@param x Data frame.
#'@param W Value.
#'@import methods
#'@return List
#'@export knapsack_dynamic 

knapsack_dynamic <- function(x, W){
  
  if(is.data.frame(x) == FALSE || colnames(x) != c("w","v")){
    stop("You should give a df with correct columns.")
  }
  if(W <= 0 || is.integer(x[,1]) == FALSE || is.numeric(W) == FALSE || length(W) != 1 || all(x < 0)){
    stop("You should give a df with positive values.")
  }
  
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
