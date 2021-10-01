knapsack_dynamic <- function(x, W){
  combs = matrix(-1, nrow = nrow(x)+1, ncol = W+1)
  combs[1, ] <- 0
  combs[ , 1] <- 0
  
  m = function(i,j){
    if(i == 0 || j <= 0){
      combs[i, j] <<- 0
      return(0)
    }
    
    if(combs[i-1, j] == -1){
      combs[i-1, j] <<- m(i-1, j)
    }
    
    if(x$w[i-1] > j || j - x$w[i-1] <= 0){
      combs[i, j] <<- combs[i-1, j]
    }else{
      if (combs[i, j-x$w[i-1]] == -1){
        combs[i, j-x$w[i-1]] <<- m(i, j-x$w[i-1])
      }
      combs[i, j] <<- max(combs[i-1,j], combs[i-1, j-x$w[i-1]] + x$v[i-1])
      }
  }

  results = m(length(x$w)+1, W+1)
  
  elem <- c()
  
  j <- W + 1
  i <- length(x$w)+1
  
  while ( i > 1) {
    if(combs[i, j] != combs[i-1, j]){
      elem <- append(elem, i-1)
      j <-  j - x$w[i-1]
    }
    i = i-1
  }
  
  return(list(value = round(results) , elements = elem))
}
