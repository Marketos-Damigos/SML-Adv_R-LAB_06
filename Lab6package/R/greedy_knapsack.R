#' greedy_knapsack 
#'
#'@description This is the greedy implementation of knapsack.
#'@usage greedy_knapsack  (x, W)
#'@param x Data frame.
#'@param W Value.
#'@import methods
#'@return List
#'@export greedy_knapsack 

greedy_knapsack <- function(x, W){
  
  if(is.data.frame(x) == FALSE || colnames(x) != c("w","v")){
    stop("You should give a df with correct columns.")
  }
  if(W <= 0 || is.integer(x[,1]) == FALSE || is.numeric(W) == FALSE || length(W) != 1 || all(x < 0)){
    stop("You should give a df with positive values.")
  }
  
  x = x[x$w <= W,]
  x$ratio = x$v / x$w
  
  x = x[order(x$ratio, decreasing = TRUE),]
  
  temp = list(val = 0, weight = 0, elements = c())

  
  for(i in 1:nrow(x)){
    temp$val = temp$val + x$v[i]
    temp$weight = temp$weight + x$w[i]
    temp$elements = c(temp$elements, rownames(x[i,]))
    if((temp$weight + x$w[i+1]) > W | nrow(x) == i){
      break
    }
  }
  
  
  return(list(value = round(temp$val), elements = as.integer(temp$elements)))
    
}