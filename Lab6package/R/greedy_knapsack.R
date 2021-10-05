#' greedy_knapsack 
#'
#'@description This is the greedy implementation of knapsack.
#'@usage greedy_knapsack  (x, W)
#'@param x Data frame.
#'@param W Value.
#'@import methods
#'@return List
#'@export greedy_knapsack 

stopifnot("You should give a df." = is.data.frame(x),
          "W should be a number." = is.numeric(W),
          "W should be positive." = W > 0,
          "You should give a df with col names: (w, v)." = colnames(x) %in% c("w","v"),
          "You should give positive values for x." = all(x >= 0))
  
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