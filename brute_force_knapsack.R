brute_force_knapsack <-function(x, W, parallel = FALSE){
  big_O <- 2^(nrow(x))-1
  
  check_combs <- function(x, W, i){
    temp_val <- sum(x$v * all_combs[,i])
    temp_weight <- sum(x$w * all_combs[,i])
    return(ifelse(temp_weight <= W, temp_val, NA))
  }
  
  if(parallel == FALSE){
    
    #all_combs <- matrix(unlist(lapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)]))), nrow = nrow(x))
    all_combs <- sapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)]))
    
    combin <- lapply(1:ncol(all_combs),  FUN = check_combs, x=x, W=W)
    
    max_pos = which.max(combin)
    max_val = combin[[max_pos]]
    elements <- which(intToBits(max_pos) > 0)
    return(list(value = round(max_val,0), elements = elements))
    
  }else{
    
    cores <- detectCores()
    all_combs <- matrix(unlist(mclapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)], mc.cores = cores))), nrow = nrow(x))
    #all_combs <- sapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)]))
    
    combin <- mclapply(1:ncol(all_combs),  FUN = check_combs, x=x, W=W, mc.cores = cores)
    
    max_pos = which.max(combin)
    max_val = combin[[max_pos]]
    elements <- which(intToBits(max_pos) > 0)
    return(list(value = round(max_val,0), elements = elements))
  }
}