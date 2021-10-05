#' brute_force_knapsack
#'
#'@description This is the brute force implementation of knapsack.
#'@usage brute_force_knapsack(x, W, parallel)
#'@param x Data frame.
#'@param W Value.
#'@param parallel Value.
#'@import methods
#'@import parallel
#'@return List
#'@export brute_force_knapsack

brute_force_knapsack <-function(x, W, parallel = FALSE){
  
  stopifnot("You should give a df." = is.data.frame(x),
            "W should be a number." = is.numeric(W),
            "W should be positive." = W > 0,
            "You should give a df with col names: (w, v)." = colnames(x) %in% c("w","v"),
            "You should give positive values for x." = all(x >= 0))
  
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
    
    #https://stackoverflow.com/a/50571533
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      cores <- 2L
    } else {
      # use all cores in devtools::test()
      cores <- parallel::detectCores()
    }
    
    
    all_combs <- matrix(unlist(mclapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)], mc.cores = cores))), nrow = nrow(x))
    #all_combs <- sapply(1:(big_O), function(i) as.integer(intToBits(i)[1:nrow(x)]))
    
    combin <- mclapply(1:ncol(all_combs),  FUN = check_combs, x=x, W=W, mc.cores = cores)
    
    max_pos = which.max(combin)
    max_val = combin[[max_pos]]
    elements <- which(intToBits(max_pos) > 0)
    return(list(value = round(max_val,0), elements = elements))
  }
}