qs <- function(vec) {
  
  if(length(vec) > 1) {
    
    pivot <- vec[1]
    
    low <- qs(vec[vec < pivot])
    mid <- vec[vec == pivot]
    high <- qs(vec[vec > pivot])
    
    c(low, mid, high)
    
    
  }
  
  else vec
  
}