# It is a recursive function that implements the quicksort algorithm.
# It takes as input a vector
# It returns the sorted vector.

#' @param vec A vector of numbers.'
#' @return A sorted vector of @code{vec}.'
#' @examples 
#' qs(c(1,2,5,4,6,10))'

qs <- function(vec)
  {
    if(length(vec) > 1)
      {
        pivot <- vec[1]
        low <- qs(vec[vec < pivot])
        mid <- vec[vec == pivot]
        high <- qs(vec[vec > pivot])
        c(low, mid, high)
      }
  
    else vec
  }