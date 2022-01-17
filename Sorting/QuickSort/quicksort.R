#' ---
#' title: "Quick Sort in R"
#' author: "Rahul Goswami"
#' ---




#' It is a recursive function that implements the quicksort algorithm.  
#' It takes as input a vector  
#' It returns the sorted vector.
#' This algorithm was invented by Tony Hoare.  

#' Algorithm:
#' 1. Pick an element as pivot
#' 2. Partition the vector around the pivot
#' 3. Sort the two partitions
#' 4. Merge the two sorted partitions
#' 5. Repeat the above steps until the vector is sorted

#' @param vec Vector to be sorted
#' @return Sorted vector




qs <- function(vec)
  {
    # If the vector has more than one element
    if(length(vec) > 1)
      {
        # Pick the first element as pivot (can be any element)
        pivot <- vec[1]
        low <- qs(vec[vec < pivot])
        mid <- vec[vec == pivot]
        high <- qs(vec[vec > pivot])
        c(low, mid, high)
      }
  
    else vec
}

#' ### Example
#' Take A vector 
vector = c(7,6,5,1,0,9,5,5)
qs(vector)