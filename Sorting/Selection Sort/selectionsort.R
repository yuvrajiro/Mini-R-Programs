#' ---
#' title: "Selection Sort in R"
#' author: "Rahul Goswami"
#' ---

#' This function implements the selection sort algorithm.
#' The basic idea of selection sort is to pick the smallest element from the unsorted list and place it at the beginning.
#' It's average case time complexity is O(n^2)  

#'
#' Algorithm:
#' 1. Pick the smallest element from the unsorted list and place it at the beginning
#' 2. Repeat the above step until the entire list is sorted

#' It is an improvement over bubble sort. It is a stable sorting algorithm.

#' Let us create a function that implements the selection sort algorithm.

selection.sort <- function(vec){
    # Iterate over the vector
    for(i in 1:length(vec)){
        # Initialize the index of the smallest element
        min_index <- i
        # Iterate over the unsorted list
        for(j in i:length(vec)){
            # If the element at j is smaller than the element at min_index
            if(vec[j] < vec[min_index]){
                # Update the index of the smallest element
                min_index <- j
            }
        }
        # Swap the smallest element with the element at i
        temp <- vec[i]
        vec[i] <- vec[min_index]
        vec[min_index] <- temp
    }
    vec
}

#' ### Example
vector <- c(7,6,5,1,0,9,5,5)
selection.sort(vector)
