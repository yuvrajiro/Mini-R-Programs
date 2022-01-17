#' ---
#' title: "Inserion Sort in R"
#' author: "Rahul Goswami"
#' ---

#' This function implements the insertion sort algorithm
#' The basic idea of insertion sort is to maintain a sorted sublist in the lower positions of the list, and then pick
#' the element from the upper part and put it in the correct position.
#' It's average case time complexity is O(n^2)



insertion.sort <- function(vec){
    # Iterate over the vector
    for(i in 2:length(vec)){
        # Initialize the index of the element to be inserted
        j <- i
        # While the index of the element to be inserted is greater than 0
        while(j > 1 && vec[j] < vec[j-1]){
            # Swap the elements
            temp <- vec[j]
            vec[j] <- vec[j-1]
            vec[j-1] <- temp
            # Decrement the index of the element to be inserted
            j <- j - 1
        }
    }
    vec
}


#' ### Example
vector <- c(7,6,5,1,0,9,5,5)
insertion.sort(vector)