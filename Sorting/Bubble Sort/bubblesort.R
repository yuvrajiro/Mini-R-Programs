#' ---
#' title: "Bubble Sort in R"
#' author: "Rahul Goswami"
#' ---


#'This function implements the bubble sort algorithm. Let us consider a vector of size n. Intentionally we
#' will take an asort of the vector. The bubble sort algorithm is a simple sorting algorithm that works by repeatedly
#' swapping the adjacent elements if they are in wrong order.

bubble.sort <- function(vec){
    # Index of the last element
    last_index <- length(vec)
    # 
    for(i in last_index:2){
        for(j in 1:(i-1)){
            # Swap if the element at j is greater than the element at j+1
            if(vec[j] > vec[j+1]){
                # Swap the elements
                temp <- vec[j]
                vec[j] <- vec[j+1]
                vec[j+1] <- temp
            }
        }
    }
    vec

}

#' ### Example  

a <- c(7,6,5,1,0,9,5,5)
bubble.sort(a)

