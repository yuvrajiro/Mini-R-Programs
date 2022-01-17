#' ---
#' title: "Linear Search in R"
#' author: "Rahul Goswami"
#' ---

#'Linear Search is a searching algorithm that is used to find an element in a sorted array.
#'The basic idea of linear search is to search for an element in a sorted array by comparing it with each element of the array.  
#'The average case time complexity of linear search is O(n)  
#'The worst case time complexity of linear search is O(n)  
#'The best case time complexity of linear search is O(1)  
#'The space complexity of linear search is O(1)  (in-place)  
#''
#' ### Algorithm:
#' 1. Compare the element to be searched with each element of the array
#' 2. If the element is found, return the index of the element

#' defintion of linear search
#' @param vec Vector to be searched
#' @param element Element to be searched
#' @return Index of the element

linear.search <- function(vec, element){
    for(i in 1:length(vec)){
        if(vec[i] == element){
            return(i)
        }
    }
    return("Not Found")
}

#' ### Example
sorted_vec <- c(0,1,2,3,4,5,6,7,8,9)
linear.search(sorted_vec, 5)




