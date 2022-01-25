#' ---
#' title: "Eigenvalue and Eigenvector in R using Power Method"
#' author: "Rahul Goswami"
#' ---

#' Power Method is a method for finding the eigenvalues and eigenvectors of a matrix.It is a iterative method.
#' Steps for the Power Method are:  
#' Step 1: Initialize the vector v to be a random vector.  
#' Step 2: Initialize the iteration counter to 0.  
#' Step 3: Repeat the following until convergence:    
#'  Step 3.1:   Calculate the matrix-vector product A*v.  
#'  Step 3.2:   Normalize the vector v.  
#'  Step 3.3:   Increment the iteration counter by 1.  
#'  Step 3.4:   If the iteration counter is greater than 100, stop the iteration.  
#'  Step 3.5:   If the iteration counter is less than 100, go to step 3.  
#'  Step 3.6:   Return the eigenvector v.  (Note: The eigenvector is not normalized.)    
#'
#'
#'
#' Let us consider a matrix A.
#' $$ A = \begin{pmatrix}
#' 1 & 2 & 0 \\
#' -2 & 1 & 2 \\
#' 1 & 3 & 1
#' \end{pmatrix} $$
#'
#'  
#' ### Method
#' We will use power method , in which first of all we take a matrix with entry 1,1,1 as initial eigeenvector
#' then further multiply this with the matrix and we will get an another 3 × 1 matrix then we will divide each
#' element of the obtained matrix by maximum of that matrix , then we will repeat same till we get a reliable
#' estimate , and we can further calculate dominant eigenvector by  

#' $$ \lambda = \frac{A \times x.x}{x.x} $$  

#' Putting Matrix in R  `matrix` function
A <- matrix(c(1,2,0,-2,1,2,1,3,1),ncol=3,byrow = TRUE)
vec <- matrix(c(1,1,1),ncol=1)

#' Initializing the iteration counter to 0
iteration <- 0
temp <- c(3,4,5)

while(any(temp != vec)){
    temp <- vec
    vec <- A %*% vec
    vec <- round(vec/max(vec),4)
    iteration <- iteration + 1
    cat("Iteration:",iteration,"\t",vec,"\n")
}


cat("Dominant Eigenvector:",vec, "and Dominant Eigenvalue:",sum((A %*% vec) * vec)/sum( vec * vec))
