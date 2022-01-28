#' ---
#' title: "LU Decomposition in R"
#' author: "Rahul Goswami"
#' ---

#' LU decomposition is a factorization method matrix A into two matrices L and U such that A = LU. L represents the lower triangular matrix and U represents the upper triangular matrix.
#'


#' Now we will write same code in R
LU_decomposition <- function(M){
    L <- 1
    if(is.matrix(M)){
    n <- ncol(M) # Number of columns
    m <- nrow(M) # Number of rows
    s <- min(n,m)
    L <- diag(x = 1 , nrow = s, ncol = s) # Lower triangular matrix
    pivot <- M[1,1] # Pivot element
    if(pivot == 0){
        stop("Pivot element is zero,Permutate the matrix")
    }
    for(i in 2:s){
        L[i,1] <- M[i,1]/pivot
        M[i,] <- M[i,] - (M[i,1]/pivot) * M[1,]
    }
    K <- LU_decomposition(M[-1,-1])
    M[-1,-1] <- K[[2]]
    L[-1,-1] <- K[[1]]
    }
    list(L = L, U = M)
}
    


#' Example:
M <- matrix(c(8,1,6,3,5,7,4,9,2),nrow = 3,byrow = TRUE)
DECOMPO <- LU_decomposition(M)
print(DECOMPO$L)
print(DECOMPO$U)

#' Checking
DECOMPO$L %*% DECOMPO$U