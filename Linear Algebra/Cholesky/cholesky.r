#' ---
#' title: "Cholesky and LDL decomposition in R"
#' author: "Rahul Goswami"
#' ---


#'  In LU decomposiition we have factored the matrix into two matrices L and U such that A = LU. L represents the lower triangular matrix and U represents the upper triangular matrix. 
#' Where L have diagonal elements as 1, U has diagonal elements not necessarily 1. In symmetric positive definite matrix, L is lower triangular matrix and U is upper triangular matrix.
#' We can factorize a symmetric positive definite matrix into two matrices L and D such that $A = LDL^T$. L represents the lower triangular matrix and D represents the diagonal matrix.
#' Whereas cholesky decomposition is a factorization of matrix $A = U^TU$. where U is upper triangular matrix and 
#' $U = \sqrt(D)L^T$.  


#' Writing the code in R

Cholesky <- function(M){
    # Check whether the matrix is Positve Symmetric or not
    # Symmetric
    for(i in 1:nrow(M)){
        for(j in 1:ncol(M)){
            if(M[i,j] != M[j,i]){
                stop("Matrix is not symmetric")
            }
        }
    }

    # Check whether the matrix is Positive Definite or not
    if(M[1,1] <= 0){
        stop("Matrix is not Positive Definite")
    }
    for(i in 2:nrow(M))
    {
        if(det(M[1:i,1:i]) <= 0){
            stop("Matrix is not Positive Definite")
        }
    }
    # LU Decomposition
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
  # Cholesky Decomposition
  D <- LU_decomposition(M)
  L <- D$L
  U <- D$U
  for(i in 1:nrow(L)){
      for(j in 1:ncol(L)){
          if(L[i,j] <0){
              L[i,j]<- -sqrt(-L[i,j])
          }
            else{
                L[i,j]<- sqrt(L[i,j])
            }
      }
  }
  A <- L
  diag(A) <- sqrt(diag(U))

A

}

#' Example
M <- matrix(c(2,-1,0,-1,2,-1,0,-1,2),nrow = 3, ncol = 3,byrow = TRUE)
Cholesky(M)
