#' ---
#' title: "Inverse of a Matrix in R"
#' author: "Rahul Goswami"
#' ---

#' We will compute the inverse of a matrix using Gauss Jordan Elimination

inv <- function(M){
    # Checking whether the matrix is square or not
    if(nrow(M) != ncol(M)){
        stop("Matrix is not square")
    }
    # Checking whether the matrix is singular or not
    if(det(M) == 0){
        stop("Matrix is singular")
    }
    L <- function(M){
    if(is.matrix(M)){
    n <- ncol(M) # Number of columns
    m <- nrow(M) # Number of rows
    s <- min(n,m)
    pivot <- M[1,1] # Pivot element
    if(pivot == 0){
        stop("Pivot element is zero,Permutate the matrix")
    }
    for(i in 2:s){
        M[i,] <- M[i,] - (M[i,1]/pivot) * M[1,]
    }
    M[-1,-1] <- L(M[-1,-1])
    }
    M
}

U <- function(M){
    if(is.matrix(M)){
    n <- ncol(M) # Number of columns
    m <- nrow(M) # Number of rows
    s <- min(n,m)
    pivot <- M[s,s] # Pivot element
    for(i in (s-1):1){
        M[i,] <- M[i,] - (M[i,s]/pivot) * M[s,]
    }
    M[-s,-s] <- U(M[-s,-s])
    }
    M
}


RREF <- function(M){
    M <- U(L(M))
    n <- ncol(M) # Number of columns
    m <- nrow(M) # Number of rows
    s <- min(n,m)
    for(i in 1:s){
        M[i,] <- M[i,]/M[i,i] 
    }
    M
    
}
Aug_mat = cbind(M,diag(x=1,nrow(M)))
R_Aug_mat = RREF(Aug_mat)
inv_mat = R_Aug_mat[,(1:nrow(M))+nrow(M)]
inv_mat


}

#' Example
M <- matrix(c(8,1,6,3,5,7,4,9,2),nrow = 3,byrow = TRUE)
print(M)
print(inv(M))