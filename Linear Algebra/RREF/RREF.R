#' ---
#' title: "Reduced Row Echloen form using Gauss Elimination in R"
#' author: "Rahul Goswami"
#' ---

#' A matrix is in Reduced Row Echloen form if its satisfy following three conditions :  
#' 1. Any row containing non-zero entries preceeds any row containing only zeros.  
#' 2. The first non-zero entry in each row is the only non-zero entries in its column  
#' 3. The first non-zero entries in each row is 1 and it occurs to the column to the right of the first non-zero entry in the preceeding row  
#' [Definition Taken from Friedberg's Linear Algebra](https://www.pearson.com/us/higher-education/program/Friedberg-Linear-Algebra-5th-Edition/PGM1939358.html)


#' Program to calculate the row echelon form of a matrix  
#' A function L to make the Lower Triangular Matrix
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
#' A function U to make the Upper Triangular Matrix
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
#' A function to compute RREF
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

#' Example of a matrix
M <- matrix(c(8,1,6,1,3,5,7,1,4,9,2,1),nrow = 3,byrow = TRUE)

#' Printing the matrix
print(M)

#' Printing the RREF
print(RREF(M))

