#' ---
#' title: "Bisection Methos in R"
#' author: "Rahul Goswami"
#' ---


#' Bisection Method is a method for finding the root of a function. Suppose we want to find the root of the equation
#' $$ f(x) = 0 $$ in the interval $[a,b]$. Steps for the Bisection Method are:
#'
#' 1. Take the midpoint of the interval $[a,b]$ , $x_0 = (a+b)/2$
#' 2. Evaluate the function at $x_0$ , $f(x_0)$
#' 3. If $f(x_0) = 0$ then the root is found. 
#' 4. If sign of $f(x_0)$ is same as sign of $f(a)$ then $a = x_0$ and $b = b$ otherwise $a = a$ and $b = x_0$
#' 5. Repeat the process until the interval converges to a root.   
#' 
#'
#' ### Example
#' f(x) = x^2 + 4x - 7  in the interval $[-10,0]$
#' f(-10) = -10^2 + 4*(-10) - 7 = -70 and f(0) = 0 Opposite sign of f(-10) and f(0) so we will start with $a = -10$ and $b = 0$
#'

f <- function(x) {
  x^2 + 4*x - 7
}

#' Initial points
a <- -10
b <- 0
count <- 0
midpoint <- c()
functionvalue <- c()
while(abs(a-b) > 0.00001) {
  count = count + 1
  x <- (a+b)/2  # midpoint
  midpoint <- c(midpoint,x) # store the midpoint
  fx <- f(x)    # evaluate f(x) at midpoint
  functionvalue <- c(functionvalue,fx) # store the function value
  if(sign(f(a)) == sign(fx)) {
    a <- x
  } else {
    b <- x
  }
  cat("Iteration:",count,"\t","f(x) =",fx,"midpoint:",x,"New interval:",a,"to",b,"\n")
}

#'### Plottings

{plot(f,xlim=c(-10,0),ylim=c(-13,7),xlab="x",ylab="f(x)",main="Bisection Method")
points(midpoint,functionvalue,pch=19,col="red",cex=0.5)
lines(c(0,-10),c(0,0),col="blue",lwd=0.5)}
