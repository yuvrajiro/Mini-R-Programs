### Functions (briefly described using comments in code)
name_of_function_1 <- function(x, k){
  stopifnot(k > 1)

  # Dataframe Creation
  h <- (hist(x , k, plot = FALSE))
  min.bound <- h$breaks[-length(h$breaks)]
  max.bound <- (h$breaks)[-1]
  freq <- h$counts
  DF <- data.frame(min.bound , max.bound , freq)

  # Mean of Input
  Data_m <- mean(x)

  #Standard Deviation of Input
  Data_sd <- sd(x)

  #  the mean computed through the tabled data
  DF_m <- sum(h$mids * h$counts)/sum(h$counts)

  # the standard deviation through the tabled data
  DF_sd <- sqrt(sum(((h$mids-DF_m)^2) * h$counts)/sum(h$counts))

  # Mean Error
  Mean_error <- sqrt((Data_m - DF_m)^2)

  # Sd_error
  Sd_error <- sqrt((Data_sd - DF_sd)^2)

  return(list(DF = DF , Data_m = Data_m ,Data_sd = Data_sd , Df_m = DF_m ,DF_sd = DF_sd ,Mean_error =  Mean_error ,Sd_error =  Sd_error))

}


name_of_function_2 <- function(x , max_k){
  opt_error = Inf
  for(k in 2:length(x)){
    error = name_of_function_1(x , k)$Mean_error
    if(error < opt_error){
      opt_error <- error
      opt_k <- k

    }

  }
  return(opt_k)
}



name_of_function_3 <- function(x , max_k){
  opt_error = Inf
  for(k in 2:length(x)){
    error = name_of_function_1(x , k)$Sd_error
    #print(paste("k : ",k , "error: ", error))
    if(error < opt_error){
      opt_error <- error
      opt_k <- k

    }

  }
  return(opt_k)
}
name_of_function_4 <- function(x,  k){

  listin <- name_of_function_1(x,k)
  first_formula_without_hist <- function(x,k){sqrt(sum((x-mean(x)^2))/length(x))}
  first_formula_with_hist <- function(x,k){
    h <- hist(x, k , plot = FALSE)
    mea <- sum(h$mids*h$counts)/sum(h$counts)
    return(sqrt((sum(h$counts*(h$mids - mea)^2))/(sum(h$counts))))}
  second_formula_without_hist <- function(x,k){sqrt((sum(x^2)/length(x)) - mean(x)^2)}
  second_formula_with_hist <- function(x,k){
    h <- hist(x, k , plot = FALSE)
    mea <- sum(h$mids*h$counts)/sum(h$counts)
    sqrt(sum((h$mids^2)*h$counts)/sum(h$counts) - mea^2)

  }

  list_of_formula <- c(first_formula_with_hist , first_formula_without_hist ,
                       second_formula_with_hist , second_formula_without_hist)

  timings <- c()

  for(fun in list_of_formula){
    start <-Sys.time()
    for(i in 1:100){
      fun(x,k)
    }
    stop <- Sys.time()

    timings <- c(timings , stop - start)
  }

  listin$times <- timings
  listin

}


### Code for using functions
set.seed(4321)
x<- rnorm(5000)
k = 100 # taking k = 100
start <- Sys.time()
res_of_function_1<- name_of_function_1(x, k)
stop <- Sys.time()
print("Results of First Function")
cat("\n")
cat("\n")
df = res_of_function_1[[1]]
print(df);cat("\n")
print(paste("Mean of x", res_of_function_1[[2]] ))
print(paste("Standard Deviation of x", res_of_function_1[[3]] ))
print(paste("Mean Calculated from DataFrame", res_of_function_1[[4]] ))
print(paste("Standard Devation Calculated from DataFrame",res_of_function_1[5] ))
print(paste("Mean Error",res_of_function_1[[6]],"\n"))
print(paste("Standard Deviation Error", res_of_function_1[[7]]))
print(paste("Time Taken in execution of first function is :", stop -start ))

cat("\n")
cat("\n")


start <- Sys.time()
res_of_function_2<- name_of_function_2(x ,k)
stop <- Sys.time()
cat("\n")
cat("\n")

print("Results of Second Function ")
print(paste("Optimal k value which minimize mean error is ", res_of_function_2 ))
print(paste("Time Taken in execution of second function is :", stop -start ))
cat("\n")
cat("\n")


start <- Sys.time()
res_of_function_3<- name_of_function_3(x,k)
stop <- Sys.time()
cat("\n")
cat("\n")
print("Results of Third Function ")
print(paste("Optimal k value which minimize standard deviation error is ", res_of_function_3 ))
print(paste("Time Taken in execution of third function is :", stop -start ))
cat("\n")
cat("\n")


start <- Sys.time()
res_of_function_4<- name_of_function_4(x , k)
stop <- Sys.time()

print("Results of Fourth Function ")
cat("\n")
cat("\n")
df <- res_of_function_4[[1]]
print(df);cat("\n")
print(paste("Mean of x", res_of_function_4[[2]] ))
print(paste("Standard Deviation of x", res_of_function_4[[3]] ))
print(paste("Mean Calculated from DataFrame", res_of_function_4[[4]] ))
print(paste("Standard Devation Calculated from DataFrame",res_of_function_4[5] ))
print(paste("Mean Error",res_of_function_4[[6]],"\n"))
print(paste("Standard Deviation Error", res_of_function_4[[7]]))
print(paste("The time taken to compute Standard deviation by different formula is ", res_of_function_4[[8]] ))
print(paste("Time Taken in execution of fourth function is :", stop - start ))



