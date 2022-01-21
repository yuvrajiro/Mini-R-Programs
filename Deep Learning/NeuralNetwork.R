#' ---
#' title: "Neural Network from Scratch in R"
#' author: "Rahul Goswami"
#' ---


#' Create a simple Neural Network for Classification Problem

NN.Classification <- function(X, Y, n_hidden = 10, n_iter = 100, learning_rate = 0.01, print_cost = FALSE)
{
  #' Initialize the parameters
  n_x <- dim(X)[2] # number of input units
  n_y <- 1 # number of output units
  parameters <- initialize_parameters(n_x, n_hidden, n_y)
  #' Loop (gradient descent)
  for(i in 1:n_iter)
  {
    #' Forward propagation:
    forward_propagate <- forward_propagation(X, parameters)
    A2 <- forward_propagate['A2']
    cache <- list(forward_propagate)
    #' Cost function
    cost <- compute_cost(A2, Y, parameters)
    #' Backward propagation
    grads <- backward_propagation(parameters, cache, X, Y)
    #' Update parameters
    parameters <- update_parameters(parameters, grads, learning_rate)
    #' Print the cost every 100 training example
    if(print_cost )
    {
      print(cost)
    }
  }
  return(parameters)
}

forward_propagation <- function(X, parameters)
{
  #' Initialize the cache
  cache <- list()
  #' Retrieve the parameters from the dictionary "parameters"
  W1 <- parameters["W1"]
  b1 <- parameters["b1"]
  W2 <- parameters["W2"]
  b2 <- parameters["b2"]
  #' Implement Forward Propagation to calculate A2 (probabilities)
  Z1 <- matrix(1, nrow = 1, ncol = W1[,1])
  for(i in 1:W1[,1])
  {
    Z1[1,i] <- W1[i,]*X[,i] + b1[i]
  }
  A1 <- sigmoid(Z1)
  Z2 <- matrix(1, nrow = 1, ncol = W2[,1])
  for(i in 1:W2[,1])
  {
    Z2[1,i] <- W2[i,]*A1 + b2[i]
  }
  A2 <- sigmoid(Z2)
  #' Save the computed values in the cache
  cache <- list(Z1 = Z1, A1 = A1, Z2 = Z2, A2 = A2)
  return(cache)
}

sigmoid <- function(z)
{
  return(1/(1+exp(-z)))
}

compute_cost <- function(A2, Y, parameters)
{
  #' Retrieve the parameters from the dictionary "parameters"
  W1 <- parameters["W1"]
  W2 <- parameters["W2"]
  #' Compute the cross-entropy cost
  m <- length(Y)
  logprobs <- log(A2)
  logprobs <- logprobs[,Y]
  cost <- -1/m * sum(logprobs)
  cost <- cost + (1/2*m)*(sum(W1^2) + sum(W2^2))
  return(cost)
}

backward_propagation <- function(parameters, cache, X, Y)
{
  #' Retrieve the parameters from the dictionary "parameters"
  W1 <- parameters["W1"]
  W2 <- parameters["W2"]
  #' Retrieve the values from the dictionary "cache"
  Z1 <- cache["Z1"]
  A1 <- cache["A1"]
  Z2 <- cache["Z2"]
  A2 <- cache["A2"]
  #' Initialize the gradients
  dZ2 <- matrix(1, nrow = 1, ncol = W2[,1])
  dW2 <- matrix(1, nrow = W2[,1], ncol = W2[,1])
  db2 <- matrix(1, nrow = 1, ncol = W2[,1])
  dZ1 <- matrix(1, nrow = 1, ncol = W1[,1])
  dW1 <- matrix(1, nrow = W1[,1], ncol = W1[,1])
  db1 <- matrix(1, nrow = 1, ncol = W1[,1])
  #' Compute the partial derivatives
  for(i in 1:W2[,1])
  {
    dZ2[1,i] <- A2[1,i] - Y[1,i]
    dW2[,i] <- dZ2[1,i]*A1
    db2[1,i] <- dZ2[1,i]
  }
  for(i in 1:W1[,1])
  {
    dZ1[1,i] <- W2*dZ2*W1[i,]
    dW1[,i] <- dZ1[1,i]*X[,i]
    db1[1,i] <- dZ1[1,i]
  }
  grads <- list(dW1 = dW1, dW2 = dW2, db1 = db1, db2 = db2)
  return(grads)
}

update_parameters <- function(parameters, grads, learning_rate)
{
  #' Retrieve the parameters from the dictionary "parameters"
  W1 <- parameters["W1"]
  W2 <- parameters["W2"]
  #' Retrieve the gradients from the dictionary "grads"
  dW1 <- grads["dW1"]
  dW2 <- grads["dW2"]
  db1 <- grads["db1"]
  db2 <- grads["db2"]
  #' Update the parameters
  W1 <- W1 - learning_rate*dW1
  W2 <- W2 - learning_rate*dW2
  b1 <- b1 - learning_rate*db1
  b2 <- b2 - learning_rate*db2
  parameters <- list(W1 = W1, W2 = W2, b1 = b1, b2 = b2)
  return(parameters)
}

#' Initialize the parameters normally distributed
initialize_parameters <- function(n_x, n_h, n_y)
{
  print(n_y)
  W1 <- matrix(sample(rnorm(50), size = n_h*n_x, replace = TRUE), nrow = n_h, ncol = n_x)
  b1 <- matrix(sample(rnorm(50), size = n_h*1, replace = TRUE), nrow = n_h, ncol = 1)
  W2 <- matrix(sample(rnorm(50), size = n_y*n_h, replace = TRUE), nrow = n_y, ncol = n_h)
  b2 <- matrix(sample(rnorm(50), size = n_y*1, replace = TRUE), nrow = n_y, ncol = 1)
  parameters <- list(W1 = W1, W2 = W2, b1 = b1, b2 = b2)
  return(parameters)
}

#' Predicting the results
predict <- function(parameters, X)
{
  #' Retrieve the parameters from the dictionary "parameters"
  W1 <- parameters["W1"]
  W2 <- parameters["W2"]
  b1 <- parameters["b1"]
  b2 <- parameters["b2"]
  #' Forward propagation
  Z1 <- matrix(1, nrow = 1, ncol = W1[,1])
  for(i in 1:W1[,1])
  {
    Z1[1,i] <- W1[i,]*X[,i] + b1[i]
  }
  A1 <- sigmoid(Z1)
  Z2 <- matrix(1, nrow = 1, ncol = W2[,1])
  for(i in 1:W2[,1])
  {
    Z2[1,i] <- W2[i,]*A1 + b2[i]
  }
  A2 <- sigmoid(Z2)
  #' Convert probabilities A2 to predictions y_pred
  y_pred <- matrix(1, nrow = 1, ncol = A2[,1])
  for(i in 1:A2[,1])
  {
    if(A2[1,i] >= 0.5)
    {
      y_pred[1,i] <- 1
    }
    else
    {
      y_pred[1,i] <- 0
    }
  }
  return(y_pred)
}


#' ### Import the iris dataset

data(iris)


labels = iris[,5] == "setosa"
features = iris[1:100,1:4]


NN.Classification(features, labels, learning_rate = 0.1,  print_cost = TRUE)





 




      



