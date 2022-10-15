library(EBImage)
library(pbapply)
library(reticulate)
library(seriation)
extract_feature <- function(file.dir,width,height){
  #file.dir <- 'permuted/split/train'
  images <- list.files(file.dir)
  label <- ifelse(grepl("dog",images)==T,1,0)
  
  feature.list <- pblapply(images,function(img.name){
    #img.name <- ' cat.0.jpg'
    img <- readImage(file.path(file.dir,img.name))
    img.resized <- EBImage::resize(img, w = width, h =height)
    img.matrix <- matrix(reticulate::array_reshape(img.resized,(width*height*3)),
                         nrow = width*height*3)#轉成一維向量12288*1250
    img.vector <- as.vector(t(img.matrix))
  })
  feature.matrix <- do.call(rbind, feature.list)
  list(t(feature.matrix), label)
}
train.data <- extract_feature('permuted/random/train', 64, 64)
myTrain.X <- train.data[[1]]
myTrain.y <- train.data[[2]]

test.data <- extract_feature('permuted/random/test', 64, 64)
myTest.X <- test.data[[1]]
myTest.y <- test.data[[2]]

#parameter initiation
myTrain.X <- scale(myTrain.X)
myTest.X <- scale(myTest.X)
num.iter <- 10000
learning.rate <- 0.01
initialize_with_zeros <- function(dim){
  W <- matrix(0, nrow = dim, ncol = 1)
  b <- 0
  list(W = W, b = b)
}

#activation function : sigmoid 
sigmoid <- function(x){
  1/(1+exp(-x))
}

#forward/backward propagation
propagate <- function(W, b, X, y){
  n <- ncol(X) 
  A <- sigmoid((t(W) %*% X)+b)
  cost <- (-1/n) * sum(y * log(A) + (1-y) * log(1 - A)) 
  dW <- (1/n) * (X %*% t(A-y))
  db <- (1/n) * rowSums(A-y)
  list(gradient = list(dW, db), cost = cost)
}

optimize <- function(W, b, X, y, num.iter, learning.rate, print.cost = FALSE){
  cost <- numeric(num.iter)#list
  for(i in 1:num.iter){
    # calculate gradient and cost
    grad.cost <- propagate(W, b, X, y)
    gradient <- grad.cost$gradient
    cost[i] <- grad.cost$cost
    # Retrieve the derivatives
    dW <- matrix(gradient[[1]])
    db <- gradient[[2]]
    # Update the parameters
    W <- W - learning.rate * dW
    b <- b - learning.rate * db
    # Print the cost every 100th iteration
    if ((print.cost == T) & (i%%1000 == 0)) {
      cat(sprintf("Cost after iteration %d: %06f\n", i, cost[i]))
    }
    params <- list(W=W, b=b)
    gradient <- list(dW=dW, db=db)
  }
  list(params = params, gradient = gradient, cost = cost)
}
# Activation vector A to predict 
# the probability of a dog/cat
pred <- function(W, b, X) {
  n <- ncol(X)
  Y.prediction <- numeric(n)
  A <- sigmoid((t(W) %*% X) + b)
  for (i in 1:n) { 
    if (A[1, i] > 0.5) {
      Y.prediction[i] <- 1
    } else{
      Y.prediction[i] <- 0
    } 
  }
  Y.prediction
}
#ANN
simple_model <- function(train.X, train.y, test.X, test.y,
                         num.iter, learning.rate, print.cost = FALSE){
  
  Wb <- initialize_with_zeros(nrow(train.X)) 
  W <- Wb$W
  b <- Wb$b
  optFn.output <- optimize(W, b, train.X, train.y,
                           num.iter, learning.rate, print.cost)
  W <- as.matrix(optFn.output$params$W)
  b <- optFn.output$params$b
  pred.train <- pred(W, b, train.X)
  pred.test <- pred(W, b, test.X)
  
  cat(sprintf("train accuracy: %#.2f \n", mean(pred.train == train.y) * 100))
  cat(sprintf("test accuracy: %#.2f \n", mean(pred.test == test.y) * 100))
  
  res <- list(cost = optFn.output$cost,
              pred.train = pred.train, pred.test = pred.test, W = W, b = b)
  res
}
#Run
SNN.model <- simple_model(myTrain.X, myTrain.y, myTest.X, myTest.y,
                          + num.iter, learning.rate, print.cost = T)

plot(1:length(SNN.model$cost), SNN.model$cost,type="l", xlab = "Iterations", ylab = "Cost")
legend(6000, 4.5, c("Learning rate = 0.01"))