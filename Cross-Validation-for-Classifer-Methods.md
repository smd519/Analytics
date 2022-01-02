Cross-Validation for Classifer Methods
================
Exploring credit card dataset

# Introduction

Here, we explore credit card dataset, and apply ksvm or kknn function to
find a good classifier. In part(a), we use cross-validation and in
part(b) we just split the data into training, validation, and test data
sets.

# Source of Data

The dataset is the “Credit Approval Data Set” from the UCI Machine
Learning Repository without the categorical variables and without data
points that have missing values. It has anonymized credit card
applications with a binary response variable (last column) indicating if
the application was positive or negative.

Link: <https://archive.ics.uci.edu/ml/datasets/Credit+Approval>

# Part(a-1): Cross-Validation for k-nearest-neighbors

## 1\. Read the dataset and load it as ‘data’.

``` r
data <- read.table(file = 'data/credit_card_data-headers.txt',
                   sep = "\t",
                   header=T,
                   na.strings=c("","NA"))
```

## 2\. Split the whole dataset into 2 distinct sets: data1 and data2

``` r
set.seed(1)
n.dataset <-nrow(data)
text.size <- floor(n.dataset*0.1) # Allocate 10% for data2
test.index <- sample(seq_len(n.dataset), size = text.size )

data.test <- data[test.index,] # To use it for measuring performance
data.train <- data[-test.index,] # To perform Cross-Validation on
```

## 3\. k-fold CV: Randomly split the data1 into ‘kcv’ groups

``` r
set.seed(1)
kcv = 10 # k in k-fold cross validation

# define a function to split the data
cv.folds <- function(n, nfolds){
  split(sample(n), rep(1:nfolds, length = n) )
}

# Split the data.train into 'kcv' groups
cv.folds.index <- cv.folds(nrow(data.train), kcv)
```

## 4\. Train kknn model using k-fold CV

``` r
library('kknn')
K.upper = 15 # We examine only K-kknn <= 15
model.kknn.accurracy <- matrix(0,nrow=K.upper,ncol = kcv)

for (Kvalue in 1:K.upper){ # Number of neighbors in Knn

  model.kknn.score.kcv <- rep(0,kcv)
  for (i in 1:kcv) { # For each k-fold group:
    
      # a. Take one group as validation.data set
      validation.index <- unlist( cv.folds.index[i] )
      validation.data <- data.train[validation.index,1:10] 
    
      # b. Take the remaining groups as a train.data set
      train.data <- data.train[-validation.index,1:10]
      response.data <- data.train[-validation.index,11]

      # c. Fit the model on the training set 
      model.knn <- kknn(response.data~., train.data,
                        validation.data,k = Kvalue, scale=TRUE)
      
      # d. Evaluate the model on the validation set
      # We can use cr-error (to minimize) or accuracy (to maximize)
      pred.kknn <- as.integer(fitted(model.knn) + 0.5)
      model.kknn.score.kcv[i] <- 
        sum(pred.kknn == data.train[validation.index,11])/ length(validation.index)
  }
  # e. save the evaluation score and discard the model
  model.kknn.accurracy[Kvalue,] <- model.kknn.score.kcv

}
# So far for each Kvalue, we did 'kcv times' training & validation 
print(model.kknn.accurracy)
```

    ##            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ##  [1,] 0.7627119 0.7796610 0.7118644 0.7457627 0.8474576 0.8813559 0.8135593
    ##  [2,] 0.7627119 0.7796610 0.7118644 0.7457627 0.8474576 0.8813559 0.8135593
    ##  [3,] 0.7627119 0.7796610 0.7118644 0.7457627 0.8474576 0.8813559 0.8135593
    ##  [4,] 0.7627119 0.7796610 0.7118644 0.7457627 0.8474576 0.8813559 0.8135593
    ##  [5,] 0.7627119 0.8474576 0.7796610 0.8644068 0.8813559 0.9152542 0.8135593
    ##  [6,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8644068 0.9152542 0.7796610
    ##  [7,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8474576 0.9152542 0.7796610
    ##  [8,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8474576 0.9152542 0.7966102
    ##  [9,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8305085 0.9152542 0.7966102
    ## [10,] 0.7288136 0.8474576 0.7966102 0.8983051 0.8474576 0.9152542 0.8135593
    ## [11,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8474576 0.9152542 0.8135593
    ## [12,] 0.7288136 0.8474576 0.7796610 0.8983051 0.8474576 0.9152542 0.8135593
    ## [13,] 0.7288136 0.8474576 0.7796610 0.8813559 0.8474576 0.9152542 0.8135593
    ## [14,] 0.7288136 0.8305085 0.7796610 0.8813559 0.8474576 0.8983051 0.8135593
    ## [15,] 0.7288136 0.8474576 0.7796610 0.8644068 0.8474576 0.8983051 0.8135593
    ##            [,8]      [,9]     [,10]
    ##  [1,] 0.8305085 0.8135593 0.8448276
    ##  [2,] 0.8305085 0.8135593 0.8448276
    ##  [3,] 0.8305085 0.8135593 0.8448276
    ##  [4,] 0.8305085 0.8135593 0.8448276
    ##  [5,] 0.8474576 0.8474576 0.8965517
    ##  [6,] 0.8474576 0.8644068 0.8965517
    ##  [7,] 0.8474576 0.8644068 0.8793103
    ##  [8,] 0.8474576 0.8474576 0.8965517
    ##  [9,] 0.8474576 0.8644068 0.8965517
    ## [10,] 0.8644068 0.8644068 0.8965517
    ## [11,] 0.8644068 0.8644068 0.8965517
    ## [12,] 0.8644068 0.8474576 0.8965517
    ## [13,] 0.8644068 0.8474576 0.9137931
    ## [14,] 0.8644068 0.8474576 0.9137931
    ## [15,] 0.8644068 0.8474576 0.9137931

## 5\. Pick the best model: I use ‘mean’ as the criteria for simplicity

``` r
# Summarize the skill scores: 
# There are many criteria to identify the best model. I use mean for simplicity 
model.kknn.score <- rowMeans(model.kknn.accurracy)

# Find the good classifier
Kvalue.chosen <- which.max(model.kknn.score)
cat ("best Kvalue is:", which.max(model.kknn.score), 
     "\nwith average CV.accuracy of:", max(model.kknn.score))
```

    ## best Kvalue is: 10 
    ## with average CV.accuracy of: 0.8472823

``` r
plot(model.kknn.score, main =" k-nearest neighbors",
     ylab = "average CV accuracy", xlab ="kvalue")
```

![](Cross-Validation-for-Classifer-Methods_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 6\. Train the chosen model on ALL of data1 to find model parameters

``` r
# Train the model on ALL of data1 which is data.train
model.knn <- kknn(data.train[,11]~., data.train[,1:10],
                  data.test,k = Kvalue.chosen, scale=TRUE)
```

## 7\. Report the picked model’s accuracy as its performance on data2

``` r
pred.kknn <- as.integer(fitted(model.knn) + 0.5)
model.kknn.performance <- sum(pred.kknn == data.test[,11])/ nrow(data.test)
cat("The picked model\n accuracy: ",model.kknn.performance, " kvalue: ",Kvalue.chosen)
```

    ## The picked model
    ##  accuracy:  0.8615385  kvalue:  10

# Part(a-2): Cross-Validation for ksvm

## 1\. Read the dataset and load it as ‘data’.

``` r
data <- read.table(file = 'data/credit_card_data-headers.txt',
                   sep = "\t",
                   header=T,
                   na.strings=c("","NA"))
```

## 2\. Split the whole dataset into 2 distinct sets: data1 and data2

``` r
set.seed(1)
n.dataset <-nrow(data)
text.size <- floor(n.dataset*0.1) # Allocate 10% for data2
test.index <- sample(seq_len(n.dataset), size = text.size )

data.test <- data[test.index,] # To use it for measuring performance
data.train <- data[-test.index,] # To perform Cross-Validation on
```

## 3\. k-fold CV: Train model on data1 using built-in k-fold option: ‘cross’, and explore different values of kernel

``` r
library(kernlab)
kernelNames = c("vanilla", "rbf", "polydot", "tanh", "bessel", "splinedot")
model.ksvm.error <- c()

for(kn in kernelNames) {

  # a. cross>0: k-fold cross validation on data2
  # data2 will be split into 'cross' groups
  # one group is taken for validation, the remaining groups for training
  model.kn <- ksvm(x = as.matrix(data.train[,1:10]),as.factor(data.train[,11]),
              type= "C-svc", kernel= kn, C=100, scaled=TRUE, cross=10)

  # b. cross>0: model validation on each k-fold validation set
  # average error across k-folds is returned
  model.ksvm.error[kn] <- cross(model.kn)
}
```

    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters

## 4\. Pick the best model: we use built-in return value

``` r
print(model.ksvm.error)
```

    ##   vanilla       rbf   polydot      tanh    bessel splinedot 
    ## 0.1357393 0.1934249 0.1358270 0.2275862 0.2291935 0.2445354

``` r
# Find the good classifier based on CV.Error
kernel.chosen <- which.min(model.ksvm.error)
kernel.chosen <- names(kernel.chosen)[1]
kernel.chosen
```

    ## [1] "vanilla"

## 5\. Train the chosen model on ALL of data1 to find model parameters

``` r
# Train the model on ALL of data1 without 'cross' option
model.ksvm <- ksvm(x = as.matrix(data.train[,1:10]),as.factor(data.train[,11]),
              type= "C-svc", kernel= kernel.chosen, C=100, scaled=TRUE)
```

    ##  Setting default kernel parameters

## 6\. Report the picked model’s accuracy as its performance on data2

``` r
pred <- predict(model.ksvm,data.test[,1:10])
model.ksvm.accuracy <- sum(pred == data.test[,11]) / nrow(data.test)
model.ksvm.error <- sum(pred != data.test[,11]) / nrow(data.test)
cat ("\nkernel.chosen:\t",as.name(kernel.chosen),
     "\nmodel.ksvm.accuracy:\t",model.ksvm.accuracy,
     "\nmodel.ksvm.error:\t",model.ksvm.error)
```

    ## 
    ## kernel.chosen:    vanilla 
    ## model.ksvm.accuracy:  0.8461538 
    ## model.ksvm.error:     0.1538462

# Part(b-1): Training, Validation, and Test for k-nearest-neighbors

Here, we just split the data into training, validation, and test data
sets.

## 1\. Read the dataset and load it as ‘data’.

``` r
data <- read.table(file = 'data/credit_card_data-headers.txt',
                   sep = "\t",
                   header=T,
                   na.strings=c("","NA"))
```

## 2\. Split the whole dataset into 3 distinct sets: data1, data2, data3

``` r
set.seed(1)
n.dataset <-nrow(data)

splitted.index <- sample(1:3,size = n.dataset, replace=TRUE, prob=c(0.70,0.15,0.15))
data.train <- data[ splitted.index == 1,] # For training (data1)
data.validation <- data[ splitted.index == 2,] # For validation (data2) 
data.test <- data[ splitted.index == 3,] # For testing (data3)
cat("data is divided into 3 groups:",
    "\ndata.train:",nrow(data.train),
    "\ndata.validation:",nrow(data.validation),
    "\ndata.test:",nrow(data.test))
```

    ## data is divided into 3 groups: 
    ## data.train: 459 
    ## data.validation: 100 
    ## data.test: 95

## 3\. Train all kknn models on data1 and validate on data2

``` r
library('kknn')
K.upper = 15 # We examine only K-kknn <= 15
model.kknn.accurracy <- rep(0,times = K.upper)

for (Kvalue in 1:K.upper){ # Number of neighbors in Knn
  # a. Fit the model on the training set 
  model.knn <- kknn(data.train[,11]~., data.train[,1:10],
                    data.validation, k = Kvalue, scale=TRUE)
  
  # b. Validate all models on data.validation (data2)
  pred.kknn <- as.integer(fitted(model.knn) + 0.5)
  model.kknn.accurracy[Kvalue] <- 
    sum(pred.kknn == data.validation[,11])/ nrow(data.validation)
}
# So far for each Kvalue, we did ''kcv times 'one' training & validation 
print(cbind(1:K.upper,model.kknn.accurracy))
```

    ##          model.kknn.accurracy
    ##  [1,]  1                 0.78
    ##  [2,]  2                 0.78
    ##  [3,]  3                 0.78
    ##  [4,]  4                 0.78
    ##  [5,]  5                 0.82
    ##  [6,]  6                 0.82
    ##  [7,]  7                 0.83
    ##  [8,]  8                 0.83
    ##  [9,]  9                 0.83
    ## [10,] 10                 0.83
    ## [11,] 11                 0.83
    ## [12,] 12                 0.82
    ## [13,] 13                 0.82
    ## [14,] 14                 0.82
    ## [15,] 15                 0.82

## 4\. Pick the best model

``` r
# Find the good classifier
Kvalue.chosen <- which.max(model.kknn.accurracy)
cat ("best Kvalue is:", Kvalue.chosen, 
     "with average CV.accuracy of:", model.kknn.accurracy[Kvalue.chosen])
```

    ## best Kvalue is: 7 with average CV.accuracy of: 0.83

``` r
plot(model.kknn.accurracy, main =" k-nearest neighbors",
     ylab = "average CV accuracy", xlab ="kvalue")
```

![](Cross-Validation-for-Classifer-Methods_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## 5\. Report the picked model’s accuracy as its performance on data3

``` r
# I have already discarded the model.
# So, I will train the chosen model again on data.train to find model parameters
# And measure performance on data.test (data3).

# a. Train the chosen model on 'data1' to find model parameters 
model.knn <- kknn(data.train[,11]~., data.train[,1:10],
                    data.test, k = Kvalue.chosen, scale=TRUE)

# b. Evaluate the model on the data3
pred.kknn <- as.integer(fitted(model.knn) + 0.5)
model.kknn.performance <- sum(pred.kknn == data.test[,11])/ nrow(data.test)
cat("The picked model\n accuracy: ",model.kknn.performance, " kvalue: ",Kvalue.chosen)
```

    ## The picked model
    ##  accuracy:  0.8105263  kvalue:  7

# Part(b-2): Training, Validation, and Test for ksvm

## 1\. Read the dataset and load it as ‘data’.

``` r
data <- read.table(file = 'data/credit_card_data-headers.txt',
                   sep = "\t",
                   header=T,
                   na.strings=c("","NA"))
```

## 2\. Split the whole dataset into 3 distinct sets: data1, data2, data3

``` r
set.seed(1)
n.dataset <-nrow(data)

splitted.index <- sample(1:3,size = n.dataset, replace=TRUE, prob=c(0.70,0.15,0.15))
data.train <- data[ splitted.index == 1,] # For training (data1)
data.validation <- data[ splitted.index == 2,] # For validation (data2) 
data.test <- data[ splitted.index == 3,] # For testing (data3)
cat("data is divided into 3 groups:",
    "\ndata.train:",nrow(data.train),
    "\ndata.validation:",nrow(data.validation),
    "\ndata.test:",nrow(data.test))
```

    ## data is divided into 3 groups: 
    ## data.train: 459 
    ## data.validation: 100 
    ## data.test: 95

## 3\. Train all ksvm models on data1 and validate on data2

``` r
library(kernlab)
kernelNames = c("vanilla", "rbf", "polydot", "tanh", "bessel", "splinedot")
model.ksvm.accurracy <- c()

for(kn in kernelNames) {
  
  # Train the model on data1
  model.kn <- ksvm(x = as.matrix(data.train[,1:10]),as.factor(data.train[,11]),
              type= "C-svc", kernel= kn, C=100, scaled=TRUE)
  
  # Validate the model on data2
  pred.kn <- predict(model.kn, data.validation[,1:10])
  model.ksvm.accurracy[kn] <- sum( pred.kn == data.validation[,11]) / nrow(data.validation)

}
```

    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters  
    ##  Setting default kernel parameters

``` r
data.frame(Kernel = kernelNames, Accuracy = model.ksvm.accurracy)
```

    ##              Kernel Accuracy
    ## vanilla     vanilla     0.85
    ## rbf             rbf     0.81
    ## polydot     polydot     0.85
    ## tanh           tanh     0.79
    ## bessel       bessel     0.80
    ## splinedot splinedot     0.80

## 4\. Pick the best model

``` r
# Find the good classifier
kernel.chosen <- which.max(model.ksvm.accurracy)
kernel.chosen <- names(kernel.chosen)[1]
kernel.chosen
```

    ## [1] "vanilla"

## 5\. Report the picked model’s accuracy as its performance on data3

``` r
# a. Train the chosen model on 'data1' to find model parameters 
model.ksvm <- ksvm(x = as.matrix(data.train[,1:10]),as.factor(data.train[,11]),
              type= "C-svc", kernel= kernel.chosen, C=100, scaled=TRUE)
```

    ##  Setting default kernel parameters

``` r
# Measure performance on data3
pred <- predict(model.ksvm, data.test[,1:10])
model.ksvm.accuracy <- sum(pred == data.test[,11]) / nrow(data.test)
model.ksvm.accuracy.error <- sum(pred != data.test[,11]) / nrow(data.test)

#Report performance on data3
cat ("\nkernel.chosen:\t",as.name(kernel.chosen),
     "\nmodel.ksvm.accuracy:\t",model.ksvm.accuracy,
     "\nmodel.ksvm.accuracy.error:\t",model.ksvm.accuracy.error)
```

    ## 
    ## kernel.chosen:    vanilla 
    ## model.ksvm.accuracy:  0.8105263 
    ## model.ksvm.accuracy.error:    0.1894737
