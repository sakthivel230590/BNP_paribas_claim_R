############################################################################################################
# Standard Library 
# Kaggle Competition: https://www.kaggle.com/c/bnp-paribas-cardif-claims-management
# Authors: Sakthivel Sabapathy
############################################################################################################

#Faster reading
library(data.table)
library(xgboost)
library(mRMRe)
library(caret)

setwd("/Users/Sakthivel/Desktop/Work2/BNBParibas/")

setResponseVariable = function (target) {
  responseVariable <<- target 
}

getResponseVariable = function () {
  return (responseVariable)
}

getHighlyCorelatedNumericalColmns = function (df, maxPossibleCor=0.9,verb=F) {
  nums <- sapply(df, is.numeric)
  numerical_columns <- df[,nums]
  corelations<-cor(numerical_columns)
  hc<-findCorrelation(corelations,cutoff=maxPossibleCor,verbose=verb)
  return (hc)
  #sample usage
  # Removing Highly corelated Numerical Columns
  #hcColms = getHighlyCorelatedNumericalColmns(train_test_merged_data,.90)
  #if (!is.na(hcColms)) {
  #  cat("There are ", length(hcColms), "highly correlated columns", "and the columns are...\n", hcColms)
  #  train_test_merged_data <- train_test_merged_data[,-c(hcColms)]
  #}
}

na.roughfix2 <- function (object, value) {
  res <- lapply(object, roughfix,value)
  structure(res, class = "data.frame", row.names = seq_len(nrow(object)))
}

roughfix <- function(x,value) {
  missing <- is.na(x)
  if (!any(missing)) return(x)
  
  if (is.numeric(x)) {
    # If the NA's follows a pattern ie not random, its better not to impute
    if (value == "median") {
      x[missing] <- median.default(x[!missing])
    } else {
      x[missing] <- value
    }
  } else if (is.factor(x)) {
    freq <- table(x)
    x[missing] <- names(freq)[which.max(freq)]
  } else {
    stop("na.roughfix only works for numeric or factor")
  }
  x
}

getBestFeatures = function (df, noOfFeaturesReq) {
  ind <- sapply(df, is.integer)
  df[ind] <- lapply(df[ind], as.numeric)
  dd <- mRMR.data(data = df)
  
  feats <- mRMR.classic(data = dd, target_indices = c(ncol(df)), feature_count = noOfFeaturesReq)
  bestVars <-data.frame('features'=names(df)[solutions(feats)[[1]]], 'scores'= scores(feats)[[1]])
  print(bestVars)
  return (bestVars[,1])
}

convertCharFactsToInt = function (df) {
  cat("assuming text variables are categorical & replacing them with numeric ids\n")
  cat("re-factor categorical vars & replacing them with numeric ids\n")
  
  featureNamesSet <- names(df)
  for (eachFeature in featureNamesSet) {
    if (class(df[[eachFeature]])=="character" || class(df[[eachFeature]])=="factor") {
      df[[eachFeature]] <- as.integer(factor(df[[eachFeature]]))
    }
  }
  return (df)
}

##Hexavigesimal decoding
az_to_int <- function(az) {
  xx <- strsplit(tolower(az), "")[[1]]
  pos <- match(xx, letters[(1:26)]) 
  result <- sum( pos* 26^rev(seq_along(xx)-1))
  return(result)
}

setXgTest = function (df) {
  xgtest <<- xgb.DMatrix(as.matrix(df))
  
}

setXgTrain = function (df) {
  xgtrain <<- xgb.DMatrix(as.matrix(df), label = responseVariable)
}

# Do cross-validation with xgboost - xgb.cv
docv <- function(param0, iter) {
  model_cv = xgb.cv(
    params = param0
    , nrounds = iter
    , nfold = 10
    , data = xgtrain
    , early.stop.round = 50
    , maximize = FALSE
    , nthread = 3
  )
  gc()
  best <- min(model_cv$test.logloss.mean)
  bestIter <- which(model_cv$test.logloss.mean==best)
  
  cat("\n",best, bestIter,"\n")
  print(model_cv[bestIter])
  
  bestIter-1
}


doTest <- function(param0, iter) {
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 20
    , nthread = 4
  )
  p <- predict(model, xgtest)
  rm(model)
  gc()
  p
}

KSTest = function(train,test,cutOff=0.007) {
  #Feature selection using KS test with 0.007 as cutoff.
  ksMat = NULL
  for (j in 1:ncol(test)) {
    cat(j," ")
    ksMat = rbind(ksMat, cbind(j, ks.test(train[,j],test[,j])$statistic))
  }
  
  ksMat2 = ksMat[ksMat[,2]<cutOff,]
  return (feats <- as.numeric(ksMat2[,1])) 
}