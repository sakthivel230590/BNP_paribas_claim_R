####################################################################################
#
# Kaggle Competition: https://www.kaggle.com/c/bnp-paribas-cardif-claims-management
# Sponsor : BNBParibas 
# Authors: Sakthivel Sabapathy
#
####################################################################################

# removing the variables from the environment
rm(list = ls())

# Sourcing the standard library
setwd("/Users/Sakthivel/BNBParibas/")
source("Kaggle_stdlib.R")

# Start the clock!
start_time <- t1 <- Sys.time()

# Setting a seed for reproducibility
set.seed(2016)

# Read the training data set
cat("reading the train data.........\n")
train_raw <- fread("train.csv", stringsAsFactors=TRUE) 

# Read the test data set
cat("reading the test data\n")
test_raw <- fread("test.csv", stringsAsFactors=TRUE) 

# Summary of the train data set
summary(train_raw)
# Printing the dimensions of the train data set
print(dim(train_raw))
# Printing the classes of the train data set
print(sapply(train_raw, class))
# Getting the number of rows of in test data set
nrowsOfTrainSet <- nrow(train_raw)

# Summary of the test data set
summary(test_raw)
# Printing the dimensions of the test data set
print(dim(test_raw))
# Printing the classes of the test data set
print(sapply(test_raw, class))
# Getting the number of rows of in test data set
nrowsOfTestSet <- nrow(test_raw)

# Setting the response variable column name
setResponseVariable(train_raw$target)

#################################################################
# Variable Adjustments for Merging Training and Testing Data sets
#################################################################
# Nullifying the target column in training set. Because, the testing set wont be having the target column 
train_raw$target <- NULL

# Nullifying the train_raw ids
train_raw$ID <- NULL

# Nullifying the test_raw ids
test_raw$ID <- NULL

# Fill the missing values
# Right not, its a rough fix mode
# Set 1 : v1,v2,v4,v6,v7,v9,v11	v13	v15	v16	v17	v18	v19	v20	v23	v26	v27	v28	v29	v32	v33	v35	v37	v39	v41	v42	v43	v44	v45	v48	v49	v51	v53	v55	v57	v58	v59	v60	v61	v64	v65	v67	v68	v69	v73	v76	v77	v78	v80	v83	v84	v85	v86	v88	v90	v92	v93	v94	v95	v96	v97	v99	v100	v101	v102	v103	v104	v106	v111	v115	v116	v118	v119	v120	v121	v122	v123	v126	v127	v130	v131
# Set 2: v114	v110	v129	v125	v107	v112	v52	v91	v74	v75	v71	v72	v79	v66	v50	v62	v47	v38	v34	v40	v56	v3	v14	v24	v21	v22	v31	v12	v10	v113	v30
# Set 3: v5	v8	v25	v36	v46	v54	v63	v70	v81	v82	v87	v89	v98	v105	v108	v109	v128	v124	v117

train_raw1 <- subset(train_raw, select = c(v1,v2,v4,v6,v7,v9,v11,v13,v15,v16,v17,v18,v19,v20,v23,v26,v27,v28,v29,v32,v33,v35,v37,v39,v41,v42,v43,v44,v45,v48,v49,v51,v53,v55,v57,v58,v59,v60,v61,v64,v65,v67,v68,v69,v73,v76,v77,v78,v80,v83,v84,v85,v86,v88,v90,v92,v93,v94,v95,v96,v97,v99,v100,v101,v102,v103,v104,v106,v111,v115,v116,v118,v119,v120,v121,v122,v123,v126,v127,v130,v131))
train_raw1 <- na.roughfix2(train_raw1,-9999)
train_raw2 <- subset(train_raw, select = c(v114,v110,v129,v125,v107,v112,v52,v91,v74,v75,v71,v72,v79,v66,v50,v62,v47,v38,v34,v40,v56,v3,v14,v24,v21,v22,v31,v12,v10,v113,v30))
train_raw2 <- na.roughfix2(train_raw2,"median")
train_raw3 <- subset(train_raw, select = c(v5,v8,v25,v36,v46,v54,v63,v70,v81,v82,v87,v89,v98,v105,v108,v109,v128,v124,v117))
train_raw3 <- na.roughfix2(train_raw3,-1111)

test_raw1 <- subset(test_raw, select = c(v1,v2,v4,v6,v7,v9,v11,v13,v15,v16,v17,v18,v19,v20,v23,v26,v27,v28,v29,v32,v33,v35,v37,v39,v41,v42,v43,v44,v45,v48,v49,v51,v53,v55,v57,v58,v59,v60,v61,v64,v65,v67,v68,v69,v73,v76,v77,v78,v80,v83,v84,v85,v86,v88,v90,v92,v93,v94,v95,v96,v97,v99,v100,v101,v102,v103,v104,v106,v111,v115,v116,v118,v119,v120,v121,v122,v123,v126,v127,v130,v131))
test_raw1 <- na.roughfix2(test_raw1,-9999)
test_raw2 <- subset(test_raw, select = c(v114,v110,v129,v125,v107,v112,v52,v91,v74,v75,v71,v72,v79,v66,v50,v62,v47,v38,v34,v40,v56,v3,v14,v24,v21,v22,v31,v12,v10,v113,v30))
test_raw2 <- na.roughfix2(test_raw2,"median")
test_raw3 <- subset(test_raw, select = c(v5,v8,v25,v36,v46,v54,v63,v70,v81,v82,v87,v89,v98,v105,v108,v109,v128,v124,v117))
test_raw3 <- na.roughfix2(test_raw3,-1111)

train_raw <- cbind(train_raw1,train_raw2,train_raw3)
test_raw <- cbind(test_raw1,test_raw2,test_raw3)

########################################
# Merging Training and Testing Data sets
########################################
cat("Combining the rows of training and testing data sets\n")
train_test_merged_data <- rbind(train_raw,test_raw)

# Convert the merged 'train_test_merged_data' data table to data frame
train_test_merged_data <- as.data.frame(train_test_merged_data) 

##########################################
# Data Prepration: Pre-Feature Engineering
#########################################

# Convert V22 column data from AZ into integers
train_test_merged_data$v22<-sapply(train_test_merged_data$v22, az_to_int)

# Convert all the charectors and Factors into integers
train_test_merged_data <- convertCharFactsToInt(train_test_merged_data)

# Right now, not removing the highly corelated columns since regression is not being used


#############################
# Reverse Feature Engineering
#############################
#There is a function findLinearCombos() in the package caret, which uses QR decomposition to find linear combinations in a matrix:
# very Slight change
#lin.comb <- findLinearCombos(train_test_merged_data)
#lin.comb
#train_test_merged_data <- train_test_merged_data[, -lin.comb$remove]

######################
# Feature Engineering
######################
nColumns <- ncol(train_test_merged_data)
feature.names <- names(train_test_merged_data)

# Count NA percentage
train_test_merged_data$NACount_N <- rowSums(is.na(train_test_merged_data)) / nColumns  

# Zero Count
train_test_merged_data$ZeroCount <- rowSums(train_test_merged_data[,feature.names]== 0) / nColumns 

# Below Zero Count
train_test_merged_data$Below0Count <- rowSums(train_test_merged_data[,feature.names] < 0) / nColumns 

# Max
train_test_merged_data$max <- do.call(pmax, train_test_merged_data)

#Min
train_test_merged_data$min <- do.call(pmin, train_test_merged_data)

#Median
train_test_merged_data$median<-apply(FUN=median,X=train_test_merged_data,MARGIN=1)

#sd 
train_test_merged_data$sd<-apply(FUN=sd,X=train_test_merged_data,MARGIN=1)

#Sum Zero
train_test_merged_data$sumzero<-apply(FUN=function(x) sum(ifelse(x==0,1,0)),X=train_test_merged_data,MARGIN=1)

#Sum one
train_test_merged_data$sumone<-apply(FUN=function(x) sum(ifelse(x==1,1,0)),X=train_test_merged_data,MARGIN=1)


# 1) Bernoulli Naive Bayes for categorical variables, except for v22, helps with (non-ET) algorithms. 
# 3) Counting the number of NAs. 
# 4) Counting the number of 0 values. 
# 5) T-SNE helps a little (especially when stacking at level 1).
# 6) probably need to use one-hot encoding for your categorical 
# features (other than v22 of course) since their assigned ordinal numbers are 
# meaningless and should not be used in arithmetic operations.


##################################################################
# Split the merged train_test data into training and testing again
##################################################################
cat("Splitting the train_test_merged_data training and testing data sets\n")
train <- train_test_merged_data[1:nrowsOfTrainSet,]
test <- train_test_merged_data[(nrowsOfTrainSet+1):nrow(train_test_merged_data),] 

##########
# KS test
##########
# Finding the features which passes KS Test
# Not much Difference seen in KS Test. So not using now
#validFeatureAfter_KSTest = KSTest(train,test)
#train <- train[,validFeatureAfter_KSTest]
#test <- test[,validFeatureAfter_KSTest]

cat("Checking the dimensions of training and testing data sets\n")
print(dim(train))
print(dim(test))

#############################
# mRMR Best Festure selection
#############################
#Choosing the top n features.
#n <- round(.80*ncol(train))
#bestFeatures3 <- getBestFeatures(train, n)
# subsetting the train data set with only the best features
#train <- subset(train, select = bestFeatures)
# subsetting the test data set with only the best features
#test <- subset(test, select = bestFeatures)



#################
# Modeling: Start 
#################
print( difftime( Sys.time(), start_time, units = 'sec'))

# set up the training set in matrix format for xgboost
setXgTrain(train)
# set up the testing set in matrix format for xgboost
setXgTest(test)

param0 <- list(
  # some generic, non specific params
  "objective"  = "binary:logistic"
  , "booster"    = "gbtree"
  , "eval_metric" = "logloss"
  , "eta" = 0.01
  , "max_depth" = 12
  , "subsample" = 0.90
  , "colsample_bytree" = 0.90
  , "min_child_weight" = 1
  , "num_parallel_tree"= 1
)


print( difftime( Sys.time(), start_time, units = 'min'))
#cv <- docv(param0, 10000)
# Try 1: parameters, "eta" = 0.05, "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 1,"max_depth" = 10 
# train-logloss:0.318681+0.001683	test-logloss:0.461203+0.004961
# Try 2: parameters, "eta" = 0.01, "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 1,"max_depth" = 10 
# [1277]	train-logloss:0.303635+0.001104	test-logloss:0.458637+0.005116
# Try3: Parameters: "eta" = 0.01, "gamma" = "2", "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 1,"max_depth" = 10 
# [1361]	train-logloss:0.304083+0.001189	test-logloss:0.459116+0.005197
# Kaggle :  0.45152; without xclassTree - 0.45746
# Try4: "objective"  = "binary:logistic", "booster"    = "gbtree", "eval_metric" = "logloss", "eta" = 0.009, "gamma" = 2, "lambda" = 2, "alpha" = 1, "silent" = 1, "scale_pos_weight" = 1.0, "max_depth" = 10, "subsample" = 0.9, "colsample_bytree" = 0.9, "min_child_weight" = 1
#[1607]	train-logloss:0.310006+0.001003	test-logloss:0.459359+0.004950
# Kaggle : 0.45177, 
# Try6: Parameters: "eta" = 0.009, "gamma" = "2", "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 5,"max_depth" = 10,"silent" = "1","scale_pos_weight" = "1.0" 
# [1651]	train-logloss:0.317357+0.001049	test-logloss:0.459886+0.005091
# Kaggle: 0.45199,
# Try7: Parameters: "eta" = 0.009, "gamma" = "2", "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 5,"max_depth" = 11,"silent" = "1","scale_pos_weight" = "1.0" ,"num_parallel_tree"= 1
# [1651] Kaggle: 0.45058
# Try8: Parameters: "eta" = 0.009, "gamma" = "2", "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 5,"max_depth" = 12,"silent" = "1","scale_pos_weight" = "1.0" ,"num_parallel_tree"= 1
# [1651] Kaggle: 0.45042
# Try9: Parameters: "eta" = 0.009, "gamma" = "2", "subsample" = 0.9, "colsample_bytree" = 0.9,"min_child_weight" = 5,"max_depth" = 13,"silent" = "1","scale_pos_weight" = "1.0", "num_parallel_tree"= 1
# train-logloss:0.257417+0.001376	test-logloss:0.459482+0.005280
# [905] Kaggle: 0.45131

print( difftime( Sys.time(), start_time, units = 'min'))

cv <- 1651
#cv <- 905
# 10 fold - 1.11
#5 fold - 1.25
cv <- round(cv * 1.11)

cat("Calculated rounds:", cv, " Starting ensemble\n")

print( difftime( Sys.time(), start_time, units = 'sec'))

cat("Training a XGBoost classifier with cross-validation\n")

# Bagging of single xgboost for ensembling
# change to e.g. 1:10 to get quite good results
ensemble <- data.frame(output=rep(0, nrow(test)))

bagCount <- 2
for (i in 1:bagCount) {
  print(i)
  set.seed(i + 2015)
  p <- doTest(param0, cv)
  
  # use 20% to 30% more than the best iter rounds from your cross-fold number.
  # as you have another 20% or 25% training data now, which gives longer optimal training time
  #ensemble <- ensemble + p
  ensemble$output <- ensemble$output + as.numeric(p)
  ensemble<-cbind(ensemble,as.numeric(p))
}

#imp<-xgb.importance(model=clf1)
#imp$Feature<-as.integer(imp$Feature)+1
#sqldf("select * from imp limit 100")
#varimp<-imp$Feature[1:150]

# sample submission total analysis
submission <- read.csv("/Users/sakthivel/BNBParibas/sample_submission.csv")

cat("reading the xtress data  \n")
xtrees <- fread("extra_trees.csv", stringsAsFactors=TRUE) 
ensemble<-cbind(ensemble,xtrees$PredictedProb)

# Finalise prediction of the ensemble
cat("Making predictions\n")
submission$PredictedProb <- (((ensemble$output/i) + xtrees$PredictedProb)/2)
#submission$PredictedProb <- (ensemble$output/i)
# Prepare submission
write.csv(submission, "final_submission.csv", row.names=F, quote=F)

# Taking a back up of ensemble
write.csv(ensemble,paste("ensemble_",start_time,".csv"))

table(round(ensemble[,2]),round(ensemble[,3]))

# Stop the clock
#print(proc.time() - start_time)
print( difftime( Sys.time(), start_time, units = 'min'))


