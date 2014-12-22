# Coursera - Practical Machine Learning
# Instructor: Dr. Jeff Leek
# Course Project
#
# Date: 2014/12/21

library(caret)
# library(rattle)

#
# Load and clean data
#
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
dim(training); dim(testing)

# Preliminary summary
str(training);
train <- training; test <- testing

# Remove new_window rows
train <- subset(train,new_window!="yes") 

# Remove non-sensor variables
train <- train[,8:160]
test <- test[,8:160]

# Functions for type conversion
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], asNumeric))
integersNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.integer)], asNumeric))

# Standardize variables as numeric
train[,-dim(train)[2]] <- factorsNumeric(train[,-dim(train)[2]])
train[,-dim(train)[2]] <- integersNumeric(train[,-dim(train)[2]])

test[,-dim(test)[2]] <- factorsNumeric(test[,-dim(test)[2]])
test[,-dim(test)[2]] <- integersNumeric(test[,-dim(test)[2]])

# Remove columns that have no variability
homogeneous = apply(train, 2, function(var) length(unique(var)) == 1)
train <- train[,!homogeneous]
homogeneous = apply(test, 2, function(var) length(unique(var)) == 1)
test <- test[,!homogeneous]
str(train); str(test) # Check variables available now
dim(train); dim(test)

# Partition out validation set
set.seed(4547)
split <- createDataPartition(y=train$classe,p=0.60,list=FALSE)
train_split <- train[split,]
validation_split <- train[-split,]
dim(train_split);dim(validation_split)

#
# Training
#
modelFit <- train(classe~.,method="rf",data=train_split,
	prox=TRUE,allowParallel=TRUE,
	trControl=trainControl(method="cv",number=2))

modelFit
modelFit$finalModel

# Test against the validation set
predictions <- predict(modelFit,validation_split)
confusionMatrix(predictions,validation_split$classe)

# Do final testing
answers <- predict(modelFit,test)

## Save predictions in submission files
pml_write_files = function(x) {
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

# End script