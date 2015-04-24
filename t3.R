library("e1071")
library("randomForest")


# Unfold
task1_data <- read.csv(file="task1Results.csv", header = T, sep=",")
task2_data <- read.csv(file="task2_LDA&SelectedFeatures.csv", header = T, sep=",")


name_col <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
                 "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
no_col <- c()

for (numCol in 1:length(name_col))
{
  no_col <- c(no_col, which(colnames(task1_data) == name_col[numCol]))
}

data<-cbind(task1_data[,c(3,no_col)], task2_data)
data<-data[-which(apply(data[,2:11], 1, sum) == 0),] 
nrow(data) # 9042!!!!!!!!!!!!

ExtendData <- data
ExtendData[,"class"] <- NA
for (j in 1:(nrow(ExtendData)))
{
  for (i in 2:11)
  { 
    if (ExtendData[j,i] == 1){
      n_row <- ExtendData[j,]
      n_row[2:11] <- 0
      n_row[i] <- 1
      n_row[ncol(ExtendData)] <- name_col[i-1]
      ExtendData <- rbind(ExtendData, n_row)
    
    }
  }
  print(j)
}

Ext <- ExtendData[-(1:rows),]
nrow(Ext) # 9990!!!!!!!!!!!
Ext <- Ext[,-c(2:11)]
write.csv(Ext, "Task3.csv", row.names= F)



########################################################## classifier
rownames <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", 
                 "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
colnames <- c("TP", "FN", "FP", "Accuracy", "Recall", "Precision", "F-measure")

task3.df <- read.csv(file="Task3.csv", header = T, sep=",")
train <- task3.df[which(task3.df$purpose == "train"),-1]
test <- task3.df[which(task3.df$purpose == "test"),-1]

Mic_mac <- matrix(NA, 1, 5)
colnames(Mic_mac) <- c("Macro Recall ", "Macro Precision ", "Micro Recall ", "Micro Precision ", "Overall Accuracy")

rF_result <- matrix(NA, 10, 7, dimnames = list(rownames, colnames))
nB_result <- matrix(NA, 10, 7, dimnames = list(rownames, colnames))
svm_result <- matrix(NA, 10, 7, dimnames = list(rownames, colnames))

rF_micromacro <- Mic_mac
nB_micromacro <- Mic_mac
svm_micromacro <- Mic_mac

numofclass <- length(rownames)
# RandomForest
rF_model <- randomForest(class ~ ., data = train)
rF_predictresult <- predict(rF_model, newdata = train[,-ncol(train)])
rF_table <- table(rF_predictresult, train$class)
rF_table

tn <- 0
for(i in 1:nrow(rF_table))
{
  tp <- rF_table[i,i]
  fn <- sum(rF_table[i,], na.rm = TRUE) - rF_table[i,i]
  fp <- sum(rF_table[,i], na.rm = TRUE) - rF_table[i,i]
  accuracy <- (tp + tn) / nrow(train)
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  fmeasure <- 2 * precision * recall / (precision + recall)
  rF_result[i,1] <- tp
  rF_result[i,2] <- fn
  rF_result[i,3] <- fp
  rF_result[i,4] <- accuracy
  rF_result[i,5] <- recall
  rF_result[i,6] <- precision 
  rF_result[i,7] <- fmeasure      
}


rF_micromacro[1] <- sum(rF_result[,5], na.rm = TRUE)/numofclass
rF_micromacro[2] <- sum(rF_result[,6], na.rm = TRUE)/numofclass
rF_micromacro[3] <- sum(rF_result[,1], na.rm = TRUE)/(sum(rF_result[,1], na.rm = TRUE)+sum(rF_result[,2], na.rm = TRUE))
rF_micromacro[4] <- sum(rF_result[,1], na.rm = TRUE)/(sum(rF_result[,1], na.rm = TRUE)+sum(rF_result[,3], na.rm = TRUE))
rF_micromacro[5] <- sum(rF_result[,4])

rF_result
rF_micromacro

#naiveBayes
nB_model <- naiveBayes(class ~ ., data = train)
nB_predictresult <- predict(nB_model, newdata = train[,-ncol(train)])
nB_table <- table(nB_predictresult, train$class)
nB_table

tn <- 0

for(j in 1:nrow(nB_table))
{
  tp <- nB_table[j,j]
  fn <- sum(nB_table[j,], na.rm = TRUE) - nB_table[j,j]
  fp <- sum(nB_table[,j], na.rm = TRUE) - nB_table[j,j]
  accuracy <- (tp + tn) / nrow(train)
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  fmeasure <- 2 * precision * recall / (precision + recall)
  nB_result[j,1] <- tp
  nB_result[j,2] <- fn
  nB_result[j,3] <- fp
  nB_result[j,4] <- accuracy
  nB_result[j,5] <- recall
  nB_result[j,6] <- precision 
  nB_result[j,7] <- fmeasure      
}

nB_micromacro[1] <- sum(nB_result[,5], na.rm = TRUE)/numofclass
nB_micromacro[2] <- sum(nB_result[,6], na.rm = TRUE)/numofclass
nB_micromacro[3] <- sum(nB_result[,1], na.rm = TRUE)/(sum(nB_result[,1], na.rm = TRUE)+sum(nB_result[,2], na.rm = TRUE))
nB_micromacro[4] <- sum(nB_result[,1], na.rm = TRUE)/(sum(nB_result[,1], na.rm = TRUE)+sum(nB_result[,3], na.rm = TRUE))
nB_micromacro[5] <- sum(nB_result[,4])

nB_result
nB_micromacro

#SVM
svm_model <- svm(class ~ ., data = train, scale = F, kernel = "linear")
svm_predictresult <- predict(svm_model, newdata = train[,-ncol(train)])
svm_table <- table(svm_predictresult, train$class)
svm_table

tn <- 0

for(k in 1:nrow(svm_table))
{
  tp <- svm_table[k,k]
  fn <- sum(svm_table[k,], na.rm = TRUE) - svm_table[k,k]
  fp <- sum(svm_table[,k], na.rm = TRUE) - svm_table[k,k]
  accuracy <- (tp + tn) / nrow(train)
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  fmeasure <- 2 * precision * recall / (precision + recall)
  svm_result[k,1] <- tp
  svm_result[k,2] <- fn
  svm_result[k,3] <- fp
  svm_result[k,4] <- accuracy
  svm_result[k,5] <- recall
  svm_result[k,6] <- precision 
  svm_result[k,7] <- fmeasure      
}

svm_micromacro[1]<-sum(svm_result[,5], na.rm=TRUE)/numofclass
svm_micromacro[2]<-sum(svm_result[,6], na.rm=TRUE)/numofclass
svm_micromacro[3]<-sum(svm_result[,1], na.rm=TRUE)/(sum(svm_result[,1], na.rm = TRUE) + sum(svm_result[,2], na.rm = TRUE))
svm_micromacro[4]<-sum(svm_result[,1], na.rm=TRUE)/(sum(svm_result[,1], na.rm = TRUE) + sum(svm_result[,3], na.rm = TRUE))
svm_micromacro[5]<-sum(svm_result[,4])

svm_result
svm_micromacro



# RandomForest
rF_model <- randomForest(class ~ ., data = train)
rF_predictresult <- predict(rF_model, newdata = test[,-ncol(test)])
rF_table <- table(rF_predictresult, test$class)
rF_table

tn <- 0
for(i in 1:nrow(rF_table))
{
  tp <- rF_table[i,i]
  fn <- sum(rF_table[i,], na.rm = TRUE) - rF_table[i,i]
  fp <- sum(rF_table[,i], na.rm = TRUE) - rF_table[i,i]
  accuracy <- (tp + tn) / nrow(test)
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  fmeasure <- 2 * precision * recall / (precision + recall)
  rF_result[i,1] <- tp
  rF_result[i,2] <- fn
  rF_result[i,3] <- fp
  rF_result[i,4] <- accuracy
  rF_result[i,5] <- recall
  rF_result[i,6] <- precision 
  rF_result[i,7] <- fmeasure      
}


rF_micromacro[1] <- sum(rF_result[,5], na.rm = TRUE)/numofclass
rF_micromacro[2] <- sum(rF_result[,6], na.rm = TRUE)/numofclass
rF_micromacro[3] <- sum(rF_result[,1], na.rm = TRUE)/(sum(rF_result[,1], na.rm = TRUE)+sum(rF_result[,2], na.rm = TRUE))
rF_micromacro[4] <- sum(rF_result[,1], na.rm = TRUE)/(sum(rF_result[,1], na.rm = TRUE)+sum(rF_result[,3], na.rm = TRUE))
rF_micromacro[5] <- sum(rF_result[,4])

rF_result
rF_micromacro










df <- read.csv(file="task3.csv", header = T, sep=",")
df <- df[,-1]

k_fold <- function(df, numfold, classifier){
  class <- df[,ncol(df)]
  numofclass <- length(unique(class))
  numdata <- nrow(df) / numfold
  random <- sample(nrow(df), nrow(df), replace = FALSE)
  rowsname <- c("topic.earn", "topic.acq", "topic.money.fx", "topic.grain", "topic.crude", "topic.trade", "topic.interest", "topic.ship", "topic.wheat", "topic.corn")
  colsname = c("TP", "FN", "FP", "Accuracy", "Recall", "Precision", "F-measure", "Macro Recall", "Macro Precision", "Micro Recall", "Micro Precision")
  result <- matrix(NA, 10, 11, dimnames = list(rowsname, colsname))
  rtable <- matrix(NA, 10, 11, dimnames = list(rowsname, colsname))
  final <- matrix(0, 10, 11, dimnames = list(rowsname, colsname))
  resultList <- list(rtable)
  
  fold_length <- nrow(df)/10
  sample_row <- sample(nrow(df))
  df <- df[sample_row, ]
  
  TP <- 0
  FN <- 0
  FP <- 0
  macro_precision <- 0
  macro_recall <- 0
  micro_precision <- 0
  micro_recall <- 0
  confusion_matrix <- 0
  
  for(i in 1:numfold){
    print(i)
    fold1 <- df[(fold_length*(i-1)+1):(fold_length*i),]
    fold9 <- df[-as.numeric(row.names(fold1)),]
    
    switch(classifier,
           randomForest = {
             rF_model <- randomForest(class ~., data = fold9, nodesize = 5) 
             predict <- predict(rF_model, fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           },
           naiveBayes = {
             nB_model <- naiveBayes(class ~., data = fold9) 
             predict <- predict(nB_model, newdata = fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           },
           SVM.linear = {
             svml_model <- svm(class ~., data = fold9, kernel = "linear", scale = F)
             predict <- predict(svm_model, fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           },
           SVM.polynomial = {
             svmp_model <- svm(class ~., data = fold9, kernel = "polynomial", scale = F)
             predict <- predict(svmp_model, fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           },
           SVM.Gaussian = {
             svmg_model <- svm(class ~., data = fold9, scale = F) # 0.7405405
             predict <- predict(svmg_model, fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           },
           SVM.sigmoid = {
             svms_model <- svm(class ~., data = fold9, kernel = "sigmoid", scale = F) # 0.7365365
             predict <- predict(svms_model, fold1[,-ncol(fold1)])
             table <- table(predict, fold1$class)
           }
    )
    
    tn <- 0
    for(j in 1:numofclass)
    {
      tp <- table[j,j]
      fn <- sum(table[j,], na.rm = TRUE) - table[j,j]
      fp <- sum(table[,j], na.rm = TRUE) - table[j,j]
      accuracy <- (tp + tn) / numdata
      recall <- tp / (tp + fn)
      precision <- tp / (tp + fp)
      fmeasure <- 2 * precision * recall / (precision + recall)
      rtable[j,1] <- tp
      rtable[j,2] <- fn
      rtable[j,3] <- fp
      rtable[j,4] <- accuracy
      rtable[j,5] <- recall
      rtable[j,6] <- precision 
      rtable[j,7] <- fmeasure      
    }
    macro_recall <- sum(rtable[,5], na.rm = TRUE) / numofclass
    macro_precision <- sum(rtable[,6], na.rm = TRUE) / numofclass
    micro_recall <- sum(rtable[,1], na.rm = TRUE) / (sum(rtable[,1], na.rm = TRUE) + sum(rtable[,2], na.rm = TRUE))
    micro_precision <- sum(rtable[,1], na.rm = TRUE) / (sum(rtable[,1], na.rm = TRUE) + sum(rtable[,3], na.rm = TRUE))
    rtable[1,8] <- macro_recall
    rtable[1,9] <- macro_precision
    rtable[1,10] <- micro_recall
    rtable[1,11] <- micro_precision
    resultList[[i]] <- rtable
    final <- final + resultList[[i]]    
    confusion_matrix <- confusion_matrix + table
  }
  
  for(k in 1:numofclass){
    TP <- final[k,1]
    TN <- 0
    FN <- final[k,2]
    FP <- final[k,3] 
    final[k,4] <- (TP + TN) / nrow(df)
    final[k,5] <- TP / (TP + FN)
    final[k,6] <- TP / (TP + FP) 
    final[k,7] <- 2 * TP / (TP + FP) * TP / (TP + FN) / (TP / (TP + FP) + TP / (TP + FN))
  }  
  overall_macrecall <- sum(final[,5], na.rm = TRUE)/numofclass
  overall_macprecision <- sum(final[,6], na.rm = TRUE)/numofclass
  overall_micrecall <- sum(final[,1], na.rm = TRUE) / (sum(final[,1], na.rm = TRUE) + sum(final[,2], na.rm = TRUE))
  overall_micprecision <- sum(final[,1], na.rm = TRUE) / (sum(final[,1], na.rm = TRUE) + sum(final[,3], na.rm = TRUE))
  final[1,8] <- overall_macrecall
  final[1,9] <- overall_macprecision
  final[1,10] <- overall_micrecall
  final[1,11] <- overall_micprecision
  total_accuracy <- sum(final[,4], na.rm = TRUE)
  
  print(resultList)
  print(final)
  print(confusion_matrix)
  print(total_accuracy)
  return(final)
}
randomForest <- k_fold(df, 10, "randomForest")
naiveBayes <- k_fold(df, 10, "naiveBayes")
svm_liner <- k_fold(df, 10, "SVM.linear")
svm_poly <- k_fold(df, 10, "SVM.polynomial")
svm_gau <- k_fold(df, 10, "SVM.Gaussian")
svm_sig <- k_fold(df, 10, "SVM.sigmoid")

