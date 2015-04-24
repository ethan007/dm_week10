################TASK 2 
library("NLP")
library("tm")


#=========================== tf-idf ===========================
dataframe <- read.csv(file="task1Results.csv", header= T, sep=",")
doc_vec <- VectorSource(dataframe[,ncol(dataframe)])
doc_corpus <- Corpus(doc_vec)
dtmatrix <- DocumentTermMatrix(doc_corpus)
dtmatrix2 <- removeSparseTerms(dtmatrix,sparse=0.95)

df_dtmatrix2 <- as.data.frame(inspect(dtmatrix2))
col_dtmatrix2 <- ncol(df_dtmatrix2)
row_dtmatrix2 <- nrow(df_dtmatrix2)
tf_idf <- matrix(0,row_dtmatrix2,col_dtmatrix2)

for (x in 1:col_dtmatrix2)
{
  for (y in 1:row_dtmatrix2)
  {
    f_dtmatrix2 <- df_dtmatrix2[y,x]
    maxf <- max(df_dtmatrix2[,x])
    tf <- f_dtmatrix2 / maxf
    d.f <- length(which(df_dtmatrix2[,x] != 0))
    log.idf <- row_dtmatrix2 / d.f
    idf <- log2(log.idf)
    tfidf <- tf * idf
    tf_idf[y,x] <- tfidf
  }
  print(x)
}

tf_idf <- as.data.frame(tf_idf)
colnames(tf_idf) <- colnames(df_dtmatrix2)

write.csv(tf_idf, "Task2_TFIDF.csv", row.names= F)

tf_idf <- read.csv(file="Task2_TFIDF.csv", header= T, sep=",")
ti <- c()
for (mm in 1:nrow(tf_idf)){
  ti <- c(ti,length(which(tf_idf[mm,] != 0)))
}
summary(ti)
per <- 0.5

for (row.ti in 1:nrow(tf_idf))
{
  no.replace <- ceiling(length(which((tf_idf[row.ti,] != 0))) * per)
  order <- order(tf_idf[row.ti,], decreasing= T)
  tf_idf[row.ti,] <- tf_idf[row.ti,] * 0
  for (ss in 1:no.replace)
  {
    tf_idf[row.ti,order[ss]] = 1
  }
  print(row.ti)
}

write.csv(tf_idf, "Task2_BinaryTFIDF.csv", row.names= F)

binary_tfidf <- read.csv(file="Task2_BinaryTFIDF.csv", header= T, sep=",")
sum.col.ti <- apply(binary_tfidf, 2, sum)
binary_tfidf <- binary_tfidf[,-which(sum.col.ti <= median(sum.col.ti))]
write.csv(binary_tfidf, "Task2_SelectedFeatures.csv", row.names = F)


#=========================== LDA ===========================
dataframe <- read.csv(file="task1Results.csv", header= T, sep=",")
doc_vec <- VectorSource(dataframe[,ncol(dataframe)])
doc_corpus <- Corpus(doc_vec)
dtmatrix <- DocumentTermMatrix(doc_corpus)
lda <- LDA(dtmatrix, control = list(alpha = 0.1), k = 10, method = "VEM")
terms(lda,10)
topics(lda)

term <- terms(lda,10)
lda_terms <- c(term[,1], term[,2],term[,3], term[,4],term[,5], term[,6],term[,7], term[,8], term[,9], term[,10])
unique.lda_terms <- unique(lda_terms)
topics <- topics(lda)
lengthoftopics <- length(topics(lda))
colname <- unique.lda_terms
features <- as.data.frame(matrix(NA, lengthoftopics, length(colname)))
colnames(features) <- colname

for (l in 1:lengthoftopics)
{
  where.is.feature <- match(term[,topics[l]], unique.lda_terms)
  features[l, ] <- 0
  features[l, where.is.feature] <- 1
  print(l)
}

write.csv(features, "Task2_LDA.csv", row.names= F)


#========================= Combined LAD fd-idf ===========================
features <- read.csv(file="Task2_LDA.csv", header= T, sep=",")
selected_features <- read.csv(file="Task2_SelectedFeatures.csv", header= T, sep=",")

colnames(features)
colnames(selected_features)

m <- match(colnames(selected_features), colnames(features))
repeated <- which(!is.na(m))
cols <- m[!is.na(m)]

combined_features <- cbind(features, selected_features[,-repeated])

for (r in 1:length(cols)){
  for (s in 1:nrow(features)){
    if (features[s,cols[r]] || selected_features[s,repeated[r]]){
    combined_features[s,cols[r]] <- 1
    }
  }
  print(r)
}

write.csv(combined_features, "Task2_LDA&SelectedFeatures.csv", row.names= F)