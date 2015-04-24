
######################Task 1

install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")
library("NLP")
library("openNLP")
library("openNLPmodels.en")
library("tm")
library("tau")
library("koRpus")

#data <- read.csv(file="Pre_data.csv", header = T, sep=",")



#Pre-processing for bag-of-words approach 
# read the data that cleaned in Python code
dataframe <- read.csv(file="cleaned_data.csv", header = T, sep=",")
dataframe [, "processed.title"] <- NA
dataframe [, "processed.text"] <- NA
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()


person_entity_annotator <- Maxent_Entity_Annotator(kind = "person")
location_entity_annotator <- Maxent_Entity_Annotator(kind = "location")
organization_entity_annotator <- Maxent_Entity_Annotator(kind = "organization")
date_entity_annotator <- Maxent_Entity_Annotator(kind = "date")
money_entity_annotator <- Maxent_Entity_Annotator(kind = "money")
percentage_entity_annotator <- Maxent_Entity_Annotator(kind = "percentage")

entities <- function(doc, kind) {
  x <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    x[a[k == kind]]
  } else {
    x[a[a$type == "entity"]]
  }
}

for (u in 2:3){
  for (f in 1:nrow(dataframe)){
    if (dataframe[f,ncol(dataframe)-u] != ""){   
      #step1: Takenize step
      txt <- as.String(dataframe[f,ncol(dataframe)-u])
      txt <- gsub("/", " ", txt)
      tokenize <- scan_tokenizer(txt)
      

      #Remove punctuation (note that sometimes punctuation can be a feature), remove links or replace with link pllaceholder
      remove <- removePunctuation(tokenize, preserve_intra_word_dashes = TRUE)
      
      #Run Part-Of-Speech tagger
      tagged.results <- treetag(remove, treetagger="manual", format="obj",
                                TT.tknz = FALSE , lang="en",
                                TT.options = list(path="./TreeTagger", preset="en"))
      
      
      #Perform lemmatisation and/or stemming
      lemmatisation <- tagged.results@TT.res$lemma
      unknown <- which(lemmatisation == "<unknown>")
      card <- which(lemmatisation == "@card@")
      lemmatisation[unknown] <- remove[unknown]
      lemmatisation[card] <- remove[card]
      


      #Remove stop words
      s <- paste(lemmatisation, collapse = " ")
      a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
      a3 <- annotate(s, pos_tag_annotator, a2)
      
      a3 <- subset(a3, type == "word")
      pos <- sapply(a3$features, '[[', "POS")
      rm_words <- c()
      rm_words <- c(rm_words, which(pos == "PRP" | pos == "IN" | pos == "TO" | pos == "MD" | pos == "DT" | pos == "CC" | pos == "WDT" | pos == "VBP" | pos == "PRP$"))
      
      if (length(rm_words) != 0){
        r <- lemmatisation[-rm_words]
      }else{
        r <- lemmatisation
      }
      
      #Named Entity recogniser
      n <- paste(r, collapse = " ")
      result <- as.String(n)
      annotations <- annotate(result, list(sent_token_annotator,word_token_annotator,person_entity_annotator,location_entity_annotator,
                                           organization_entity_annotator,date_entity_annotator,money_entity_annotator,percentage_entity_annotator))
      
      if (annotations$type[nrow(as.data.frame(annotations))] == "entity"){
        doc <- AnnotatedPlainTextDocument(result, annotations)
        ftr <- unique(annotations$features)
        length.ftr <- length(ftr)
        kinds <- c()
        start <- c()
        for (p in 1:length.ftr){
          start <- c(start, length(ftr[[p]]))
        }
        start.ftr <- which(start==0) + 1
        for (q in start.ftr:length.ftr){
          kinds <- c(kinds, as.character(ftr[[q]]))
        }
        replacement <- toupper(kinds)
        
        for (l in 1:length(kinds)){
          replace <- as.character(entities(doc, kind = kinds[l]))
          for (m in 1:length(replace)){
            result <- gsub(replace[m], replacement[l], result)  # replace between start and end e.g. substr
          }
        }      
      }
      result <- gsub("[[:digit:]]+","NUMBER",result)
      dataframe[f,ncol(dataframe)-u+2] <- result
    }
    print(f)
  }
}

#dataframe <- useful.dataframe 
dataframe [, "title.text"] <- NA
for(v in 1:nrow(dataframe)){
  tt <- as.String(c(as.String(dataframe[v,124]), as.String(dataframe[v,125])))
  dataframe[v,126] <- gsub(",", "", tt)
}
write.csv(dataframe, "task1_Results.csv", row.names = F)

#useful.dataframe <- dataframe[-which(dataframe$purpose == "not-used"),]
