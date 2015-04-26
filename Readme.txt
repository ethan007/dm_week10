1.run the file “pre_processing.py¡” in terminal(make sure the original data in the same folder)
command line:”python pre_processing.py¡”

2 Use R to running the R files.
2.1 for t1.R: install link:install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source") 

Make sure you have all these packages.If not, please install library:
library("NLP")
library("openNLP")
library("openNLPmodels.en")
library("tm")
library("tau")
library("koRpus")

2.2 for t2.R
Library requierd:
   library("NLP")
   library("tm")
   library("topicmodels")
   library("koRpus")


2.3 for t3.T
Library required:
   library("e1071")
   library("randomForest")



3.the clustering: weka.