## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Improve Stop Words

## Library

library(tm)
rm(list = ls())  # cleanup env 

## load mycorpus and get basic metadata

setwd("c:/mdsi/dam/at1")
docs_folder <- "stem"
# docs <- Corpus(
#   DirSource(docs_folder, encoding = "UTF-8"), 
#   readerControl = list(reader=readPlain, language="en"))
docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))

mystopwords <- c("the", "can", "this", "one", "will")
write.table(mystopwords, file = "mystopwords.txt", row.names = F, col.names = F)
docs <- tm_map(docs, removeWords, mystopwords)

filenames <- meta(docs,"id")
num_docs <- length(filenames)
summary(docs)


## start by creating a DocumentTermMatrix

dtm <- DocumentTermMatrix(docs)

# note that there are 4183 terms in the 34 documents
# <<DocumentTermMatrix (documents: 34, terms: 4183)>>
# Non-/sparse entries: 16551/125671
# Sparsity           : 88%
# Maximal term length: 114
# Weighting          : term frequency (tf)


# inspecting the terms' frequencies to improve the stopwords list

termFreq <- colSums(as.matrix(dtm))
termFreq <- data.frame(term = names(termFreq), termFreq, stringsAsFactors = FALSE)

# half of the terms appear 2 times or less
summary(termFreq)

# create a sort order and look at a snapshot of most and least frequent terms
ord <- order(-termFreq[,2])
head(termFreq[ord,], 20)

# The following terms are appearing in the top 20: 
# the, can, this, one, will -- removed
# this list was compiled after a couple of iterations 

# it would be interesting to explore with inverst document frequency
# that is looking at document frequency: the number of documents 
# in the collection that contain a term
# https://nlp.stanford.edu/IR-book/html/htmledition/inverse-document-frequency-1.html


# explore the most frequent terms by document
mostFreqTerms <- findMostFreqTerms(dtm)

mostFreqTerms

