## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Topic Modeling

## Library

library(tm)
rm(list = ls())  # cleanup env 

## load mycorpus and get basic metadata

setwd("c:/mdsi/dam/at1")
docs_folder <- "stem"
docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))
filenames <- meta(docs,"id")
num_docs <- length(filenames)

dtm <- DocumentTermMatrix(docs, 
                          control=list(
                            wordLengths=c(4, 20),
                            bounds = list(global = c(3,29))
                          )
)

# use filenames as a row name in dtm
rownames(dtm) <- filenames
termFreq <- colSums(as.matrix(dtm))
termFreq <- data.frame(term=names(termFreq), count=termFreq)
ord <- order(termFreq$count, decreasing = T)
termFreq <- termFreq[ord,]
# save term freq to file 
write.csv(termFreq, file = "termFreq.csv", row.names = F)

#######
#######
#######



## Topic modelig is used to classify documents into topics/themes, a useful
## technique in exploring the content of the documents or a subset of the docs

library(topicmodels)

# Set params for Gibbs sampling
burnin <- 4000
iterations <- 2000
thin <- 500
seed <- list(123,999,2342, 1112,7)
nstart <- 5
best <- TRUE

ctrl <-  list(nstart = nstart, 
              seed = seed, 
              best = best,
              burnin = burnin, 
              iter = iterations, 
              thin = thin)

# number of topics k
k <- 5

ldaout <- LDA(dtm, 
              k, 
              method = "Gibbs",
              control = ctrl
              )


# get the document to topic mapping
ldaout.topics <- as.matrix(topics(ldaout))
ldaout.topics <- data.frame(filename = rownames(ldaout.topics), topic = ldaout.topics)

# top 6 terms in each topic
ldaout.terms <- as.matrix(terms(ldaout,6))

# topic assignment probablities 
ldaout.topicProbabilities <- as.data.frame(ldaout@gamma)
rownames(ldaout.topicProbabilities) <- filenames



# Calculating relative importance of topics in a document
# sort topic probabilities and then compare p(k) element (highest)
# and p(k-1) element (second highest), and then second highest
# with the third highest p(k-2)
# p(k)/p(k-1) and p(k-1)/p(k-2)

sort(ldaout.topicProbabilities[1,])[k]
sort(ldaout.topicProbabilities[1,])[k-1]
sort(ldaout.topicProbabilities[1,])[k-2]

topic1ToTopic2 <- lapply(1:nrow(ldaout.topicProbabilities), function(i, tp, k){
      return (sort(tp[i,])[k]/sort(tp[i,])[k-1])
    }, 
    ldaout.topicProbabilities,
    k)
topic2ToTopic3 <- lapply(1:nrow(ldaout.topicProbabilities), function(i, tp, k){
    return (sort(tp[i,])[k-1]/sort(tp[i,])[k-2])
    }, 
    ldaout.topicProbabilities,
    k)

topic2topic <- data.frame(filename = filenames, 
                          topic1ToTopic2 = unlist(topic1ToTopic2), 
                          topic2ToTopic3 = unlist(topic2ToTopic3))

