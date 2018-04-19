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

num_docs <- length(docs)

mydtm <- DocumentTermMatrix(docs, 
                          control=list(
                            wordLengths=c(4, 20),
                            bounds = list(global = c(3,29))
                          )
)

# use filenames as a row name in dtm
rownames(mydtm) <- meta(docs,"id")
termFreq <- colSums(as.matrix(mydtm))
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


topicModelingLDA <- function(dtm, k, ctrl){
  
  filenames <- rownames(dtm)
  ldaout <- LDA(dtm, 
                k, 
                method = "Gibbs",
                control = ctrl
  )
  
  # get the document to topic mapping
  ldaout.topics <- as.matrix(topics(ldaout))
  ldaout.topics <- data.frame(filename = filenames, topic = ldaout.topics)
  
  # top 6 terms in each topic
  ldaout.terms <- as.matrix(terms(ldaout,6))
  ldaout.terms <- data.frame(ldaout.terms)
  
  # topic assignment probablities 
  ldaout.topicProbabilities <- as.data.frame(ldaout@gamma)
  ldaout.topicProbabilities <- data.frame(filename = filenames, ldaout.topicProbabilities )
  rownames(ldaout.topicProbabilities) <- filenames
  
  
  # Calculating relative importance of topics in a document
  # sort topic probabilities and then compare p(k) element (highest)
  # and p(k-1) element (second highest), and then second highest
  # with the third highest p(k-2)
  # p(k)/p(k-1) and p(k-1)/p(k-2)
  
  
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
  
  
  
  ## save outputs to files
  
  write.csv(ldaout.topics, file=paste0("lda-",k,"-DocsToTopics.csv"), row.names = F)
  write.csv(ldaout.terms, file=paste0("lda-",k,"-TopicsToTerms.csv"))
  write.csv(ldaout.topicProbabilities, file=paste0("lda-",k,"-TopicProbablities.csv"), row.names = F)
  write.csv(topic2topic, file=paste0("lda-",k,"-Topic1to2_2to3.csv"))
}


## call the topic modeling function and examine outputs 
## run the topic modeling for fours values of k <- 4:8
lapply(4:8, function(x) { topicModelingLDA(dtm=mydtm, k=x, ctrl=ctrl)})
# topicModelingLDA(dtm = mydtm, k = k, ctrl = ctrl)


# some intuitive meaning is found when k = 6
topic2term_6 <- read.csv("lda-6-topicstoterms.csv")
topic2term_6


# X  Topic.1 Topic.2 Topic.3 Topic.4  Topic.5  Topic.6
# 1 1 document practic organis project knowledg    model
# 2 2     word    best    work    risk     issu     data
# 3 3  cluster problem   manag   manag question techniqu
# 4 4    topic   paper   chang process     idea     mani
# 5 5     term  reason   organ  social    figur     base
# 6 6   corpus possibl  system  involv     said function

# based on topic/term matrix, topics labels could be: 
# topic 1: term clustering 
# topic 2: best practice discussion
# topic 3: orgnisation change magement
# topic 4: project/risk management
# topic 5: knowledge and issues! 
# topic 6: data modeling techniques

# next, will run some clustring on the content and try to 
# determine an optimal number of clusters (WSS technique)
# to see what would it indicate as optimal cluster K value
