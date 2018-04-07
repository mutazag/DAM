# The steps relating to the creation of the  the dtm  are almost identical to 
# those the first part of the previous class exercise. However, we recommend you
# run through the creation of the dtm again for practice
#
# Some additional tips:
# 0. DON'T PANIC if if you don't get it all at first. To begin with you can
#    skip the explanations, run the code and experiment with  different parameter
#    settings and come back to the explanations later. 
#    For a comprehensive intro to topic modeling using R check out: 
#    https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
# 1. The algorithm behind topic modelling, Latent Dirichlet Allocation (LDA), is technically
#    quite complex. A good intuitive explanation of the LDA can be found at:
#    http://blog.echen.me/2011/08/22/introduction-to-latent-dirichlet-allocation/
#    and
#    http://www.matthewjockers.net/2011/09/29/the-lda-buffet-is-now-open-or-latent-dirichlet-allocation-for-english-majors/
# 2. For tech details and options pertaining to the lda algorithm in R, check out
#    the following vignette on CRAN:
#    https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
# 3. Don't forget to modify the directory path in the setwd() command before starting!
#


#load tm library
library(tm)

## prepare file names 
filenames <- list.files("./docs",pattern="*.txt")
filenames <- lapply(filenames, function(f) { return(paste0("./docs/", f))})

#read files into a character vector
files <- lapply(filenames,readLines)


docs <- Corpus(VectorSource(files))


#Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)
#Remove stopwords from standard stopword list 
docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)
#some clean up
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")




#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- filenames





######
#####
###
##
#


#Topic models
library(topicmodels)

#Run LDA using Gibbs Sampling
# Gibbs Sampling is a "Markov Chain Monte Carlo" algorithm that is often used to
# to approximate a probability distribution. See: https://en.wikipedia.org/wiki/Gibbs_sampling
# It works by performing a random walk in such a way that reflects the 
# characteristics of a desired distribution.
#
# The burn-in period is used to ensure that we start from a representative point. There
# is some controversy about the need to use burn-in periods. See: 
# https://www.johndcook.com/blog/2011/08/10/markov-chains-dont-converge/ for example
# We'll ignore the controversy and set...
burnin <- 1000
# and perform 2000 iterations (after burn-in)...
iter <- 2000
#..taking every 500th one for further use. This "thinning" is done to ensure that
# samples are not correlated.
thin <- 500
#We'll use 5 different, randomly chosen starting points
nstart <- 5
#using random integers as seed. Feel free to change these
seed <- list(2003,5,63,100001,765)
#...and take the best run (the one with the highest probability) as the result
best <- TRUE



#Number of topics (try different numbers from, say 4 to 8 and see which one returns
# the best results)
k <- 6
#Patience, this WILL take a while....
#................
#.............
#..........
#......
#....
#..
ldaOut <- LDA(dtm,k, method="Gibbs", control=
              list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

class(ldaOut)   ## "topicmodels"

topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

terms(ldaOut,6)
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))



#How good (i.e. distinct) are the topic assignments?
#Find relative importance of top 2 topic assignments
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
#Find relative importance of second and third ranked topic assignments
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
