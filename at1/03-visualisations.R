## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Visualisations

## Library

library(tm)
rm(list = ls())  # cleanup env 

## load mycorpus and get basic metadata

setwd("c:/mdsi/dam/at1")
docs_folder <- "stem"
docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))
filenames <- meta(docs,"id")
num_docs <- length(filenames)


# document term matrix is created with bounds on the word length and occurances
# word length between 4 and 20 to eliminate short words or very long ones
# as they maybe a result of issues in cleansing 
# bounds: keeping terms that occure is more than 3 documents (not very unique)
#         and words that don't appear in most documents < 29
#         words that appear in all document may not be very indicative of 
#         meaning
dtm <- DocumentTermMatrix(docs, 
                          control=list(
                            wordLengths=c(4, 20),
                            bounds = list(global = c(3,29))
                            )
                          )

# calculate term frequencies in preperation to plotting them
termFreq <- colSums(as.matrix(dtm))
termFreq <- data.frame(term=names(termFreq), count=termFreq)


## bar chart to show high freq terms 
library(ggplot2)
library(RColorBrewer)

# subset to get terms that appear > 100 times
freq_100 <- termFreq[termFreq$count>100,]
ord <- order(freq_100$count, decreasing = T)
# reorder freq_100 in descending order, good for plotting
freq_100$term <- factor(freq_100$term, levels = freq_100$term[ord])

ggplot( freq_100, aes(term, count)) + 
  geom_bar(stat="identity", aes(fill=count)) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Top Terms in Documents")


# bottom terms 25 to 50
freq_low50 <- termFreq[termFreq$count >= 25 & termFreq$count <= 50, ]
ord <- order(freq_low50$count, decreasing = F)
freq_low50$term <- factor(freq_low50$term, levels = freq_low50$term[ord])

ggplot( freq_low50, aes(term, count)) + 
  geom_bar(stat="identity", aes(fill=count)) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Terms in the middle of the scale")
## this turned out to be a bad idea as it is too crowded to show anything
## and besides this approach is not sustainable, a better approach is use
## some clustering techniques or topic modeling

# word count visualisations 
library(wordcloud)
set.seed(1234)  # ensure consistent look everytime code is run
wordcloud(freq_100$term, freq_100$count, colors =  brewer.pal(8,"Dark2"))
  