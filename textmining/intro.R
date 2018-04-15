## https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

# set working directory
setwd("c:/mdsi/dam/textmining")

# load text mining library
library(tm)

# load corpus
docs <- Corpus(DirSource("text"))

# list of files in corpus 
summary(docs)

# inspect a single document
inspect(docs[[1]])
writeLines(as.character(docs[[1]]))


# tm transformation functions 
getTransformations()

# define toSpace function 
toSpace <- content_transformer(
  function(x, pattern){
    return (gsub(pattern, " ", x))
  }
)

# use tm_map to replace - and :
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")

# replace punctuations with space 
docs <- tm_map(docs, removePunctuation)

# remove non standard punctuations found in text 
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "`")  
docs <- tm_map(docs, toSpace, " -")
