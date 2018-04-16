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
# inspect(docs[[1]])
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



# use content transformer with tolower
docs <- tm_map(docs, content_transformer(tolower))

# remove numbers 
docs <- tm_map(docs, removeNumbers)


# remove english stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# cosmetic - remove whitespaces
docs <- tm_map(docs, stripWhitespace)


# Stemming 
# is the process of reducing to their
# common root, which in this case would be the word offer 

docs <- tm_map(docs, stemDocument)  # uses tm::stemDocument

## dealig with some issues after stemming
docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")


##
##  Creat the document term matrix 
## 

dtm <- DocumentTermMatrix(docs)

# note the sparsity parameter
dtm 

# inspect 
inspect(dtm[1:5, 1200:1205])

# find freq of terms 
freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq, decreasing = TRUE)
freq[head(ord)]
# order freq

# terms appearing > 200 times
freq_200 <- freq[freq >200]
freq_200[order(freq_200, decreasing = TRUE)]
hist(names(freq_200), freq_200)
  names(freq_200)
  
  
# new DTM with control list 
ctrl <- list(
    wordLengths = c(4,20), 
    bounds = list(global = c(3,27))
)
dtmr <- DocumentTermMatrix(docs, control = ctrl)
dtmr_m <- as.matrix(dtmr)
freqr <- colSums(as.matrix(dtmr))
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]   ## nothing that appears less than 3 times
findFreqTerms(dtmr,lowfreq=230)  # terms that appear atleast 230 times



## term association 
findAssocs(dtmr, "organ", 0.5)
