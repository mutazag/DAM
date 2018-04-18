## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Stemming


## Library
library(tm)
rm(list = ls())  # cleanup env 


## load corpus and get basic metadata
mycorpus_folder <- "docs"
mycorpus <- Corpus(
  DirSource(mycorpus_folder, encoding = "UTF-8"), 
  readerControl = list(reader=readPlain, language="en"))

dictcorpus <- mycorpus
filenames <- meta(mycorpus,"id")
num_docs <- length(filenames)

# define corpus export function to save and check 
# corpus after transformation/cleanup steps 
saveCorpToDisk <- function(c, folder, filenames){
  tmp_folder <- folder
  if (!file.exists(tmp_folder)) { dir.create(file.path(".", tmp_folder))}
  writeCorpus(c, path = tmp_folder, filenames = filenames)
}


# stem
mycorpus <- tm_map(mycorpus, stemDocument)
saveCorpToDisk(mycorpus, "stem", filenames)

stemCompletion2 <- content_transformer(
  function(x, dictcorpus){
    x <- unlist(strsplit(as.character(x), " "))
    # Unexpectedly, stemCompletion completes an empty string to
    # a word in dictionary. Remove empty string to avoid above issue.
    x <- x[x != ""]
    x <- stemCompletion(x, dictionary=dictcorpus)
    x <- paste(x, sep="", collapse=" ")
    return(PlainTextDocument(stripWhitespace(x)))
  }
)
data("crude")
mycorpus <- tm_map(mycorpus, stemCompletion2, crude)
ss <- inspect(mycorpus[[24]])
stemCompletion(ss, dictionary = as.character(dictcorpus[[24]]))
tm_map(mycorpus, stemCompletion, crude)


x <- c("completed","complete","completion","teach","taught")
tm <- Corpus(VectorSource(x))
tm <- tm_map(tm, stemDocument)
inspect(tm)
dictCorpus <- tm
tm <- tm_map(tm, stemDocument)
tm <- tm_map(tm, stripWhitespace, mc.cores=cores)  

tm<-tm_map(tm, stemCompletion,dictionary=dictCorpus)
tm


