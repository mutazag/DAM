## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Cleansing

## Library

library(tm)
library(textstem)
rm(list = ls())  # cleanup env 

## load mycorpus and get basic metadata

setwd("c:/mdsi/dam/at1")
docs_folder <- "docs"
# docs <- Corpus(
#   DirSource(docs_folder, encoding = "UTF-8"), 
#   readerControl = list(reader=readPlain, language="en"))
docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))

filenames <- meta(docs,"id")
num_docs <- length(filenames)
summary(docs)

## prepare a content transformation function 
## to replace a pattern with space 
## this function will be useful to cleanup 
## odd characters that are not picked up by
## tm standard transformations (getTransformations)


# define toSpace function 
toSpace <- content_transformer(
  function(x, pattern){
    return (gsub(pattern, " ", x))
  }
)


# remove chars by utf code 

removeUtfChars <- content_transformer(
  function(x, utfList = c("\x96", "\x86", "\x85", "\x91")){
    utfList <- utf8::as_utf8(utfList)
    utfPattern <- paste0("[",paste(utfList, collapse = "|"),"]")
    return(gsub(utfPattern, " ", x))
  }
)

# define corpus export function to save and check 
# corpus after transformation/cleanup steps 
saveCorpToDisk <- function(c, folder, filenames){
  tmp_folder <- folder
  if (!file.exists(tmp_folder)) { dir.create(file.path(".", tmp_folder))}
  writeCorpus(c, path = tmp_folder, filenames = filenames)
}


## apply data cleansing 
# Remove punctuation 
#Transform to lower case
#Strip digits
#Remove stopwords from standard stopword list 
#Strip whitespace (cosmetic?)

docs <- tm_map(docs, removePunctuation)

# use content transformer with tolower


docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("en"))

#save after 
saveCorpToDisk(docs, "temp4", filenames)
docs <- tm_map(docs, removeUtfChars)
saveCorpToDisk(docs, "temp4", filenames)


# docs <- tm_map(docs, content_transformer(enc2utf8))
# docs <- tm_map(docs, toSpace, utf8::as_utf8("\x96"))
# docs <- tm_map(docs, toSpace, utf8::as_utf8("\x85"))
# docs <- tm_map(docs, toSpace, utf8::as_utf8("\x86"))
# docs <- tm_map(docs, removeWords, c(utf8::as_utf8("\x96"),
#                                         utf8::as_utf8("\x85"),
#                                         utf8::as_utf8("\x86")) )

# saveCorpToDisk(docs, "temp4", filenames)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

mystopwords <- unlist(read.table(file = "mystopwords.txt", header = F, stringsAsFactors = F))
docs <- tm_map(docs,removeWords, mystopwords)
saveCorpToDisk(docs, "temp4", filenames)

## other options for replacing unwanted char
# stringi::stri_replace_all_regex(mycorpus[[34]], "[^\x20-\x7E]", " ")
# mycorpus <- tm_map(mycorpus, toSpace, "[^\x20-\x7E]")
# included in this range: intToUtf8(0x20:0x7e)
# " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"


# a heavy-handed approach to remove anything that is not 
# a char or space, but it keeps punctuation and numbers 
# mycorpus <- tm_map(mycorpus, toSpace, "[^a-z,A-Z, ]")  


## steming document 
# save outcome to stem to use as a starting point for further
stemdocs <- tm_map(docs, stemDocument)
saveCorpToDisk(stemdocs, "stem", filenames)

## lemmatise document


lemmaTransform <- content_transformer(
  function(x){
    return (lemmatize_strings(x))
  }
)

lemmadocs <- tm_map(docs, lemmaTransform)
saveCorpToDisk(lemmadocs, "lemma", filenames)
