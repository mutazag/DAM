## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383


## Library

library(tm)
rm(list = ls())  # cleanup env 

## load corpus and get basic metadata

corpus_folder <- "docs"
corpus <- Corpus(
  DirSource(corpus_folder, encoding = "UTF-8"), 
  readerControl = list(reader=readPlain, language="en"))

filenames <- meta(corpus,"id")
num_docs <- length(filenames)


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
  function(x, utfList = c("\x96", "\x86", "\x86", "\x91")){
    utfList <- utf8::as_utf8(utfList)
    utfPattern <- paste0("[",paste(utfList, collapse = "|"),"]")
    return(gsub(utfPattern, " ", x))
  }
)

# define corpus export function to save and check 
# corpus after transformation/cleanup steps 
saveCorpToDisk <- function(corpus, folder, filenames){
  tmp_folder <- folder
  if (!file.exists(tmp_folder)) { dir.create(file.path(".", tmp_folder))}
  writeCorpus(corpus, path = tmp_folder, filenames = filenames)
}


## apply data cleansing 
# Remove punctuation 
#Transform to lower case
#Strip digits
#Remove stopwords from standard stopword list 
#Strip whitespace (cosmetic?)
saveCorpToDisk(corpus, "temp4", filenames)
corpus <- tm_map(corpus, removePunctuation)
saveCorpToDisk(corpus, "temp4", filenames)
corpus <- tm_map(corpus, content_transformer(tolower))
saveCorpToDisk(corpus, "temp4", filenames)
corpus <- tm_map(corpus, removeNumbers)
saveCorpToDisk(corpus, "temp4", filenames)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
saveCorpToDisk(corpus, "temp4", filenames)


# corpus <- tm_map(corpus, content_transformer(gsub), pattern = ppUtf, replacement = " ") # works
# corpus <- tm_map(corpus, toSpace, ppUtf)
#save after 
corpus <- tm_map(corpus, removeUtfChars)
saveCorpToDisk(corpus, "temp4", filenames)


corpus <- tm_map(corpus, content_transformer(enc2utf8))
corpus <- tm_map(corpus, removeWords, c(utf8::as_utf8("\x96"),
                                        utf8::as_utf8("\x85"),
                                        utf8::as_utf8("\x86")) )

saveCorpToDisk(corpus, "temp4", filenames)
corpus <- tm_map(corpus, stripWhitespace)


### note: some tansformers need to be run more than once 
### e.g tolower did not transform entire doc to lower the first time 

corpus <- tm_map(corpus, toSpace, utf8::as_utf8("\x85"))
# save before 
saveCorpToDisk(corpus, "temp4", filenames)
## replacing other encoded char
# stringi::stri_replace_all_regex(corpus[[34]], "[^\x20-\x7E]", " ")
# corpus <- tm_map(corpus, toSpace, "[^\x20-\x7E]")
# included in this range: intToUtf8(0x20:0x7e)
# " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"


# a heavy-handed approach to remove anything that is not 
# a char or space, but it keeps punctuation and numbers 
# corpus <- tm_map(corpus, toSpace, "[^a-z,A-Z, ]")  



