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
## that replaces a pattern with space 
## this function will be useful to cleanup 
## odd characters that are not picked up 
## tm standard transformations (getTransformations)


# define corpus export function 
saveCorpToDisk <- function(corpus, folder, filenames){
  tmp_folder <- folder
  if (!file.exists(tmp_folder)) { dir.create(file.path(".", tmp_folder))}
  writeCorpus(corpus, path = tmp_folder, filenames = filenames)
}

# define toSpace function 
toSpace <- content_transformer(
  function(x, pattern){
    return (gsub(pattern, " ", x))
  }
)


## apply data cleansing 
# Remove punctuation 
#Transform to lower case
#Strip digits
#Remove stopwords from standard stopword list 
#Strip whitespace (cosmetic?)

# corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

cc <- "[\x96|\x85]"
# corpus <- tm_map(corpus, removeWords, c("\x96", "\x85"))

utflist <- c(utf8::as_utf8("\x96"),
             utf8::as_utf8("\x85"),
             utf8::as_utf8("\x86")) 
ppUtf <- paste0("[",paste(utflist, collapse = "|"),"]")

# save before 
saveCorpToDisk(corpus, "temp2", filenames)
# corpus <- tm_map(corpus, content_transformer(gsub), pattern = ppUtf, replacement = " ") # works
corpus <- tm_map(corpus, toSpace, ppUtf)
#save after 
saveCorpToDisk(corpus, "temp2", filenames)


corpus <- tm_map(corpus, content_transformer(enc2utf8))
corpus <- tm_map(corpus, removeWords, c(utf8::as_utf8("\x96"),
                                        utf8::as_utf8("\x85"),
                                        utf8::as_utf8("\x86")) )

saveCorpToDisk(corpus, "temp2", filenames)

## replacing other encoded char
# stringi::stri_replace_all_regex(corpus[[34]], "[^\x20-\x7E]", " ")
# corpus <- tm_map(corpus, toSpace, "[^\x20-\x7E]")
# included in this range: intToUtf8(0x20:0x7e)
# " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"


# a heavy-handed approach to remove anything that is not 
# a char or space, but it keeps punctuation and numbers 
# corpus <- tm_map(corpus, toSpace, "[^a-z,A-Z, ]")  



# export corpus to disk and inspect using bash and regex
# for any non-word characters that still exist
tmp_folder <- "tmp"
if (!file.exists(tmp_folder)) { dir.create(file.path(".", tmp_folder))}
writeCorpus(corpus, path = tmp_folder, filenames = filenames)

#Stem document

