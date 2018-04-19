install.packages("pacman")
pacman::p_load(textstem, dplyr)
setwd("c:/mdsi/dam/at1")  # set working directory



# stem vs lemma
dw <- c('driver', 'drive', 'drove', 'driven', 'drives', 'driving')
stem_words(dw)
lemmatize_words(dw)

bw <- c('are', 'am', 'being', 'been', 'be')
stem_words(bw)
lemmatize_words(bw)


str <- "the quick brown fox jumps over the lazy dog"

stem_strings(str)
lemmatize_strings(str)


# use it with tm library

library(tm)
docs_folder <- "docs"
docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))


lemmaTransform <- content_transformer(
  function(x){
    return (lemmatize_strings(x))
  }
)

docs <- tm_map(docs, lemmaTransform)
