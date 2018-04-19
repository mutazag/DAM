## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Intro

# The source code is contained in the following files: 

# 01-cleanring.R: contains the code for loading an cleaning the dataset, in
# addition to tm library stem function, this code also includes a lemmatisation
# transform. outputs from stem and lemmatisations were saved to file and used in
# later stages for comparison

# 02-improveStopWords.R: includes the code used to examine term frequencies and
# produce custom stop words. custom stop words list was then introduced in the
# cleaning step and cleaning was run again

# 03-visualisations-stem.R: The file plots bar and wordclouds of terms and frequencies

# 04-topicmodel-stem.R: the file implements wrapper for the LDA topic modelling
# function, this wrapper is then called to produce topic model files for k =
# 4:8, the outpus of the process are saved to disk. outputs include csv files
# for topics/documents, topic/terms, topic probablities and topic1to2 and 2to3
# comparison

# 04-topicmodel-summary.R: produces a measure of the difference between
# topic1to2 and 2to3, which is used later to produce a radar chart in excel

# 05-clustering.R: evaulates hclustering based on three distance functions:
# eculadian, manhattan and cosine similarity. the source file also implements a
# wrapper function that produces dendogram for diffferent k settings and
# wordcloud for each cluster found for a given cut