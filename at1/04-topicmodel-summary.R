## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Visualisations  - on lemma corpus

## Library

library(ggplot2)
rm(list = ls())  # cleanup env 
setwd("c:/mdsi/dam/at1")
## load mycorpus and get basic metadata

filename <- "./topicmodel-stem/topicmodel-stemlda-6-Topic1to2_2to3.csv"

t12Tot23 <- read.csv(filename, stringsAsFactors = F)


t12Tot23$S1toS2 <- t12Tot23$topic1ToTopic2 - t12Tot23$topic2ToTopic3
mean.n <- mean(t12Tot23$S1toS2)
ggplot(t12Tot23, aes(1, S1toS2)) +
  geom_boxplot(width=0.1) + geom_point(shape=21) + coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank()) 
