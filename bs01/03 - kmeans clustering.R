# The steps relating to the creation of the  the dtm  are almost identical to 
# those the first part of the previous class exercise. However, we recommend you
# run through the creation of the dtm again for practice
#
#Some additional tips:
# 1. Read the documentation for kmeans clustering algorithms:
#    https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
#  2. There are many options for algorithms, we'll use the default (Hartigan Wong) method for kmeans. See 
#     http://stackoverflow.com/questions/20446053/k-means-lloyd-forgy-macqueen-hartigan-wong
#     for details of the different methods if you're interested
# 3. Code assumes you are already in the working directory set in the earlier exercise
#



library(tm)
docs <- Corpus(DirSource("./docs"))

#Check number of docs loaded
print(docs)
#inspect a particular document
writeLines(as.character(docs[[30]]))
#Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)
#Remove stopwords from standard stopword list 
docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)
#some clean up
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")
#inspect
writeLines(as.character(docs[[30]]))







###### 
#### 
##
#

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
  # <<DocumentTermMatrix (documents: 30, terms: 3724)>>
  #   Non-/sparse entries: 13791/97929
  # Sparsity           : 88%
  # Maximal term length: 26
  # Weighting          : term frequency (tf)



####
##
#


## start clustering specific code

##
##
#convert dtm to matrix (what format is the dtm stored in?)
m<-as.matrix(dtm)


#write as csv file
write.csv(m,file="dtmAsMatrix.csv")
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                     substring(rownames(m),
                               nchar(rownames(m))-12,nchar(rownames(m))-4))

##
##
#compute distance between document vectors
d <- dist(m)   # eculidian distance

#kmeans clustering
#kmeans - run with nstart=100 and k=2,3,5 to compare results with hclust
kfit <- kmeans(d, 2, nstart=100)

class(kfit)  # class: kmeans -- this is the Kmeans clusters

#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)



#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss


#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:29

# run kmeans 2:29 times with K == 2:29 to find the withiness for each choice of K
for (i in 2:29) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)

# plot the withinss of the K(2:29) to see if there is an elbow
plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
      

      ###  no obvious elbow, try a different clustering/distance calculation method 


#rerun using cosine distance  ## previous ran dist() function which was an eculadian distance measure 
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 2, nstart=100)

clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size    ### [1] 28  2, docs, K
#print clusters (members)
kfit$cluster

#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss


#### Elbow method on the cosine distance k-means cluster for K = 2:29

#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:29
for (i in 2:29) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)

plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



### i will choose K = 5

kfit5_cd <- kmeans(cd, centers=5, nstart=25)
kfit5_d <- kmeans(d, centers=5, nstart=25)

clusplot(as.matrix(cd), kfit5_cd$cluster, color=T, shade=T, labels=2, lines=0, main = "cosine distance K = 5")
clusplot(as.matrix(d), kfit5_d$cluster, color=T, shade=T, labels=2, lines=0, main = "eculadian distance K =5")
