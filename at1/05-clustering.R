## 36106 - Data Algorithms and Meaning
## Assignment 1: Analysis and interpertation of unstructured data
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Clustering

## Library

library(tm)
rm(list = ls())  # cleanup env 
setwd("c:/mdsi/dam/at1")  # set working directory


docs_folder <-  "stem" 

docs <- Corpus(DirSource(docs_folder, encoding = "UTF-8"))


mydtm <- DocumentTermMatrix(docs,
                            control = list(wordLengths = c(4, 20),
                                           bounds = list(global = c(3, 29))))

mydtm_m <- as.matrix(mydtm)
# compute distance between document vectors this can be done using euclidean or
# using custom cosine distance after calculating the distance, run either
# heirarical or kmeans clustring


# prepare cosine similarity distance function 
cosineSimilarity <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

# the following code will calculate distances bsed on three methods: eculidean,
# manhattan, cosine similarity
euc_distance <- dist(mydtm_m, method = "euclidean")
man_distance <- dist(mydtm_m, method = "manhattan")
cos_distance <- 1-cosineSimilarity(mydtm_m)


# then fit clusters based on the three distance measures
h_clust_euc <- hclust(d = euc_distance, method = "ward.D")
h_clust_man <- hclust(d = man_distance, method = "ward.D")
h_clust_cos <- hclust(d = cos_distance, method = "ward.D")
h_clust_cos$dist.method <- "cos_method" # setting this attribute to use for file name generaetion


# plot a dendogram for each cluster, each with 4 cuts after trying different
# cuts 3:6 it was observed that deeper cuts (>4) create very narrow group
# members: a couple of groups with 1 member only, while higher cuts (<4) were
# creating very broad group membership. So I decided to use cut the tree at 4
plot(h_clust_euc, hang=-1)
rect.hclust(h_clust_euc, 4)


plot(h_clust_man, hang=-1)
rect.hclust(h_clust_man, 4)

plot(h_clust_cos, hang=-1)
rect.hclust(h_clust_man, 4)

# out of the three distance methods, manhattan and cosine similarty produced
# very specific groups with 1 member only. as a result i decided not to
# manhattan or cosine distance methods for this excercise and focus the effort
# on the results from eculadian distance method


# at this point, to gain further insights into the clusters, i will explore
# visualising top terms in each of the cluster groups word clouds

library(wordcloud)


wordCloudForCluster <- function(clust, k, dtm_m){
  print(paste("processing cluster: ", clust$dist.method, " k: ", k))
  # cut the cluster by k
  cut_c <- cutree(clust,k)
  cut_c <- data.frame(cluster = cut_c, filename = names(cut_c), stringsAsFactors = F)
  
  # save dendo gram to disk
  if (!file.exists(clust$dist.method)) { dir.create(file.path(".", clust$dist.method))}
  cluster_filename <- paste0(clust$dist.method,"/hclus-",clust$dist.method,"-k",k)
  cluster_dendogram_file <- paste0(cluster_filename,"-dendo.png")
  png(cluster_dendogram_file)
  plot(clust, hang=-1)
  rect.hclust(clust, 4)
  dev.off()
  # save cluster file to disk
  cluster_files <- paste0(cluster_filename,".csv")
  print(paste("Write cut ",k,"to file ", cluster_files))
  write.csv(x = cut_c, file = cluster_files, row.names = F)
  
  # foreach group in cut, subset the dtm for document members, calc term freq
  # and visualise the wordcloud
  for (i in 1:k){
    # i <- 1
    # prepare output file names for files in cluster and wordcloud image
    cluster_filename <- paste0(cluster_filename,"-g",i)
    cluster_wordcloud_file <- paste0(cluster_filename,"-wc.png")

    # group <- cut_c[cut_c$cluster == i,]
    group <- subset(cut_c, cluster == i)
    dtm_group <- dtm_m[group$filename,]
    # THE FOLLOWING CONDITIONAL CHECK IS NECESSARY TO HANDLE SINGLE DOC DTM GROUP
    freq_group <- do.call(function(dtm_group){ 
      if(class(dtm_group) == "numeric"){
        return(dtm_group)
      }
      else {
        return(colSums(dtm_group))
        }
      }, list(dtm_group))
    # order freq and pick top 50 terms
    freq_group <- data.frame(count = freq_group, term = names(freq_group), stringsAsFactors = F)
    
    # freq_group <- freq_group[order(freq_group$count, decreasing = T),  ]
    # freq_group <- freq_group[1:50,]
    

    print(paste("Write wordcloud", i,"to file"))
    set.seed(1234)  # ensure consistent look everytime code is run
    
    
    png(cluster_wordcloud_file)
    wordcloud(freq_group$term, 
              freq_group$count, 
              colors =  brewer.pal(8,"Dark2"),
              max.words = 15
    )
    dev.off()
    

    
  }
}

wordCloudForCluster(clust = h_clust_euc, k = 4, dtm_m = mydtm_m)
wordCloudForCluster(clust = h_clust_cos, k = 4, dtm_m = mydtm_m)
wordCloudForCluster(clust = h_clust_man, k = 4, dtm_m = mydtm_m)

# 
# h_clust_cos$dist.method
# h_clust_cos$clusters
# cut_c <- cutree(h_clust_cos, k =6)
# cut_c[cut_c == 6]
# cut_c <- data.frame(c = cut_c, n = names(cut_c))
# str(cut_c)
# cut_c$n <- as.character(cut_c$n)
# cut_c$n
# cut_c6 <- subset(cut_c, c == 6) # cluster 6
# cut_c6$n
# 
# mydtm[c("Doc34.txt","Doc32.txt"), ]
# mydtm[cut_c6$n,]
# 
# # on matrix
# mydtm_m[c("Doc34.txt","Doc32.txt"), ]
# mydtm_m[cut_c6$n,]
# nrow(mydtm_m[cut_c6$n,])
# nrow(cut_c6)
# freq_c6 <- colSums(mydtm_m[cut_c6$n,])
# freq_c6 <- data.frame(term=names(freq_c6), count=freq_c6)
# library(wordcloud)
# set.seed(1234)  # ensure consistent look everytime code is run
# freq_c6_ord <- freq_c6[order(freq_c6$count, decreasing = TRUE),]
# freq_c6_ord_100 <- freq_c6_ord[1:100,]
# wordcloud(freq_c6_ord_100$term, freq_c6_ord_100$count, colors =  brewer.pal(8,"Dark2"))
# ### just do top 100