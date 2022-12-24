##INSTALLING LIBRARIES
#install.packages("rtweet")
#install.packages("httpuv")
#install.packages("base64enc")
#install.packages("twitteR")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
library("rtweet")
library("httpuv")
library("base64enc")
library("twitteR")
library("tm")
library("SnowballC")
library("wordcloud")

##ACCESING TWITTER ACOUNT

app = "Social-Web-Analytics"
key = "Fl4I6S1Ya7MVDryQLh64Ot2V2"
secret = "pz8ilFCx8W65Sm9ci4xGQs1WIlGxLb7ytIWPFbvzxK0RESVvZK"
access_token = "1177448633639112706-5voNOoeDiRzmC9OhcILLQpongurFqH"
access_secret = "enshgeK4L8A6Zs29nwtgCAqxgR59ovAuK4rdamWwtRPny"

##AUTHENTICATION

twitter_token = create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)

tweets = search_tweets('scrowder', n = 2000, type = "recent", 
                       token = twitter_token, include_rts = FALSE)

##PRE-PROCESSING THE DATA

library("tm")
library("SnowballC")

corpus = Corpus(VectorSource(tweets$description))
corpus = tm_map(corpus, function(x) iconv(x, to='UTF8', sub='byte'))
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII', sub=' '))
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus = tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus = tm_map(corpus, removeNumbers) # remove numbers
corpus = tm_map(corpus, removePunctuation) # remove punctuation
corpus = tm_map(corpus, stripWhitespace) # remove whitespace
corpus = tm_map(corpus, tolower) # convert all to lowercase
corpus = tm_map(corpus, removeWords, stopwords()) # remove stopwords
corpus = tm_map(corpus, stemDocument) # convert all words to their stems
tweet.user.dtm = DocumentTermMatrix(corpus)
tweet.user.wdtm = weightTfIdf(tweet.user.dtm)
tweet.user.matrix = as.matrix(tweet.user.wdtm)
6
## remove empty tweets
empties = which(rowSums(abs(tweet.user.matrix)) == 0)
tweet.user.matrix = tweet.user.matrix[-empties,]

#Cosine distance works better than Euclidean in text data.
norm.user.matrix = diag(1/sqrt(rowSums(tweet.user.matrix ^2))) %*% tweet.user.matrix
## then create the distance matrix
D.u = dist(norm.user.matrix, method = "euclidean")^2/2
## perform MDS using 300 dimensions
mds.user.matrix = cmdscale(D.u, k=300)

n=15
SSW.u=rep(0,n)
for(i in 1:n) {
  K=kmeans(mds.user.matrix,i,nstart=15)
  #choose 10 best inital randomization
  # nstart attempts multiple initial configurations
  SSW.u[i]=K$tot.withinss
}
plot(1:15,SSW.u,type="b", main="Elbow Plot")

#compute 4 clusters
K.u=kmeans(mds.user.matrix, 4, nstart=25) #this creates clusters
table(K.u$cluster) #see how many data in each cluster

#dimension reduction to 2 dimensional vector space to visualize
mds2.user.matrix = cmdscale(D.u, k=2)
plot(mds2.user.matrix,col=K.u$cluster)

## find position of tweets in cluster
clusterUserId1 = which(K.u$cluster == 1)
d1=length(clusterUserId1)
clusterUser = tweet.user.matrix[clusterUserId1,]
clusterUserWeight = colMeans(clusterUser)
s1=sort(clusterUserWeight, decreasing = TRUE)[1:10]
names(s1)

clusterUserId2 = which(K.u$cluster == 2)
d2=length(clusterUserId2)
clusterUser2 = tweet.user.matrix[clusterUserId2,]
clusterUserWeight2 = colMeans(clusterUser2)
s2=sort(clusterUserWeight2, decreasing = TRUE)[1:10]
names(s2)

clusterUserId3 = which(K.u$cluster == 3)
d3=length(clusterUserId3)
clusterUser3 = tweet.user.matrix[clusterUserId3,]
clusterUserWeight3 = colMeans(clusterUser3)
s3=sort(clusterUserWeight3, decreasing = TRUE)[1:10]
names(s3)

clusterUserId4 = which(K.u$cluster == 4)
d4=length(clusterUserId4)
clusterUser4 = tweet.user.matrix[clusterUserId4,]
clusterUserWeight4 = colMeans(clusterUser4)
s4=sort(clusterUserWeight4, decreasing = TRUE)[1:10]
names(s4)

#Dendrogram of the words used in tweets
#use tfidf weighted document term matrix
frequent.words = which(apply(tweet.user.matrix > 0, 2, sum) > 20)
#2 indicates columns, apply sum to columns
term.matrix = tweet.user.matrix[,frequent.words] #dtm
#normalize by columns, because words are in the columns in a dtm
norm.term.matrix = term.matrix %*% diag(1/sqrt(colSums(term.matrix^2)))
#normalization must be applied to cosine distance at the same time
colnames(norm.term.matrix) = colnames(term.matrix)
dtm <- norm.term.matrix
tf <- as.matrix(dtm)
( idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) )
( idf <- diag(idf) )
tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)

tf_idf

cosine_dist = 1-crossprod(tf_idf) /(sqrt(colSums(tf_idf^2)%*%t(colSums(tf_idf^2))))
index <- sample(1:70, size = 10)

#dtm1 <- dtm[index, ]
#dtm2 <- dtm[-index, ]
#D=dist(t(norm.term.matrix),method="cosine")^2/2 #cosine distance
#hierarchical clustering using single linkage
#install.packages("slam")
#library(slam)
#cosine_sim <- tcrossprod_simple_triplet_matrix(dtm1, dtm2)/sqrt(row_sums(dtm1^2) %*% t(row_sums(dtm2^2)))
#cosine_dist = 1-crossprod(tf_idf) /(sqrt(colSums(tf_idf^2)%*%t(colSums(tf_idf^2))))

## remove NaN's by 0
cosine_dist[is.na(cosine_dist)] <- 0

# create dist object
cosine_dist <- as.dist(cosine_dist)

cluster1 <- hclust(cosine_dist, method = "ward.D")

plot(cluster1)

cluster1 <- hclust(cosine_dist, method = "ward.D
h1=hclust(cosine_sim,method="single")
plot(h1)
rect.hclust(h1, k = 2, border = "red") 
