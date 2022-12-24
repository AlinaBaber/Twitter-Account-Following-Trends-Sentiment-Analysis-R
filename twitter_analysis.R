library("rtweet")
library("base64enc")
library("httpuv")
library("tm")
## Loading required package: NLP
library("SnowballC")
library("wordcloud")
## Loading required package: RColorBrewer
app="Nashi"                                  
key= "Uv2FoT2P9cWfJ5AM0ylHKuS9g"                                   
secret= "HPs9wgxNWHVitaxb4mQGO6yZ1qUvw7PXDBuv2ZjsuX4pAQ4R06"       
access_token="1327095808706060288-XsepkEopwPzljlaxuLLH23UfyvCMps"  
access_secret="z6I3KQoxPa2AaSjR0tAmY8EycB2XkJuAUSEYvqLIoWYcP"      
twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)
tweets=search_tweets('ElonMusk', n = 1000, type = "recent",
                     token = twitter_token, include_rts = FALSE)
saveRDS(tweets, file="tweets.rds")
corpus = Corpus(VectorSource(tweets$text))
corpus = tm_map(corpus, function(x) iconv(x, to='UTF8', sub='byte'))
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII', sub=' ')) # remove special characters
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus = tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+") #remove http...
corpus = tm_map(corpus, removeNumbers) # remove numbers
corpus = tm_map(corpus, removePunctuation) # remove punctuation
corpus = tm_map(corpus, stripWhitespace) # remove whitespace
corpus = tm_map(corpus, tolower) # convert all to lowercase
myStopList=c(stopwords(),"elon","musk","elonmusk","elonmusk","musk")
1
corpus = tm_map(corpus, removeWords, myStopList) # remove stopwords
corpus = tm_map(corpus, stemDocument) # convert all words to their stems

tweet.dtm = DocumentTermMatrix(corpus) #this is document term matrix
tweet.tdm=TermDocumentMatrix(corpus) #this is term document matrix
#tfidf matrices
weight.tdm=weightTfIdf(tweet.tdm)
weight.matrix=as.matrix(weight.tdm)
tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.matrix = as.matrix(tweet.wdtm)

#wordcloud from tfidf weights
freqsw = rowSums(weight.matrix) #use tdm
wordcloud(names(freqsw), freqsw, random.order=FALSE, scale=c(2,.17),min.freq=4,
          colors=brewer.pal(8,"Dark2"))


# first calculate term frequencies
tweet.tdm=as.matrix(tweet.tdm)
freqs = rowSums(tweet.tdm) #use tdm
id=order(freqs,decreasing = TRUE)[1:20]
names(freqs)[id]


prop=freqsw/s #calculating proportions
prop
barplot(prop[id], cex.names = 0.7, las=2, main="Proportion of top 20 frequent words")

#Dendrogram of the words used in tweets
#use tfidf weighted document term matrix
frequent.words = which(apply(tweet.matrix > 0, 2, sum) > 20)
#2 indicates columns, apply sum to columns
term.matrix = tweet.matrix[,frequent.words] #dtm
#normalize by columns, because words are in the columns in a dtm
norm.term.matrix = term.matrix %*% diag(1/sqrt(colSums(term.matrix^2)))
#normalization must be applied to cosine distance at the same time
colnames(norm.term.matrix) = colnames(term.matrix)
D=dist(t(norm.term.matrix),method="euclidean")^2/2 #cosine distance
#hierarchical clustering using single linkage
h1=hclust(D,method="single")
plot(h1)

#hierarchical clustering using complete linkage
h2=hclust(D,method="complete")
plot(h2)
rect.hclust(h2, k = 4, border = "red")

# Take user description information from the relevant column and repeat preprocessing
#tweets.rtweet$description
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
tweet.user.matrix
## remove empty tweets
empties = which(rowSums(abs(tweet.user.matrix)) == 0)
tweet.user.matrix = tweet.user.matrix[-empties,]

#Cosine distance works better than Euclidean in text data.
norm.user.matrix = diag(1/sqrt(rowSums(tweet.user.matrix ^2))) %*% tweet.user.matrix
## then create the distance matrix
D.u = dist(norm.user.matrix, method = "euclidean")^2/2
## perform MDS using 300 dimensions
mds.user.matrix = cmdscale(D.u, k=300)
mds.user.matrix

n=15
SSW.u=rep(0,n)
for(i in 1:n) {
  K=kmeans(mds.user.matrix,i,nstart=15)
  #choose 10 best inital randomization
  # nstart attempts multiple initial configurations
  SSW.u[i]=K$tot.withinss
}
plot(1:15,SSW.u,type="b", main="Elbow Plot")

SSW.u

#compute 4 clusters
K.u=kmeans(mds.user.matrix, 4, nstart=25) #this creates clusters
table(K.u$cluster) #see how many data in each cluster

#duser.matrix = cmimension reduction to 2 dimensional vector space to visualize
mds2.dscale(D.u, k=2)
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
retweet.count.c1=sum(tweets$retweet_count[clusterUserId1]!=0)
retweet.count.c2=sum(tweets$retweet_count[clusterUserId2]!=0)
retweet.count.c3=sum(tweets$retweet_count[clusterUserId3]!=0)
retweet.count.c4=sum(tweets$retweet_count[clusterUserId4]!=0)

d1=length(clusterUserId1)
d1
d2=length(clusterUserId2)
d2
d3=length(clusterUserId3)
d3
d4=length(clusterUserId4)
d3
m=matrix(c(retweet.count.c1,retweet.count.c2,retweet.count.c3,retweet.count.c4,
           d1-retweet.count.c1,d2-retweet.count.c2,d3-retweet.count.c3,d4-retweet.count.c4),
         byrow = TRUE,nrow=2)

colnames(m)=c("cluster1","cluster2", "cluster3","cluster4")
rownames(m)=c("retweeted","not retweeted")
m

# compute expected counts, in a chi-squared test expected counts must be bigger than 5.
E=( (rowSums(m)/sum(m)) %o% (colSums(m)/sum(m)) *sum(m)) #pqn
E

chisq.test(m,simulate.p.value = TRUE)

library("base64enc")
library("httpuv")
library("tm")
## Loading required package: NLP
library("SnowballC")
library("igraph")
##
## Attaching package: 'igraph'
## The following objects are masked from 'package:stats':
##
## decompose, spectrum
## The following object is masked from 'package:base':
##
## union
corpus = Corpus(VectorSource(tweets$text))
corpus = tm_map(corpus, function(x) iconv(x, to='UTF8', sub='byte'))
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII', sub=' ')) # remove special characters
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus = tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+") #remove http...
corpus = tm_map(corpus, removeNumbers) # remove numbers
corpus = tm_map(corpus, removePunctuation) # remove punctuation
corpus = tm_map(corpus, stripWhitespace) # remove whitespace
corpus = tm_map(corpus, tolower) # convert all to lowercase
myStopList=c(stopwords(),"elon","musk","elonmusk","elonmusk","musk")
corpus = tm_map(corpus, removeWords, myStopList) # remove stopwords
corpus = tm_map(corpus, stemDocument) # convert all words to their stems

tweet.dtm = DocumentTermMatrix(corpus) #this is document term matrix
#tfidf document term matrix

tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.matrix = as.matrix(tweet.wdtm)

tweet.wdtm
#Cosine similarity is the normalized S.

retweet.count=as.numeric(tweets)
hist(retweet.count,breaks=20)
retweet.count


#logarithm of the retweet counts
retweet.count.log=log(retweet.count+1)
#add 1 to all counts in order to compute logarithm for all values.
hist(retweet.count.log)

#create a weighted graph
g3=graph.adjacency(S,weighted=TRUE,diag=FALSE,mode="undirected") #this is a weighted graph
g3=delete.edges(g3,which(E(g3)$weight <8)) # delete edges with small weights.
g4=delete.vertices(simplify(g3), degree(g3)==0) # delete vertices with zero degree.

l = layout.kamada.kawai(g4)
l = norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
#take node size from retweet counts,
#increase the size of the edges by multiplying with a scaler to get more visible edge weigths.
a=log(tweets$retweet_count[V(g4)]+1)*2
plot(g4,layout = l, rescale=F,
     vertex.size=a,
     vertex.label.cex=0.5,edge.width=E(g3)$weight*0.08,
     edge.color="orange", vertex.color="tomato")

c1=order(closeness(g4),decreasing = TRUE)[1:10]
V(g3)$name[c1]
## [1] "105" "26" "108" "56" "91" "16" "34" "57" "71" "97"
c2=order(betweenness(g4),decreasing = TRUE)[1:10]
V(g3)$name[c2]
## [1] "108" "26" "64" "105" "91" "56" "4" "81" "41" "83"
c3=order(degree(g4),decreasing = TRUE)[1:10]
V(g3)$name[c3]
adjacency.to.probability = function(A) {
  cols = ncol(A)
  for (a in 1:cols) {
    A[, a] = normalise(A[, a])
  }
  return(A)
}
normalise = function(x) {
  if(sum(x)!=0){
    return(x/sum(x))
  } else return(0)
}
A = t(as.matrix(get.adjacency(g4)))
T=as.matrix(adjacency.to.probability(A))
#use random jump matrix as the graph is disjoint.
n=nrow(T)
J = matrix(rep(1/n, n^2), n, n)
alpha=0.8
M = alpha * T + (1 - alpha) * J
M = adjacency.to.probability(M)
#while loop for power method
5
stationary.distribution = function(T) {
  n = ncol(T)
  p = rep(0, n)
  p[1] = 1
  p.old = rep(0, n)
  while (differenceEuc(p, p.old) > 1e-06) { #instead of not equal, use this
    p.old = p
    p = T %*% p.old
  }
  return(p)
}
differenceEuc = function(x,y) {
  return(sqrt(sum((x - y)^2)))
}
p = stationary.distribution(M)

plot(p,type="b", main="Elbow Plot")
#order to find the most influential tweets
order(p,decreasing = TRUE)[1:10]
## [1] 26 49 81 41 94 73 4 64 91 55
#Stationary distribution using the Eigenvalue Decomposition
influential.tweets=order(p,decreasing = TRUE)[1:10]
influential.tweets


users=tweets$screen_name[influential.tweets]
unique(users)

influential.tweets=order(p,decreasing = TRUE)[1:70]
users=tweets$screen_name[influential.tweets]
unique(users)

InfluenceRatio= tweets$followers_count[influential.tweets]/
  tweets$friends_count[influential.tweets]

ActivityMeasure=tweets$statuses_count[influential.tweets]
#save users, influence ratio and activity measure in a data frame
df= data.frame(users,InfluenceRatio,ActivityMeasure)
df=unique(df)
df[1:10,]

plot(df$ActivityMeasure~df$InfluenceRatio, xlim = c(0, 16), ylab = "Acivity Measure",
     xlab ="Influence", main= "Top 10 Users", col="red")
text(df$ActivityMeasure~df$InfluenceRatio,labels = df$users, pos=4)


