#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages('base64enc')
#install.packages('ROAuth')
#install.packages('plyr')
#install.packages('stringr')
#install.packages('twitteR')
#install.packages("ggplot2")
#install.packages("httpuv")
#install.packages("ggdendro")
#install.packages("igraph")
#install.packages("dplyr")
#install.packages("tidygraph")
library(tidygraph)
library(igraph)
library(ggdendro)
library(httpuv)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library("ggplot2")
library(stringr)
library(tidytext)
library(dplyr)
app="Nashi"                                  
key= "Uv2FoT2P9cWfJ5AM0ylHKuS9g"                                   
secret= "HPs9wgxNWHVitaxb4mQGO6yZ1qUvw7PXDBuv2ZjsuX4pAQ4R06"       
access_token="1327095808706060288-XsepkEopwPzljlaxuLLH23UfyvCMps"  
access_secret="z6I3KQoxPa2AaSjR0tAmY8EycB2XkJuAUSEYvqLIoWYcP"      
twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)
tweets.rtweet=search_tweets('KamalaHarris', n = 1000, type = "recent", token = twitter_token, include_rts = FALSE)
#Extracting tweets
tweet_celeb <- userTimeline("KamalaHarris", n=1000, includeRts = TRUE)
tweets_public <- tweets.rtweet
tweets.celeb = twitteR::twListToDF(tweet_celeb)
dim(tweets.celeb)
View(tweets.celeb)
tweets.public = twitteR::twListToDF(tweets_public)
dim(tweets.public)
View(tweets.public)
table(tweets.celeb$statusSource)
table(tweets.public$statusSource)

###########celeb tweet cleaning
tweets.celeb$stripped_text <- gsub('<a href="//twitter.com/download/',"",  tweets.celeb$statusSource)
tweets.celeb$stripped_text <- gsub('<a href="//twitter.com/download/',"", tweets.celeb$stripped_text)
library(tidytext)
tweets.celeb_clean<- tweets.celeb %>%
  dplyr::select(stripped_text) %>%
  tidytext::unnest_tokens(word, stripped_text)

ggplot(tweets.celeb, aes(x = created, fill = screenName)) +
  geom_histogram(
    position = "identity", bins = 50, show.legend = FALSE) +
  facet_wrap(~screenName, ncol = 1, scales = "free_y") + 
  ggtitle("celeb Tweet Activity (Adaptive y-axis)")

####celeb tweet most frequency word
tweets.celeb_clean %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x =word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets celeb")

############public tweet cleaning
tweets.public$stripped_text <- gsub('<a href="//twitter.com/download/',"",  tweets.public$statusSource)
tweets.public$stripped_text <- gsub('<a href="//twitter.com/download/',"", tweets.public$stripped_text)


tweets.public_clean<- tweets.public %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

####public tweet most frequency word
tweets.public_clean %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x =word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets public")


TAB <- table(tweets.celeb$statusSource, tweets.celeb$isRetweet)
TAB
chisq.test(TAB, correct=T)
TAB2 <- table(tweets.public$statusSource, tweets.public$isRetweet)
TAB2
chisq.test(TAB2, correct=T)


# 95% confidence Interval
width_text1 <- nchar(tweets.celeb$text)
width_text1
mean <- mean(width_text1)
mean
conf.level <- 0.95
z <- qt((1+conf.level)/2, df =1000-1)
z
se <- sd(width_text)/sqrt(1000)
se
ci <- z*se
ci
lower_limit <- mean - ci
lower_limit
upper_limit <- mean + ci
upper_limit

width_text <- nchar(tweets.public$text)
mean <- mean(width_text)
mean
conf.level <- 0.95
z <- qt((1+conf.level)/2, df =1000-1)
z
se <- sd(width_text)/sqrt(1000)
se
ci <- z*se
ci
lower_limit <- mean - ci
lower_limit
upper_limit <- mean + ci
upper_limit

#################### 8.2
#################### 8.2
##TFIDF

clean_tweet = gsub("&amp", "", tweets.celeb$text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
# Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
#data1.text <- sapply(data1, x)
twcomp=clean_tweet[1:70]
clean_tweet = gsub("&amp", "", tweets.public$text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
# Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
#data1.text <- sapply(data1, x)
#data1.text <- sapply(data1, x)
twpublic=clean_tweet
tweets<-data.frame(cbind(twpublic,twcomp))

corpus <- iconv(tweets$twcomp, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

############
tdm <- DocumentTermMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
#tdm[1:10, 1:20]
w <- rowSums(tdm)
w <- subset(w,w>=25)
barplot(w,
        las=2,
        col = rainbow((50)))
##############################
###Compute the most appropriate number of clusters using the elbow method for the combined tweets by using cosine distance.
distmatrix<-dist(scale(tdm),method="euclidean")

kmeans5<- kmeans(distmatrix, 5)
class(kmeans5)


kw_with_cluster <- data.frame(cbind(tweets$twcomp,kmeans5$cluster))
names(kw_with_cluster)[2] <- c("kmeans5")
head(kw_with_cluster)
dim(kw_with_cluster)
kw_with_cluster$num=1
no_clusters_vs_cost <- data.frame()

for(i in 1:20){
  #Run kmeans for each level of i, allowing up to 20 iterations for convergence
  kmeans<- kmeans(x=tdm, centers=i, iter.max=20)
  
  #Combine cluster number and costfunction together, write to df
  no_clusters_vs_cost<- rbind(no_clusters_vs_cost, cbind(i, kmeans$tot.withinss))
  
}
names(no_clusters_vs_cost) <- c("cluster", "cost")

ggplot(data=no_clusters_vs_cost, aes(x=cluster, y=cost)) + 
  theme_bw(base_family="Arial") + 
  geom_line(colour = "blue") +
  theme(text = element_text(size=10)) +
  ggtitle("Reduction In Cost For Values of 'k'") +
  xlab("Clusters") + 
  ylab("Within-Cluster Sum of Squares")

kmeans5<- kmeans(distmatrix, 4)
class(kmeans5)
kw_with_cluster <- data.frame(cbind(tweets$twcomp,kmeans5$cluster))
names(kw_with_cluster)[2] <- c("kmeans5")
head(kw_with_cluster)
dim(kw_with_cluster)
kw_with_cluster$num=1

#Split into different clusters

cluster1<-kw_with_cluster[kw_with_cluster$kmeans5==1,]
cluster2<-kw_with_cluster[kw_with_cluster$kmeans5==2,]
cluster3<-kw_with_cluster[kw_with_cluster$kmeans5==3,]
cluster4<-kw_with_cluster[kw_with_cluster$kmeans5==4,]


c1<-aggregate(kw_with_cluster$num,by=list(kw_with_cluster$kmeans5),sum)


##dendrogram
tweets.h<-hclust(distmatrix,method="ward")
cut<-cutree(tweets.h, k = 5)
plot(tweets.h,cex=0.1,hang=-1,which.plots = 2,main="Word cluster Dendogram")

# basic option
ggdendrogram(tweets.h,cex=0.9)
ggdendrogram(tweets.h, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(tweets.h, k = 5, cex=0.5,boxes = FALSE, col.up = "grey50", col.down = c("green", 
                                                                                "blue", "black","red","yellow","orange","brown"))

##################################

words=names(w)
words
d=data.frame(word=words,freq=w)
wordcloud(d$word,d$freq,random.order=FALSE,colors=brewer.pal(8,'Dark2'))

####################### 8.3:
user <- getUser("@KamalaHarris")
friends.object <- user$getFriends() # who I follow
friends_df <- twListToDF(friends)
save(friends_df, file = "my_friends.RData")
friends_Ds <- friends_df
friends_df %>%
  ggplot(aes(x = log2(friendsCount))) +
  geom_density(alpha = 0.8) +
  labs(x = "log2 of number of friends",
       y = "density")

top10friendsName <- friends_df %>%
  mutate(date = as.Date(created, format = "%Y-%m-%d"),
         today = as.Date("2019-09-20", format = "%Y-%m-%d"),
         days = as.numeric(today - date),
         statusesCount_pDay = statusesCount / days) %>%
  dplyr::select(screenName, friendsCount, statusesCount_pDay) %>%
  arrange(desc(friendsCount)) %>%
  dplyr::top_n(10)
top10friendsName
barplot(top10friendsName$friendsCount, main="Twitter friends by activity count", 
        ylab="Number of status messages", xlab="twitter friends", space=0.2, density=50, angle=1.5, cex.names=0.7) 

set.seed(1)
plot(friends, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

# Script for graphing Twitter friends/followers
# by Kai Heinrich (kai.heinrich@mailbox.tu-dresden.de) 

# load the required packages

library("twitteR")
library("igraph")

# HINT: In order for the tkplot() function to work on mac you need to install 
#       the TCL/TK build for X11 
#       (get it here: http://cran.us.r-project.org/bin/macosx/tools/)
#
# Get User Information with twitteR function getUSer(), 
#  instead of using ur name you can do this with any other username as well 

start<-getUser("KamalaHarris") 

# Get Friends and Follower names with first fetching IDs (getFollowerIDs(),getFriendIDs()) and then looking up the names (lookupUsers()) 

friends.object<-lookupUsers(start$getFriendIDs())
#follower.object<-lookupUsers(start$getFollowerIDs())
library(dplyr)
# Retrieve the names of your friends and followers from the friend
# and follower objects. You can limit the number of friends and followers by adjusting the 
# size of the selected data with [1:n], where n is the number of followers/friends 
# that you want to visualize. If you do not put in the expression the maximum number of 
# friends and/or followers will be visualized.


friends <- sapply(friends.object[1:100],name)
#followers <- sapply(followers.object[1:n],name)

# Create a data frame that relates friends and followers to you for expression in the graph
relations <- data.frame(User='elonmusk', Follower=friends) %>%
  #data.frame(User=followers, Follower='YOUR_NAME'), all=T)
  dplyr::top_n(10)
# Create graph from relations.
g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name

# Plot the graph using plot() or tkplot(). Remember the HINT at the 
# beginning if you are using MAC OS/X
plot(g)

g <- graph.data.frame(relations, directed = T)
#Betweenness Centrality
b <- betweenness(g,directed=TRUE)
sort(b,decreasing=TRUE)
g4 <- g
V(g4)$size <- b*1.5#can adjust the number
plot(g4,  vertex.label=NA)


