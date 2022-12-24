library("rtweet")
app="Nashi"                                  
key= "Uv2FoT2P9cWfJ5AM0ylHKuS9g"                                   
secret= "HPs9wgxNWHVitaxb4mQGO6yZ1qUvw7PXDBuv2ZjsuX4pAQ4R06"       
access_token="1327095808706060288-XsepkEopwPzljlaxuLLH23UfyvCMps"  
access_secret="z6I3KQoxPa2AaSjR0tAmY8EycB2XkJuAUSEYvqLIoWYcP"      
twitter_token=create_token(app, key, secret, access_token, access_secret, set_renv = FALSE)
tweets.rtweet=search_tweets('elon musk', n = 1000, type = "recent", token = twitter_token, include_rts = FALSE)

saveRDS(tweets.rtweet, file="tweets.rds")

class(tweets.rtweet)
dim(tweets.rtweet)
tweets.rtweet[1,1:5]

#examine the column headings
names(tweets.rtweet)
View(tweets.rtweet)

#see the sixteenth tweet
tweets.rtweet$text[16]

#check the class
class(tweets.rtweet$text)

tweets.rtweet = as.data.frame(tweets.rtweet)

tweet.df = tweets.rtweet

#observe that text is one of the names
names(tweet.df) 
tweet.df$text[16]

#number of characters
nchar(tweet.df$text[16])

class(tweet.df)
names(tweet.df)

library("tm")
tweet.corpus = Corpus(VectorSource(tweet.df$text))

#convert charts to UTF8 format: for Windows users only
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='UTF8', sub='byte'))

class(tweet.corpus)
tweet.corpus$content[16]
tweet.df$text[16]

# convert special characters 
corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII', sub=' '))

#see what happens after
corpus$content[16] 
tweet.df$text[16]

#ignore url
corpus = tm_map(corpus,content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+","",x)))

#remove username
corpus = tm_map(corpus,content_transformer(function(x) gsub("@\\w+","",x)))

# remove numbers
corpus = tm_map(corpus, removeNumbers)

# remove punctuation 
corpus = tm_map(corpus, removePunctuation)

# remove whitespace
corpus = tm_map(corpus, stripWhitespace)

# convert all to lowercase
corpus = tm_map(corpus, tolower)

# remove stopwords
corpus = tm_map(corpus, removeWords, c(stopwords(),"joe","biden")) 

# convert all words to their stems
library(SnowballC)
corpus = tm_map(corpus, stemDocument)

#create the DocumentTermMatrix object (This creates a list)

#create your document matrix
tweet.dtm = DocumentTermMatrix(corpus) 
tweet.tdm = TermDocumentMatrix(corpus)

#convert to a matrix. This is the fd,t term in the week4 lecture slide  21
tweet.matrix = as.matrix(tweet.dtm)
tweet.matrixtdm = as.matrix(tweets.tdm)
tweet.matrixtdm
#Number of rows is the number of Tweets
dim(tweet.matrix)

tweet.matrix[1,1:4]
tweet.df$text[16]

empties = which(rowSums(as.matrix(tweet.matrix)) == 0)
empties
length(empties)

if(length(empties)!=0){
  tweet.matrix = tweet.matrix [,-empties]
}

#number of documents
N=nrow(tweet.matrix)
N

#in how many documents term t appeared in,
ft=colSums(tweet.matrix>0)
ft

TF=log(tweet.matrix+1)
IDF=log(N/ft) 
IDF
length(IDF)
TF

#to find weigthed matrix, you have to multiply term by term; 
tweets.wdtm=TF%*%diag(IDF)

dim(tweets.wdtm)
w=colSums(tweets.wdtm)
w
length(w)

#Locate the position of the top 20 words, according to the overall word weight

wordsWeight=order(w,decreasing = TRUE)[1:100] #gives the position
wordsWeight
w[3]
w[14]
w[96]

words=sort(w,decreasing = TRUE)[1:100] #gives the values
words

most.relevant.words=colnames(tweet.matrix)[wordsWeight]
most.relevant.words

most.relevant.words2=colnames(tweet.matrix)[words]
most.relevant.words2


#Wordcloud
#Wordcloud works with corpus object 
library("wordcloud")

wordcloud(corpus)

#word cloud of top 20 words
wordcloud(most.relevant.words,wordsWeight,random.order = FALSE, min.freq=1,scale=c(1,1),colors = brewer.pal(8,"Dark2"))
wordcloud(most.relevant.words2,words,random.order = FALSE, min.freq=1,scale=c(1,1),colors = brewer.pal(8,"Dark2"))

barplot(wordsWeight,names.arg=most.relevant.words)
barplot(words,names.arg=most.relevant.words2)

if(length(wordsWeight) >= 10){
  print(wordsWeight )
}

if(length(words) >= 10){
  print(words )
}

tweets.wdtm>=10

tweets.tdm = TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, 
                                                
                                                
                                                stopwords = c(stopwords()),
                                                #stopwords = TRUE,#use default stopwords
                                                removeNumbers = TRUE, 
                                                tolower = TRUE, 
                                                stemming=FALSE)) # No stemming 
dim(tweets.tdm)

tweet.tdm = TermDocumentMatrix(tweet.corpus)
tweet.wtdm = weightTfIdf(tweet.tdm)
tweet.matrix = t(as.matrix(tweet.wtdm)) #tweet.matrix is now in DTM form###################


as.matrix(tweets.tdm) #see how the tdm is structured.
#Observe the terms are laid in rows and tweets are in columns.


tweets.tdm
dim(tweets.tdm)
empties = which(colSums(as.matrix(tweets.tdm)) == 0)
length(empties)

if(length(empties)!=0){
  tweets.tdm = tweets.tdm[,-empties]
}
dim(tweets.tdm)

# Convert to a standard R matrix. 
M = as.matrix(tweets.tdm)
M

row.names(tweet.dtm)
colnames(tweet.dtm)

rowSums(tweets.tdm)

as.matrix(dist(t(tweet.dtm), method = "euclidean"))
tweets.tdm = t(tweet.dtm)
tweets.tdm

rowSums(tweets.tdm)

UTDM = TDM %*% diag(1/sqrt(colSums(TDM^2)))
ntdm = t(tweet.dtm) %*% diag(1/sqrt(colSums((t(tweet.dtm))^2)))
cdtm= dist(ntdm, method = "euclidean")^2/2

ntdm
colnames(tweet.dtm)
rownames(tweet.dtm)

tweet.dtm


K = kmeans(iris[,-5],3)
K
names(K)
K$cluster
length(K$cluster)

DTM = matrix(c(1,1,2,1,0,1),ncol = 2, byrow = TRUE)
DTM
row.names(DTM) =c("D1","D2","D3")
colnames(DTM) = c("Cow", "Jumps")
DTM

TDM = t(DTM)
TDM

#term frequencies in TDM
rowSums(tweets.tdm)
#term frequencies in DTM
colSums(DTM)

#names of terms in TDM
row.names(TDM)
#names of terms in DTM
colnames(DTM)

row.names(tweet.dtm)
colnames(tweet.dtm)

row.names(tweets.tdm)
colnames(tweets.tdm)

row.names(tweet.dtm) = c("doc1", "doc")

frequent.words = which(colSums(tweet.matrix > 0) > 50)
length(frequent.words)
term.matrix = tweet.matrix[,frequent.words]
term.matrix

norm.term.matrix = term.matrix %*% diag(1/sqrt(rowSums(term.matrix^2)))
norm.term.matrix

colnames(norm.term.matrix) = colnames(term.matrix)
colnames(norm.term.matrix)

n.tdm = t(norm.term.matrix)
D = dist(n.tdm, method = "euclidean")^2/2
D
as.matrix(D)

h = hclust(D, method="single") 
plot(h)

h = hclust(D, method="complete") 
plot(h)
h

dtm = matrix(c(1,2,3,4,5,6,7,8,9,10,11), byrow=TRUE,ncol = 4)
colnames(tweet.dtm) =c("aoc","aww","tesla","girl")
rownames(tweet.dtm) = c("doc1", "doc2", "doc3")
dtm

dtm = matrix(c(tweet.dtm), byrow = TRUE, ncol = 4)
tdm = matrix(c(tweets.tdm), byrow = TRUE, ncol = 4)
colnames(tweet.dtm)
row.names(tweet.dtm)
cbind(colnames(tweet.dtm),row.names(tweet.dtm))
tdm


