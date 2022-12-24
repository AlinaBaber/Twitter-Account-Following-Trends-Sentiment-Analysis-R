library(tidyverse)
#install.packages("httr")
#install.packages("rtweet")
#install.packages('here')
library(here)
library("httr")
library("rtweet")
library("igraph")
library(ggraph)
# the name of the twitter app created by you
appname <- "19262563Zeeshan"
# api key (replace the following sample with your key)
key <- "lkcfq0FdGcr5xJMGmf96XuKvG"
# api secret (replace the following with your secret)
secret <- "QIu7io5y6hF6X3DRycHI0sJ9udQEmvcoTaiGgJjMAy8Mw6LrMh"
# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
#Downloading the tweets posted by Emma Watson
#ew_tweets <- get_timeline("EmmaWatson", n = 3200)
my_friends <- get_friends("PlayStation")

ids <- sample.int(my_friends$user_id,75, useHash = FALSE)

# create empty list to store results
friends <- list()

# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends, here("data", "twitter_friends.csv"))
friends <- read_csv(here("data", "twitter_friends.csv"))
filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)
glimpse(net)
library(tidygraph)
g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g



ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.25, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness())
g2
ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()

#Betweenness Centrality
b <- betweenness(g,directed=TRUE)
sort(b,decreasing=TRUE)
g4 <- g
V(g4)$size <- b*1.5#can adjust the number
plot(g4,  vertex.label=NA)