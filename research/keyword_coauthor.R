############################################
# Keyword network and co-authorship network 
# Code by CJ Lee
############################################
# install.packages("wordcloud2")

# import the excel file
library(readxl)
df <- read_excel("research/cj_published_coauthor.xlsx")

# import libraries required
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(igraph)
library(visNetwork)

# Set the locale language
# Sys.setlocale("LC_ALL", locale="Korean")

#############
# Keywords wordcloud
#############

# df %>%  
#   unnest_tokens(word, Title) %>%
#   anti_join(get_stopwords()) %>%
#   count(word, sort = TRUE) %>% 
#   setNames(c("word", "freq"))-> df2
# 
# wordcloud2(df2, size = 0.5)


df %>% 
  # filter(Year>=2015 & Year<=2017) %>% 
  unnest_tokens(word, Title) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE) %>% 
  setNames(c("word", "freq")) %>% 
  filter(freq>1) %>%
  wordcloud2(., size = 0.5) %>% 
  visSave(file="research/keyword_wordcloud.html")


#############
# Co-author network
#############

pull_all_unique_authors <- function(df) {
  from <- which(colnames(df) == "Author_1")
  to <- which(colnames(df) == "Author_7")
  
  L1 <- apply(df, 1, function(y) unique(y[c(from:to)]))
  L1 <- lapply(L1, function(y) y[nchar(y) > 1 & !is.na(y)])
  V1 <- unique(unlist(L1))
  V1 <- V1[!is.na(V1)]
  
  return(V1)
}


# Function to convert data into an adjacency matrix
convert_to_adjacency_matrix <- function(df) {
  from <- which(colnames(df) == "Author_1")
  to <- which(colnames(df) == "Author_7")
  
  L1 <- apply(df, 1, function(y) unique(y[c(from:to)]))
  L1 <- lapply(L1, function(y) y[nchar(y) > 1 & !is.na(y)])
  V1 <- unique(unlist(L1))
  V1 <- V1[!is.na(V1)]
  L2 <- lapply(L1, function(y) !is.na(match(V1, y)))
  M1 <- do.call(rbind, L2)
  M2 <- t(M1) %*% M1
  rownames(M2) <- V1
  colnames(M2) <- V1
  # diag(M2) <- 0
  return(M2)
}

AM <- convert_to_adjacency_matrix(df)

# Pull node values (no. of collabo)
diag(AM) %>% 
  as_tibble %>% 
  mutate(label = pull_all_unique_authors(df)) %>% 
  filter(value!=max(value)) -> node_value

# put 0 for diagonal values of AM
diag(AM)<-0

# Adjacency matrix to Graph
AM %>% 
  graph_from_adjacency_matrix(., mode = "undirected", 
                              weighted=T, diag = F) -> g

# Graph to Node list
vertex_attr(g)$label<-vertex_attr(g)$name
g %>% get.vertex.attribute() -> node_list


# Node list for visualizing
node_final <- data.frame(id=1:length(node_list$name), 
                         label=node_list$name, 
                         color=NA, shadow=T) %>% 
  left_join(node_value) 

node_final %>% 
  select(label, id) %>% 
  setNames(c("label_f", "from")) -> node_f

node_final %>% 
  select(label, id) %>% 
  setNames(c("label_t", "to")) -> node_t

# Edge list for visualizing
edge <- cbind(get.edgelist(g) ,
              round(E(g)$weight, 3 ))

edge_list <- data.frame(label_f = edge[,1], 
                        label_t = edge[,2], 
                        value   = edge[,3])

edge_list %>% 
  left_join(node_f) %>% 
  left_join(node_t) %>% 
  select(from, to, value) -> edge_final

# Network Visualization 
visNetwork(node_final, edge_final, 
           height = "1000px", width = "100%") %>% 
  visOptions(nodesIdSelection = TRUE,  highlightNearest = TRUE) %>% 
  visNodes(shadow = TRUE, opacity=0.7) %>% 
  visEdges(color="#BDBDBD") %>% 
  visSave(file="research/coauthor_network.html") 

