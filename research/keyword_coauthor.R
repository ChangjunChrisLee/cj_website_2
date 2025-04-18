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

library(igraph)


# ① 커뮤니티 탐지 (예: Louvain 알고리즘)

# Louvain community detection
# g_community <- cluster_louvain(g)
# membership_vec <- membership(g_community)  # 각 노드의 그룹 번호

# ① cluster_walktrap() → 계층적 탐색 기반

# g_community <- cluster_walktrap(g)
# membership_vec <- membership(g_community)

# ① cluster_leiden() → Louvain 개선 버전 (세분화 가능)
g_community <- cluster_leiden(g, resolution_parameter = 0.3)  
membership_vec <- membership(g_community)

# ② 노드에 group 컬럼 추가
# 노드 정보에 group 번호 추가
node_final <- node_final %>%
  mutate(group = membership_vec[label])  # label은 노드명

# ③ visNetwork에서 group 적용
visNetwork(node_final, edge_final, 
           height = "1000px", width = "100%") %>% 
  visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE) %>% 
  visNodes(shadow = TRUE, opacity = 0.8) %>%
  visGroups(groupname = NULL) %>%  # 그룹 색 자동 매핑
  visEdges(color = "#BDBDBD") %>%
  visLegend() %>%
  visSave(file = "research/coauthor_network_grouped.html")



# ② cluster_leiden() → Louvain 개선 버전 (세분화 가능)

############################
# topic 
#########################

# 1. 텍스트 전처리 및 토픽 모델링 준비
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)

df %>% glimpse
# Title과 Abstract_eng를 결합한 텍스트 필드 생성
df_text <- df %>%
  mutate(text = paste(Title, Abstract_eng, sep = " ")) %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, Year, text)

# 토큰화 및 stopword 제거
df_tokens <- df_text %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "en")) %>%
  filter(str_detect(word, "[a-z]"))  # 영문자 필터

# DTM(Document-Term Matrix) 생성
df_dtm <- df_tokens %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# 2. LDA 토픽 모델 생성
lda_model <- LDA(df_dtm, k = 5, control = list(seed = 1234))


# 3. 토픽별 상위 키워드 시각화
library(ggplot2)
library(scales)

lda_topics <- tidy(lda_model, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic (Title + Abstract)", x = NULL, y = "β (Word Probability)")


doc_top <- read_csv("research/Documents_with_Topic_Labels_and_Disciplines.csv")



# 토픽별 논문수
df %>% glimpse
doc_top %>% glimpse

df %>% 
  left_join(doc_top %>% select(-Title)) -> doc_top_n


doc_top_n %>%
  count(topic_label_eng) %>%
  ggplot(aes(x = reorder(topic_label_eng, n), y = n, fill = topic_label_eng)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Number of Documents by Topic",
       x = "Topic",
       y = "Number of Documents")

# 연도별 토픽

doc_top_n %>%
  count(Year, topic_label_eng) %>%
  ggplot(aes(x = Year, y = n, color = topic_label_eng)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Topic Trends by Year",
       x = "Year",
       y = "Number of Documents",
       color = "Topic")

# 학문 분과 분포
library(tidyr)

doc_top_n %>%
  unnest(related_disciplines) %>%
  count(related_disciplines, sort = TRUE) %>%
  ggplot(aes(x = reorder(related_disciplines, n), y = n, fill = related_disciplines)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Distribution of Related Academic Disciplines",
       x = "Discipline",
       y = "Count")

# 토픽 & 연도 히트맵

doc_top_n %>%
  count(Year, topic_label_eng) %>%
  ggplot(aes(x = factor(Year), y = topic_label_eng, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Topic Frequency by Year (Heatmap)",
       x = "Year",
       y = "Topic",
       fill = "Count")


#################

library(ggtext)       # for modern styled titles/subtitles
library(showtext)     # for nice Google fonts (optional)

# 선택적으로 Google Fonts 로드
# showtext_auto()
# font_add_google("Roboto", "roboto")

# 토픽별 + 유형별 문서 수 집계
doc_top_n %>%
  count(topic_label_eng, Type) %>%
  group_by(topic_label_eng) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(topic_label_eng, total), y = n, fill = Type)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.5), 
            size = 4.2, color = "white") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "📚 Topic Distribution of the Current Research",
    subtitle = "Grouped by Topic Labels (SSCI, KCI, Book)",
    x = NULL,
    y = "",
    fill = "Output"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "gray30", size = 13, margin = margin(b = 15)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 20, 15, 20)
  )


#################
library(tidyverse)
library(ggthemes)   # for clean theme
library(scales)     # for year formatting if needed

doc_top_n %>%
  count(Year, topic_label_eng) %>%
  ggplot(aes(x = Year, y = n, color = topic_label_eng)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  labs(
    title = "📈 Topic Trends Over Time",
    subtitle = "Annual publication trends by topic area",
    x = "Year",
    y = "Number of Documents",
    color = "Topic"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, color = "gray40", margin = margin(b = 15)),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )


# 시작/끝 연도
start_year <- 2017
end_year <- 2024

ggplot(topic_rank, aes(x = Year, y = rank, group = topic_label_eng, color = topic_label_eng)) +
  # 선 그리기
  geom_line(size = 1.5, alpha = 0.8) +
  
  # 모든 시작점 강조
  # geom_point(data = filter(topic_rank, Year == start_year),
  #            size = 4, shape = 21, fill = "white", stroke = 1.2) +
  
  # 모든 끝점 강조
  geom_point(data = filter(topic_rank, Year == end_year),
             size = 5, shape = 21, fill = "white", stroke = 1.5) +
  
  # 끝점에 토픽명 표시 (모두)
  geom_text(data = filter(topic_rank, Year == end_year),
            aes(label = topic_label_eng),
            hjust = -0.1, size = 4, fontface = "bold", show.legend = FALSE) +
  
  scale_y_reverse(breaks = 1:length(unique(topic_rank$topic_label_eng))) +
  scale_color_brewer(palette = "Set2") +
  expand_limits(x = end_year + 0.8) +  # 오른쪽 라벨 공간 확보
  labs(
    title = "📊 Topic Ranking Over Time",
    subtitle = "All topics’ start/end points emphasized and labeled at the latest year",
    x = "Year",
    y = "Rank (1 = Most Frequent)",
    color = "Topic"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

################


# 1. 연도를 factor로 변환하며 모든 조합 채움
topic_prop_filled <- doc_top_n %>%
  count(Year, topic_label_eng) %>%
  complete(Year, topic_label_eng, fill = list(n = 0)) %>%
  filter(Year >= 2019 & Year <= 2024) %>% 
  group_by(Year) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(Year = factor(Year))  # 연도를 factor로 변환

# 2. 면적 그래프
ggplot(topic_prop_filled, 
       aes(x = Year, y = proportion, fill = topic_label_eng, group = topic_label_eng)) +
  geom_area(position = "stack", alpha = 0.9, color = "white", size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "📊 Topic Proportions Over Time",
    subtitle = "Relative share of each topic by year",
    x = "Year",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40", margin = margin(b = 15)),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
