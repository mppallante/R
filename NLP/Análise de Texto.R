### Nome : Análise de Texto       ### 
### Tipo : Anotação               ###
### Autor: Matheus Pina Pallante. ### 
### Ano  : 2021                   ### 

# Pacotes -----------------------------------------------------------------
library(dplyr)
library(reshape2)
library(tm)
library(rtweet)
library(igraph)
library(ggraph)
library(ggplot2)
library(networkD3)
library(udpipe)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(stm)
library(BTM)
library(textplot)
library(concaveman)
library(syuzhet)
library(stopwords)
library(echarts4r)

# Preparação e Tratamento dos Dados ---------------------------------------

# Definir função "notin"
`%notin%` <- Negate(`%in%`)

# StopWords
STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
STOP <- stopwords(language = "pt", source = "stopwords-iso")

# Tokens & DFM
df <- text$text %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         split_hyphens = TRUE) %>%
  tokens_remove(c(STOP_TWITTER, STOP, "#*", "@*", emojis$code)) %>%
  dfm(tolower = TRUE) %>%
  dfm_group(text$screen_name)

# Nuvem de Palavras -------------------------------------------------------

# Palavras frequentes
frequency_cloud <- textstat_frequency(df, n = 500)
# Nuvem de palavras
frequency_cloud %>% 
  e_color_range(frequency, color) %>% 
  e_charts() %>% 
  e_cloud(feature, frequency, color, shape = "square", sizeRange = c(20, 60)) %>% 
  e_tooltip() %>% 
  e_theme("infographic") %>%
  e_legend(show = F) %>% 
  e_title("text", "Frequência de palavras")

# Gráfico de Ocorrências --------------------------------------------------

# Palavras frequentes
frequency_word <- textstat_frequency(df, n = 15)
# Gráfico de volumetria
frequency_word %>%
  arrange(frequency) %>%
  e_charts(feature) %>%
  e_bar(frequency, name = "Frequência") %>%
  e_tooltip() %>%
  e_theme("infographic") %>%
  e_legend(show = F) %>%
  e_labels(position = "right") %>% 
  e_flip_coords()

# Rede de Palavras --------------------------------------------------------

# Identifica as maiores frequencias
toptag <- names(topfeatures(df, 50))
tag_fcm <- fcm(df)
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)

# Transforma em objeto igraph
tn <- as.igraph(topgat_fcm, min_freq = 1, omit_isolated = FALSE)
wc <- cluster_walktrap(tn)
members <- membership(wc)

# Transforma em formato compativel com networkD3
graph_d3 <- igraph_to_networkD3(tn, group = members)
links <- as.data.frame(graph_d3$links)
nodes <- as.data.frame(graph_d3$nodes)

# Cria o dataframe final com as associações
target <- NA
source <- NA
clusts <- NA
for (i in 1:length(links$source)) {
  target[i] <- nodes$name[[links$target[i]]]
  source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
  clusts[i] <- ifelse(links$source[i] == 0, NA, nodes$group[[links$source[i]]])
}
graph_df = data.frame(
  source = source,
  target = target,
  clusts = factor(clusts)
)
graph_df <- graph_df %>%
  group_by(source, target, clusts) %>%
  summarise(freq = n()) %>%
  na.omit()
wordnetwork <- as.data.frame(graph_df[,-3])
names(wordnetwork) <- c("term1","term2","cooc")
wordnetwork <- graph_from_data_frame(wordnetwork)

# Rede de palavras
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "firebrick") +
  geom_node_text(aes(label = name), col = "black", size = 5) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Coocorrências dentro da frase", subtitle = "Análise de Texto (NLP)")


# Análise de Sentimentos --------------------------------------------------

# Realiza a nálise de sentimentos
felling <- get_nrc_sentiment(text$text, language = "portuguese")
names(felling) <- c("Raiva", "Ansiedade", "Desgosto", "Receio", "Alegria", "Tristeza",
                    "Surpresa", "Confiança", "Negativo", "Positivo")
# Preparação dos dados
felling <- felling %>%
  # group_by(Serviço) %>%
  summarise("Raiva" = round(sum(Raiva)),
            "Ansiedade" = round(sum(Ansiedade)),
            "Desgosto" = round(sum(Desgosto)),
            "Receio" = round(sum(Receio)),
            "Alegria" = round(sum(Alegria)),
            "Tristeza" = round(sum(Tristeza)),
            "Surpresa" = round(sum(Surpresa)),
            "Confiança" = round(sum(Confiança)),
            "Negativo" = round(sum(Negativo)),
            "Positivo" = round(sum(Positivo)))
felling$ID <- "Geral"
felling <- melt(felling,
                id.vars = "ID",
                variable.names = "Sentimentos",
                value.name = "Volumetria")
felling$color <-  c("#D3D3D3","#D3D3D3",
                    "#D3D3D3","#D3D3D3",
                    "#D3D3D3","#D3D3D3",
                    "#D3D3D3","#D3D3D3",
                    "#B22222","#32CD32")

felling %>%
  arrange(Volumetria) %>%
  e_charts(variable) %>%
  e_bar(Volumetria, name = "Freqência") %>%
  e_title("Freqência de palavras sobre os sentimentos") %>%
  e_tooltip() %>%
  e_theme("infographic") %>%
  e_legend(show = F) %>%
  e_labels(position = "right")  %>% 
  e_flip_coords() %>%
  e_add("itemStyle", color) 


# Análise de Tópicos ------------------------------------------------------

# Transforma em objeto igraph
tn <- as.igraph(fcm(df), min_freq = 1, omit_isolated = FALSE)
wc <- cluster_walktrap(tn)
members <- membership(wc)

# Transforma em formato compativel com networkD3
graph_d3 <- igraph_to_networkD3(tn, group = members)
links <- as.data.frame(graph_d3$links)
nodes <- as.data.frame(graph_d3$nodes)
target <- NA
source <- NA
clusts <- NA
for (i in 1:length(links$source)) {
  target[i] <- nodes$name[[links$target[i]]]
  source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
  clusts[i] <- ifelse(links$source[i] == 0, NA, nodes$group[[links$source[i]]])
}
graph_df = data.frame(
  source = source,
  target = target,
  clusts = factor(clusts)
)
graph_df <- graph_df %>%
  group_by(doc_id = clusts, lemma = source) %>%
  summarise(freq = n()) %>%
  na.omit()  
graph_df <- graph_df[, c("doc_id", "lemma")]

# Análise de topicos
model <- BTM(graph_df, k = 10, window = 15, iter = 5000)
plot(model)

