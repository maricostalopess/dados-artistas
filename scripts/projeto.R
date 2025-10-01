install.packages("igraph")
install.packages("ggraph") 
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(igraph)
library(ggraph)

dados <- read_csv("artistas_relacionados.csv", col_names = FALSE)

glimpse(dados)
spec_csv("artistas_relacionados.csv")

dadost <- as.data.frame(t(dados))

glimpse(dadost)

colnames(dadost)

names(dadost) <- c("artista", "correlacao_1", "correlacao_2", "correlacao_3", "correlacao_4", "correlacao_5",
                  "correlacao_6", "correlacao_7", "correlacao_8", "correlacao_9", "correlacao_10", "correlacao_11",
                  "correlacao_12", "correlacao_13", "correlacao_14", "correlacao_15", "correlacao_16", "correlacao_17",
                  "correlacao_18", "correlacao_19", "correlacao_20")

glimpse(dadost) # conferindo se alterou certo

dadost$insta <- NA
glimpse(dadost) # conferindo se adicionou certo

dadost[,1] #pedindo nome dos artistas pra eu não me perder imputando dados
dadost <- dadost[,-39]
glimpse(dadost)
dim(dadost)
dadost <- dadost %>% select(-insta)
dim(dadost)
glimpse(dadost)

----------------

str(insta)

insta <- read_excel("seguidores_instaR.xlsx", sheet = "Planilha1")
getwd()
setwd("/Users/marianalopes/Downloads")
getwd()
insta <- read_excel("seguidores_instaR.xlsx", sheet = "Planilha1")
dim(insta)

dadost <- dadost[-40, ]

# integrando bases:
dadost$insta <- insta$insta
sample_n(dadost, 10)

# colocando dados sobre gênero musical e ouvintes mensais:

genouv <- genouv %>%
  mutate(
    artista = str_replace_all(artista, "\\s", " "),
    artista = str_squish(artista)
  )

dadost <- dadost %>%
  left_join(
    genouv %>% select(artista, gênero, `ouvintes mensais`),
    by = "artista"
  )  

dadost <- dadost %>%
  select(-gênero.x, -`ouvintes mensais.x`)

dadost <- dadost %>%
  select(-gênero.y, -`ouvintes mensais.y`)

# dadost <- dadost %>%
  # rename(
    # gênero = gênero.y,
    # ouvintes_mensais = `ouvintes mensais.y`
  # )

dadost[ dadost$artista == "Anitta", "gênero" ] <- "Pop"
dadost[ dadost$artista == "Metallica", "gênero" ] <- "Rock"
dadost[ dadost$artista == "Maria Bethânia", "gênero" ] <- "MPB & Samba"
dadost[ dadost$artista == "Zeca Pagodinho", "gênero" ] <- "MPB & Samba"

# Estatística Descritiva
summary(dadost)








#grafos

# 1) Edges
from <- dadost[[1]]
tos  <- dadost[, 2:21]

edges <- data.frame(
  from = rep(from, times = ncol(tos)),
  to   = unlist(tos, use.names = FALSE)
) %>%
  filter(!is.na(to))

# 2) Lista completa de artistas
todos_artistas <- unique(c(edges$from, edges$to))

# 3) Nodes com atributos
nodes <- data.frame(name = todos_artistas) %>%
  left_join(
    dadost %>% select(artista, gênero, `ouvintes mensais`),
    by = c("name" = "artista")
  ) %>%
  mutate(
    gênero = ifelse(is.na(gênero), "Desconhecido", gênero),
    `ouvintes mensais` = ifelse(is.na(`ouvintes mensais`), 1000, `ouvintes mensais`),
    font_size = ifelse(`ouvintes mensais` > 9.0e+07, 3, 2.2)
  )

# 4) Grafo inicial
g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# 5) Paleta
paleta <- c(
  "Pop"         = "#8E44AD",  # roxo
  "Rock"        = "#2C3E99",  # azul escuro
  "Heavy metal" = "#2C3E99",
  "Funk"        = "#991232",  # amarelo
  "MPB & Samba" = "#991232",  # amarelo
  "MPB"         = "#991232",  # amarelo
  "Reggae"      = "#2980B9",  # azul médio
  "R&B"         = "#2e9995",  # lilás
  "Hip hop"     = "#2e9995",
  "Desconhecido"= "#F4D03F"
)

# 6) Pesos das arestas → Pop–Pop bem mais fracas
edges <- edges %>%
  left_join(nodes %>% select(name, gênero), by = c("from" = "name")) %>%
  rename(gen_from = gênero) %>%
  left_join(nodes %>% select(name, gênero), by = c("to" = "name")) %>%
  rename(gen_to = gênero) %>%
  mutate(weight = ifelse(gen_from == "Pop" & gen_to == "Pop", 0.000000000000000000000001, 1))






# Recriar grafo (com todos)
g <- graph_from_data_frame(edges %>% select(from, to, weight), vertices = nodes, directed = FALSE)

# 7) Layout
set.seed(123)
afastamento <- 2
coords <- layout_with_fr(
  g,
  niter = 15000,
  area = vcount(g)^4 * afastamento,
  repulserad = vcount(g)^4 * afastamento,
  weights = E(g)$weight
)

# 8) Plot
ggraph(g, layout = "manual", x = coords[,1], y = coords[,2]) + 
  geom_edge_link(colour = "orange", alpha = 0.8) +
  geom_node_point(aes(size = `ouvintes mensais`, fill = gênero),
                  shape = 21, stroke = 0) +
  geom_node_text(aes(label = name),
                 size = 2.2, color = "black", repel = FALSE) +  # fonte sempre preta
  scale_size_continuous(range = c(6, 24)) +
  scale_fill_manual(values = paleta) +
  theme_void() +
  theme(legend.position = "bottom")


# ====================================
  
# Gráfico de Barras
  
str(dadost)
  
dadost[dadost$artista == "Rihanna", "insta"] <- 149000000 #socorro o dado do insta da rihanna tá errado
  
cluster1 <- dadost %>% filter(artista %in% c("Ariana Grande", "Billie Eilish", "Camila Cabello", "Chappell Roan",
                                             "Demi Lovato", "Doechii", "Gracie Abrams", "Imagine Dragons",
                                             "Justin Bieber", "Katy Perry", "Lady Gaga", "Niall Horan",
                                             "Olivia Rodrigo", "Rihanna", "Sabrina Carpenter", "Selena Gomez", 
                                             "Shawn Mendes", "Taylor Swift”, “The Weeknd"))

ggplot(cluster1, aes(x = reorder(artista, -insta), y = `insta`, fill = gênero)) +
  geom_col() +
  scale_fill_manual(values = paleta) +
  theme_minimal() +
  labs(x = "artista", y = "seguidores instagram", fill = "gêneros") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cluster2 <- dadost %>% filter(artista %in% c("Anitta", "BLACKPINK", "BTS", "Carol Biazin", 
                                             "DUDA BEAT", "Jão", "RBD"))

ggplot(cluster2, aes(x = reorder(artista, -insta), y = `insta`, fill = gênero)) +
  geom_col() +
  scale_fill_manual(values = paleta) +
  theme_minimal() +
  labs(x = "artista", y = "seguidores instagram", fill = "gêneros") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cluster3 <- dadost %>% filter(artista %in% c("Bob Marley & The Wailers", "Charlie Brown Jr.", "Cazuza”, 
                                             “Engenheiros do Hawaii", "Guns N' Roses", "Maria Bethânia”, 
                                             “Metallica", "Queen", "Rita Lee", "The Beatles”, 
                                             “The Rolling Stones", "U2", "Zeca Pagodinho"))

ggplot(cluster3, aes(x = reorder(artista, -insta), y = `insta`, fill = gênero)) +
  geom_col() +
  scale_fill_manual(values = paleta) +
  theme_minimal() +
  labs(x = "artista", y = "seguidores instagram", fill = "gêneros") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


