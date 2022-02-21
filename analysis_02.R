###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################

## Libraries
library(tidyverse)
library(tidytext)
library(ggrepel)
library(ggtext)
library(factoextra)

## Data acquisition
df00 <- read_csv("gdp_pc_productivity.csv")


## Code for measure definition
code_subject <- df00 %>% 
  select(SUBJECT, Subject, MEASURE, Measure, Unit, PowerCode) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  unique()


## Data frame for USD Current PPPs
df01 <- df00 %>% 
  filter(MEASURE == "CPC") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain", "Germany",
                        "Costa Rica", "Korea", "Colombia",
                        "Greece", "Portugal", "Switzerland",
                        "France", "United Kingdom", "Japan")) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value) %>% 
  filter(TIME == 2020) %>% 
  select(-TIME)

features <- df01 %>% 
  select(-c(LOCATION, Country)) %>% 
  scale()

rownames(features) <- df01$Country

# Clustering
model_clust <- hclust(d = dist(features), 
                      method = "ward.D2")

# Ploteo de dendograma
plot(model_clust)
rect.hclust(model_clust, k = 5, border = "magenta")

fviz_dend(x = model_clust, k = 5, repel = TRUE) +
  labs(title = "Dendograma")

clusters <- cutree(tree = model_clust, k = 5)

clustificado <- cbind(df01, clusters)

rownames(clustificado) <- NULL


pheatmap::pheatmap(mat = features, 
                   scale = "none",
                   clustering_distance_cols = "euclidean",
                   clustering_distance_rows = "euclidean",
                   clustering_method = "average",
                   cutree_rows = 5)

