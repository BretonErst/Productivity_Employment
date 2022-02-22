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
library(cluster)
library(NbClust)


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
                        "France", "United Kingdom", "Japan",
                        "Russia", "Turkey", "Italy")) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value) %>% 
  filter(TIME == 2020) %>% 
  select(-TIME) %>% 
  drop_na()

# Matrix of features
features <- df01 %>% 
  select(-c(LOCATION, Country)) %>% 
  scale() 

# Rownames to features matrix
rownames(features) <- df01$Country

# Cluster model fit hierarchical clustering algo
model_clust <- hclust(d = dist(features), 
                      method = "ward.D2")

# Dendogram plot for 5 clusters
plot(model_clust)
rect.hclust(model_clust, 
            k = 5, 
            border = "magenta")

# Viz of dendogram
fviz_dend(x = model_clust, 
          k = 5, 
          repel = TRUE) +
  labs(title = "Dendograma")

# Cluster assignation
clusters <- cutree(tree = model_clust, 
                   k = 5)

# Integration of clusters and original dataset
clustificado <- cbind(df01, clusters)
rownames(clustificado) <- NULL

# Heatmap
pheatmap::pheatmap(mat = features, 
                   scale = "none",
                   clustering_distance_cols = "euclidean",
                   clustering_distance_rows = "euclidean",
                   clustering_method = "average",
                   cluster_cols = FALSE,
                   cutree_rows = 5,
                   fontsize = 6,
                   main = "How Alike Is Productivity Among Countries?")

# Visualization of clusters
fviz_cluster(object = list(data = features,
                           cluster = clusters),
             show.clust.cent = FALSE,
             ellipse.type = "euclid",
             repel = TRUE) +
  theme(text = element_text(family = "Optima"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F9F9F9"),
        plot.title.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        plot.caption.position = "plot") +
  labs(title = "How Alike Are Countries When It Comes To Productivity?",
       subtitle = "Work Productivity",
       x = NULL,
       y = NULL,
       caption = "Source: OECD Stats.
         <i>Work productivity measures.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") 



## Clustering kmeans algo
m_dist <- get_dist(features)
fviz_dist(m_dist)

# Optimal number of clusters
fviz_nbclust(matrix(features), 
             kmeans, 
             method = "silhouette")

# Tool to find optimal number of clusters
nm_clus <- NbClust(data = features, distance = "euclidean", 
                   min.nc = 2, max.nc = 10, method = "kmeans",
                   index = "alllong")

# Model fit for kmeans algo
klus <- kmeans(x = features, centers = 3, nstart = 25)

# Kmeans object
klus

# Visualization od clusters
fviz_cluster(object = klus, 
             data = features,
             repel = TRUE,
             ellipse.type = "euclid",
             star.plot = FALSE)

# Integration of cluster
klusterizado <- cbind(df01, klus$cluster)
rownames(klusterizado) <- NULL

klusterizado <- klusterizado %>% 
  mutate(cluster = as.factor(`klus$cluster`)) %>% 
  select(-`klus$cluster`)

# Visualization of each feature for countries
klusterizado %>%
  pivot_longer(cols = GDP:GNIHRS, 
               names_to = "state", 
               values_to = "valor") %>%
  ggplot(aes(x = as.factor(state), 
             y = valor, 
             group = cluster, 
             color = cluster)) +
    geom_point(aes(shape = cluster),
                alpha = 0.7) +
    geom_text_repel(aes(label = Country),
                    size = 2.5,
                    segment.alpha = 0.7,
                    nudge_x = 0.1) +
    facet_wrap(~ state, 
               scales = "free") +
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Group By Cluster",
         subtitle = "Features by Cluster",
         x = NULL,
         y = NULL,
         caption = "Source: OECD Stats.
         <i>Figures in USD current PPPs</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") 
  
  
  
  
  
  
  
  
  













