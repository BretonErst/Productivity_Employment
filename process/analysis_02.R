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
suppressWarnings(source("source/data_read.R"))

# df00 <- read_csv("gdp_pc_productivity.csv")


## Code for measure definition
# code_subject <- df00 %>% 
#   select(SUBJECT, Subject, MEASURE, Measure, Unit, PowerCode) %>% 
#   mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
#   unique()


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
  geom_hline(yintercept = 5,
             linetype = 2) +
  ylim(-2, NA) +
  labs(title = "How Alike Are Countries In Productivity Measures?") #-> sp01

# Plot save
ggsave(filename = "sp_01", plot = sp01, device = "tiff")

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
                   fontsize = 10,
                   fontsize_row = 8,
                   fontsize_col = 6,
                   main = "How Alike Is Productivity Among Countries?") #-> sp02

# Plot save
ggsave(filename = "sp_02", plot = sp02, device = "tiff")

# Visualization of clusters
fviz_cluster(object = list(data = features,
                           cluster = clusters),
             show.clust.cent = FALSE,
             ellipse.type = "ellipse",
             repel = TRUE) +
  theme(text = element_text(family = "Optima"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F9F9F9"),
        plot.title.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        plot.caption.position = "plot") +
  labs(title = "How Alike Are Countries When It Comes To Work Productivity?",
       subtitle = "Productivity Measures",
       x = NULL,
       y = NULL,
       caption = "Source: OECD Stats 2020. Figures in USD PPPs<br>
         <i>Gross Domestic Product, Gross Domestic Product Per Person
         Employed, Gross Domestic Product Per Hour Worked,
         Gross Domestic Product Per Head Of Population,
         Gross National Income Per Hour Worked.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") #-> sp03

# Plot save
ggsave(filename = "sp_03", plot = sp03, device = "tiff")


## Clustering kmeans algo
m_dist <- get_dist(features)
fviz_dist(m_dist)

# Optimal number of clusters
fviz_nbclust(matrix(features), 
             kmeans, 
             method = "silhouette")

# Tool to find optimal number of clusters
nm_clus <- NbClust(data = features, 
                   distance = "euclidean", 
                   min.nc = 2, 
                   max.nc = 10, 
                   method = "kmeans",
                   index = "alllong")

# Model fit for kmeans algo
klus <- kmeans(x = features, 
               centers = 5, 
               nstart = 25)

# Kmeans object
klus

# Visualization of clusters
fviz_cluster(object = klus, 
             data = features,
             repel = TRUE,
             ellipse.type = "norm",
             show.clust.cent = FALSE,
             star.plot = FALSE) +
  theme(text = element_text(family = "Optima"),
        legend.position = "none",
        panel.background = element_rect(fill = "#F9F9F9"),
        plot.title.position = "plot",
        plot.caption = element_markdown(color = "darkgrey",
                                        hjust = 0),
        plot.caption.position = "plot") +
  labs(title = "Countries Clustered By Efficiency of Production Systems",
       subtitle = "Work Productivity Measures",
       x = NULL,
       y = NULL,
       caption = "Source: OECD Stats 2020. Figures in USD PPPs<br>
         <i>Gross Domestic Product, Gross Domestic Product Per Person
         Employed, Gross Domestic Product Per Hour Worked,
         Gross Domestic Product Per Head Of Population,
         Gross National Income Per Hour Worked.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") #-> sp04
  
# Plot save
ggsave(filename = "sp_04", plot = sp04, device = "tiff")


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
                    nudge_x = 0.1,
                    max.overlaps = 20) +
    facet_wrap(~ state, 
               scales = "free") +
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "How Do Productivity Systems Stack?",
         subtitle = "Productivity Measure by Cluster",
         x = NULL,
         y = NULL,
         caption = "Source: OECD Stats 2020. Figures in USD PPPs<br>
         <i>Gross Domestic Product, Gross Domestic Product Per Person
         Employed, Gross Domestic Product Per Hour Worked,
         Gross Domestic Product Per Head Of Population,
         Gross National Income Per Hour Worked.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") #-> sp05

# Plot save
ggsave(filename = "sp_05", plot = sp05, device = "tiff")


df_sample <- df00 %>% 
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
  select(Country, TIME, GDP) %>% 
  arrange(desc(GDP))

df_sample %>% 
  ggplot(aes(x = GDP, 
             y = fct_reorder(Country, GDP))) +
    geom_bar(stat = "identity",
             show.legend = FALSE,
             fill = "steelblue",
             alpha = 0.7) +
    labs(y = NULL)
  
  
  
  
  
  
  













