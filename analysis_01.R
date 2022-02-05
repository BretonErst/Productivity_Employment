# Libraries
library(tidyverse)


# Data acquisition
df00 <- read_csv("gdp_pc_productivity.csv")


# Distinct instances of time
df00 %>% 
  distinct(Time)

# Distinct instances of Country 
df00 %>% 
  distinct(Country)


# Top 10 Countries with most hour worked per person
df00 %>% 
  filter(Time == 2020 & Subject == "Average hours worked per person employed") %>% 
  select(Country,
         Value) %>% 
  slice_max(order_by = Value, 
            n = 10, 
            with_ties = FALSE) %>% 
  ggplot(aes(x = Value, 
             y = reorder(Country, Value))) + 
    geom_col(fill = "darkgreen",
             alpha = 0.70) +
    geom_text(aes(label = format(round(Value), 1, 
                                 big.mark = ","),
                  hjust = 1.3),
              color = "grey95")
  

# Top 3 countries with most hours worked per person 2018-2020
df00 %>% 
  filter(Subject == "Average hours worked per person employed") %>% 
  filter(Time > 2017) %>% 
  select(Country,
         Time,
         Value) %>% 
  group_by(Time) %>%
  slice_max(order_by = Value, 
            n = 5, 
            with_ties = FALSE) %>% 
  ggplot(aes(y = fct_reorder(Country, Value), 
             x = Value)) +
    geom_col(fill = "darkgreen", 
             alpha = 0.7) +
    geom_text(aes(label = format(round(Value, 1), big.mark = ","),
                  hjust = 1.3), 
              color = "grey85", 
              size = 3) +
    facet_grid(Time ~ ., 
               scales = "free") +
    theme(axis.title.y = element_blank()) +
    labs(title = "Horas Trabajadas por Persona Empleada",
         x = "Horas trabajadas")

