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


# Distinct instances of Country 
df00 %>% 
  filter(Time == 2020 & Subject == "Average hours worked per person employed") %>% 
  select(Country,
         Value) %>% 
  slice_max(order_by = Value, 
            n = 10, 
            with_ties = FALSE) %>% 
  ggplot(aes(x = Value, 
             y = reorder(Country, Value))) + 
  geom_col(fill = "steelblue",
           alpha = 0.85) 



df00 %>% 
  filter(`Employment status` == "Total employment") %>% 
  filter(Time > 2017) %>% 
  select(Country,
         Time,
         Value) %>% 
  group_by(Time) %>%
  slice_max(order_by = Value, 
            n = 3, 
            with_ties = FALSE) %>% 
  ggplot(aes(y = reorder(Country, Value), 
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
       x = "Horas trabajadas por persona empleada")
