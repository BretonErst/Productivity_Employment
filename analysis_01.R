###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################

# Libraries
library(tidyverse)
library(tidytext)
library(ggrepel)
library(ggtext)


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
              color = "grey85") +
    theme(plot.caption = element_markdown(color = "darkgrey")) +
    labs(title = "Hours Worked Per Person Employed 2020",
         subtitle = "Top 10 Countries",
         x = "Hours worked",
         y = " ",
         caption = "Source: OECD")
  

# Top 3 countries with most hours worked per person 2017-2020
df00 %>% 
  filter(Subject == "Average hours worked per person employed") %>% 
  filter(Time > 2016) %>% 
  select(Country,
         Time,
         Value) %>% 
  group_by(Time) %>%
  slice_max(order_by = Value, 
            n = 3, 
            with_ties = FALSE) %>% 
  ggplot(aes(y = reorder_within(x = Country, 
                                by = Value , 
                                within = Time), 
             x = Value)) +
    geom_col(fill = "darkgreen", 
             alpha = 0.7) +
    geom_text(aes(label = format(round(Value, 0), big.mark = ","),
                  hjust = 1.3), 
              color = "grey85", 
              size = 3) +
    facet_grid(Time ~ ., 
               scales = "free") +
    theme(axis.title.y = element_blank(),
          plot.caption = element_markdown(color = "darkgrey")) +
    labs(title = "Hours Worked Per Person Employed",
         subtitle = "Top 3 Countries Per Year",
         x = "Hours worked") +
    scale_y_reordered()


# Code for measure definition
code_subject <- df00 %>% 
  select(SUBJECT, Subject, MEASURE, Measure, Unit, PowerCode) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  unique()



# Data frame for USD Current PPPs
df01 <- df00 %>% 
  filter(MEASURE == "CPC") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value)

# Evolution of GDP per hour worked
df01 %>% 
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain")) %>% 
  mutate(max_va = ifelse(TIME == 2020, Country, NA)) %>% 
  ggplot(aes(x = TIME, 
             y = GDPHRS, 
             color = Country)) +
    geom_line() +
    geom_text_repel(aes(label = max_va),
                    na.rm = TRUE,
                    size = 3,
                    direction = "y",
                    hjust = -1,
                    segment.size = 0.7,
                    segment.alpha = 0.5,
                    segment.linetype = "dotted") +
    scale_x_continuous(limits = c(1970, 2025.5)) +
    theme(legend.position = "none",
          plot.caption = element_markdown(color = "darkgrey"),
          plot.caption.position = "plot") +
    labs(title = "Gross Domestic Product Per Hour Worked",
         subtitle = "Current USD ",
         caption = "<i>Elaboración propia</i><br>Source: OECD")



# Top 5 Countries GDPHRS
df01 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME > 2016) %>% 
  group_by(TIME) %>%
  slice_max(order_by = GDPHRS, 
            n = 5, 
            with_ties = FALSE) %>% 
  ggplot(aes(x = GDPHRS,
             y = reorder_within(x = Country, 
                                by = GDPHRS, 
                                within = TIME))) + 
    geom_col(fill = "darkgreen",
             alpha = 0.7) +
    geom_text(aes(label = paste("$", 
                                format(round(GDPHRS, 2), 
                                       big.mark = ",")),
                  hjust = 1.3),
              color = "grey85",
              size = 3) +
    facet_grid(rows = vars(TIME), 
               scales = "free") +
    scale_y_reordered() +
    theme(axis.title.y = element_blank(),
          plot.caption = element_markdown(color = "darkgrey")) +
    labs(title = "GDP Per Hour Worked",
         subtitle = "Top 5 in USD Current PPPs",
         x = "USD",
         caption = "Source:OECD")

# 2020
df01 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME == 2020) %>% 
  drop_na(GDPHRS) %>%
  ggplot(aes(x = GDPHRS,
             y = fct_reorder(Country, GDPHRS))) + 
    geom_col(fill = "darkgreen",
             alpha = 0.7) +
    geom_text(aes(label = paste("$", 
                                format(round(GDPHRS, 2), 
                                       big.mark = ",")),
                  hjust = 1.3),
              color = "grey85",
              size = 2) +
    theme(axis.title.y = element_blank(),
          plot.caption = element_markdown(color = "darkgrey")) +
    labs(title = "GDP Per Hour Worked 2020",
         subtitle = "USD Current PPPs",
         x = "USD",
         caption = "Source: OECD")



# Gross National Income per hour worked
df02 <- df00 %>% 
  filter(Subject == "Gross national income per hour worked") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value)


df02 %>% 
  filter(TIME > 2016) %>% 
  group_by(TIME) %>% 
  slice_max(order_by = GNIHRS,
            n = 5, with_ties = FALSE) %>% 
  ggplot(aes(x = GNIHRS, 
             y = reorder_within(x = Country,
                                by = GNIHRS,
                                within = TIME))) + 
    geom_col(fill = "darkgreen",
             alpha = 0.7) +
    geom_text(aes(label = paste("$", 
                                format(round(GNIHRS, 2), 
                                       big.mark = ",")),
                  hjust = 1.3),
              color = "grey85",
              size = 3) +
    facet_grid(rows = vars(TIME),
               scales = "free") +
    theme(axis.title.y = element_blank(),
          plot.caption = element_markdown(color = "darkgrey")) +
    scale_y_reordered() +
    labs(title = "Income Per Hour Worked",
         subtitle = "Top 5 in USD Current PPPs",
         x = "USD per hour",
         caption = "Source: OECD")


df02 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME == 2020) %>% 
  drop_na(GNIHRS) %>%
  ggplot(aes(x = GNIHRS,
             y = fct_reorder(Country, GNIHRS))) + 
  geom_col(fill = "darkgreen",
           alpha = 0.7) +
  geom_text(aes(label = paste("$", 
                              format(round(GNIHRS, 2), 
                                     big.mark = ",")),
                hjust = 1.3),
            color = "grey85",
            size = 2) +
  theme(axis.title.y = element_blank()) +
  labs(title = "Income Per Hour Worked 2020",
       subtitle = "USD Current PPPs",
       x = "USD per hour")


# iPhone Index (8 hour work journey)
df02 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME == 2020) %>% 
  drop_na(GNIHRS) %>%
  mutate(iPhone_index = (999 / GNIHRS) / 8) %>% 
  ggplot(aes(x = iPhone_index,
             y = fct_reorder(Country, iPhone_index))) + 
  geom_col(fill = "darkgreen",
           alpha = 0.7) +
  geom_text(aes(label = format(round(iPhone_index, 2), 
                               big.mark = ","),
                hjust = 1.2),
            color = "grey85",
            size = 2) +
  theme(axis.title.y = element_blank()) +
  labs(title = "iPhone Index 2020",
       subtitle = "USD Current PPPs",
       x = "8-hour Work Journey",
       caption = "Source: OECD")



# Evolution of Gross Income per Hour
df02 %>% 
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain")) %>% 
  mutate(max_va = ifelse(TIME == 2020, Country, NA)) %>% 
  ggplot(aes(x = TIME, 
             y = GNIHRS, 
             color = Country)) +
    geom_line() +
    geom_text_repel(aes(label = max_va),
                    na.rm = TRUE,
                    size = 3,
                    direction = "y",
                    hjust = -1,
                    segment.size = 0.7,
                    segment.alpha = 0.5,
                    segment.linetype = "dotted") +
    scale_x_continuous(limits = c(1970, 2025.5)) +
    theme(legend.position = "none",
          plot.caption = element_markdown(color = "darkgrey"),
          plot.caption.position = "plot") +
    labs(title = "Gross National Income Per Hour Worked",
         subtitle = "Current USD ",
         caption = "<i>Elaboración propia</i><br>Source: OECD")







    

