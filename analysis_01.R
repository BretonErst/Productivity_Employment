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
library(showtext)


# Data acquisition
df00 <- read_csv("gdp_pc_productivity.csv")

# Font instalation
font_add_google(family = "roboto", "Roboto")

showtext_auto()


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
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")
  

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
         x = "Hours worked",
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
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
    scale_x_continuous(limits = c(1970, 2027.5)) +
    theme(legend.position = "none",
          plot.caption = element_markdown(color = "darkgrey"),
          plot.caption.position = "plot") +
    labs(title = "Gross Domestic Product Per Hour Worked",
         subtitle = "USD Current PPPs ",
         x = "Year",
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")


# Preparation of data frame
## GDP Per Hour Worked
df_plot1 <- df01 %>%
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain", "Germany",
                        "Costa Rica", "Korea")) %>%
  filter(TIME >1989) %>% 
  mutate(val20 = ifelse(TIME == 2020, GDPHRS, NA),
         destaca = (Country == "Mexico"))
  
# Median value GDPHRS 2020
valor_medio <- df_plot1 %>% 
  summarize(media = median(val20, na.rm = TRUE)) %>% 
  pull()


# Visualization of Data
wp_01 <- df_plot1 %>%
  ggplot(aes(x = GDPHRS, 
             y = Country,
             color = destaca)) +
    geom_jitter(height = 0.15,
                width = 0.01,
                alpha = 0.6) +
    geom_point(aes(x = val20),
               size = 1.5) +
    geom_point(data = df_plot1 %>% 
                 filter(TIME == 2020),
               pch = 21,
               size = 4, 
               color = "midnightblue",
               fill = "midnightblue",
               alpha = 0.4) +
    geom_vline(xintercept = valor_medio,
               color = "midnightblue",
               alpha = 0.4) +
    geom_label(aes(x = valor_medio, 
                  y = 10.47, 
                  label = scales::dollar(valor_medio)),
               size = 2.5,
               color = "midnightblue") +
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Mexican Work Productivity Lags In OECD",
         subtitle = "Evolution Of GDP Per Hour Worked 1990 - 2020",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD. <i>Gross Domestic Product per hour worked 
         1990 - 2020. Selected countries. Last available values highlighted 
         in blue; median value indicated.</i> 
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") +
    scale_x_continuous(limits = c(0, 130),
                       labels = scales::dollar_format(),
                       breaks = c(0, 25, 50, 75, 100, 125)) +
    scale_color_manual(breaks = c(F, T),
                       values = c("#AAAAAA", "darkred"))

# Save plot
ggsave("wp_01", plot = wp_01, device = "tiff")

# Change in GDPHRS
# Min
gdp_min <- df_plot1 %>% 
  select(Country, TIME, GDPHRS) %>% 
  mutate(decade = 10 * (TIME %/% 10)) %>% 
  group_by(Country) %>% 
  slice_min(order_by = GDPHRS, with_ties = FALSE) %>% 
  mutate(min = GDPHRS)
  
# Max
gdp_max <- df_plot1 %>% 
  select(Country, TIME, GDPHRS) %>% 
  mutate(decade = 10 * (TIME %/% 10)) %>% 
  group_by(Country) %>% 
  slice_max(order_by = GDPHRS, with_ties = FALSE) %>% 
  mutate(max = GDPHRS)

# Percent of change
gdp_change <- gdp_min %>%
  inner_join(gdp_max, by = "Country") %>% 
  select(!c("GDPHRS.x", "decade.x", "GDPHRS.y", "decade.y")) %>% 
  mutate(change = scales::percent((max - min) / min, accuracy = 0.01)) %>% 
  arrange(desc(change))



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
    theme(text = element_text(family = "Optima"),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "GDP Per Hour Worked",
         subtitle = "Top 5 in USD Current PPPs",
         x = "USD Current PPPs",
         caption = "Source:OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::dollar_format())


# Bottom 5 Countries GDPHRS
df01 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME > 2016) %>% 
  group_by(TIME) %>%
  slice_min(order_by = GDPHRS, 
            n = 5, 
            with_ties = FALSE) %>% 
  ggplot(aes(x = GDPHRS,
             y = reorder_within(x = Country, 
                                by = GDPHRS, 
                                within = TIME))) + 
  geom_col(fill = "steelblue",
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
       subtitle = "Bottom 5 in USD Current PPPs",
       x = "USD",
       caption = "Source:OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")



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
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")



# Gross National Income per hour worked
df02 <- df00 %>% 
  filter(Subject == "Gross national income per hour worked") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value)

# Top 5 GNI per hour worked
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
    labs(title = "Gross National Income Per Hour Worked",
         subtitle = "Top 5 in USD Current PPPs",
         x = "USD per hour",
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")

# Bottom 5 GNI per hour worked
df02 %>% 
  filter(TIME > 2016) %>% 
  group_by(TIME) %>% 
  slice_min(order_by = GNIHRS,
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
       subtitle = "Bottom 5 in USD Current PPPs",
       x = "USD per hour",
       caption = "<i>Elaboración propia</i><br>Source: OECD")


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
  theme(axis.title.y = element_blank(),
        plot.caption = element_markdown(color = "darkgrey")) +
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
    scale_x_continuous(limits = c(1970, 2027.5)) +
    theme(legend.position = "none",
          plot.caption = element_markdown(color = "darkgrey"),
          plot.caption.position = "plot") +
    labs(title = "Gross National Income Per Hour Worked",
         subtitle = "Current USD ",
         caption = "<i>Elaboración propia</i><br>Source: OECD")


# Data frame for USD Current PPPs
df03 <- df00 %>% 
  filter(MEASURE == "PCTUS") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value)


# Bottom 5 GDP per hour worked AS PERCENT
df03 %>% 
  filter(TIME > 2016) %>% 
  group_by(TIME) %>% 
  slice_min(order_by = GDPHRS,
            n = 5, with_ties = FALSE) %>% 
  ggplot(aes(x = GDPHRS, 
             y = reorder_within(x = Country,
                                by = GDPHRS,
                                within = TIME))) + 
   geom_col(fill = "darkgreen",
            alpha = 0.7) +
   geom_text(aes(label = scales::percent(GDPHRS / 100)),
             hjust = 1.3,
             color = "grey85",
             size = 3) +
    facet_grid(rows = vars(TIME),
             scales = "free") +
    theme(axis.title.y = element_blank(),
          plot.caption = element_markdown(color = "darkgrey")) +
    scale_y_reordered() +
    labs(title = "GPD Per Hour Worked As Percent",
         subtitle = "Bottom 5",
         x = "Percent of US Value",
         caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")
  


# Evolution of GDP per Hour As Percent
df03 %>% 
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
  scale_x_continuous(limits = c(1970, 2027.5)) +
  theme(legend.position = "none",
        plot.caption = element_markdown(color = "darkgrey"),
        plot.caption.position = "plot") +
  labs(title = "Gross Domestic Product Per Hour Worked",
       subtitle = "Percent of US Value",
       x = "Year",
       caption = "Source: OECD<br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)")


df03 %>% 
  filter(TIME == 2020,
         Country %in% c("Mexico", "Ireland")) %>% 
  summarize(veces = pull(.[1, 6]) / pull(.[2, 6]),
            porcentual = ((pull(.[1, 6]) - pull(.[2, 6])) / pull(.[2, 6])) * 100,
            prop = (pull(.[2, 6]) * 100) / pull(.[1, 6]))
  

df03 %>% 
  filter(TIME == 2020,
         Country %in% c("Mexico", "Ireland")) %>% 
  ggplot(aes(x = Country, y = GDPHRS, fill = Country)) +
    geom_bar(stat = "identity", 
             position = "nudge") +
    theme(legend.position = "none")

















    

