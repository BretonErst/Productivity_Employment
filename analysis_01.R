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



## Data acquisition
df00 <- read_csv("gdp_pc_productivity.csv")

## Code for measure definition
code_subject <- df00 %>% 
  select(SUBJECT, Subject, MEASURE, Measure, Unit, PowerCode) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  unique()

# Distinct instances of time
df00 %>% 
  distinct(Time)

# Distinct instances of Country 
df00 %>% 
  distinct(Country)


## Hours Worked Annually
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
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Mexican Workers Spend The Most Time On The Job",
         subtitle = "Top 10 Countries Hours Worked Per Person",
         x = "Hours worked annually per person",
         y = NULL,
         caption = "Source: OECD Stats.
         <i>Average hours worked annually per person employed year 2020
         </i><br>Visualization: Juan L. Bretón, PMP 
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::label_number()) -> wp_02
  
# Plot save
ggsave(filename = "wp_02", plot = wp_02, device = "tiff")


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
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Where Do People Work The Most Hours?",
         subtitle = "Hours Worked Per Person, Top 3 Countries Per Year",
         x = "Hours worked annually per person",
         y = NULL,
         caption = "Source: OECD Stats. <i>Hours worked per person employed
         20217 - 2020.</i> 
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") +
    scale_y_reordered() +
    scale_x_continuous(labels = scales::label_number()) -> wp_03

# Save plot
ggsave(filename = "wp_03", plot = wp_03, device = "tiff")




## Data frame for USD Current PPPs
df01 <- df00 %>% 
  filter(MEASURE == "CPC") %>% 
  select(-c(Subject, MEASURE, Measure, Time, `Unit Code`, `PowerCode Code`,
            `Reference Period Code`, `Reference Period`, PowerCode,
            Unit, `Flag Codes`, Flags)) %>% 
  mutate(SUBJECT = str_match(SUBJECT, "T.(\\w*)")[ , 2]) %>% 
  pivot_wider(names_from = SUBJECT, 
              values_from = Value)


# Plot of Evolution of GDP per hour worked
df01 %>% 
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain", "Costa Rica")) %>% 
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
    theme(text = element_text(family = "Optima"),
          legend.position = "none",
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Evolution Of Work Productivity",
         subtitle = "Gross Domestic Product Per Hour Worked",
         x = NULL,
         y = "USD current PPPs",
         caption = "Source: OECD Stats. <i>Gross Domestic Product Per Hour Worked
         1970 - 2020. Selected countries.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") +
    scale_y_continuous(labels = scales::dollar_format()) -> wp_04

# Save plot
ggsave(filename = "wp_04", plot = wp_04, device = "tiff")


## Spread of GDP per hour worked
# Preparation of data frame
df_spread <- df01 %>%
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain", "Germany",
                        "Costa Rica", "Korea", "Colombia")) %>%
  filter(TIME >1989) %>% 
  mutate(val20 = ifelse(TIME == 2020, GDPHRS, NA),
         destaca = (Country == "Mexico"))
  
# Median value GDPHRS 2020
valor_medio <- df_spread %>% 
  summarize(media = median(val20, na.rm = TRUE)) %>% 
  pull()


# Visualization of Spread of GDPHRS
df_spread %>%
  ggplot(aes(x = GDPHRS, 
             y = Country,
             color = destaca)) +
    geom_jitter(height = 0.15,
                width = 0.01,
                alpha = 0.6) +
    geom_point(aes(x = val20),
               size = 1.5) +
    geom_point(data = df_spread %>% 
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
                   y = 11.49, 
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
         subtitle = "Spread Of GDP Per Hour Worked 1990 - 2020",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD Stats. <i>Gross Domestic Product per hour worked 
         1990 - 2020. Selected countries. Last available values highlighted 
         in blue; median value indicated in blue.</i> 
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") +
    scale_x_continuous(limits = c(0, 130),
                       labels = scales::dollar_format(),
                       breaks = c(0, 25, 50, 75, 100, 125)) +
    scale_color_manual(breaks = c(F, T),
                       values = c("#AAAAAA", "darkred")) -> wp_01

# Save plot
ggsave("wp_01", plot = wp_01, device = "tiff")


## Change in GDPHRS
# Min value
gdp_min <- df_spread %>% 
  select(Country, TIME, GDPHRS) %>% 
  mutate(decade = 10 * (TIME %/% 10)) %>% 
  group_by(Country) %>% 
  slice_min(order_by = GDPHRS, with_ties = FALSE) %>% 
  mutate(min = GDPHRS)
  
# Max value
gdp_max <- df_spread %>% 
  select(Country, TIME, GDPHRS) %>% 
  mutate(decade = 10 * (TIME %/% 10)) %>% 
  group_by(Country) %>% 
  slice_max(order_by = GDPHRS, with_ties = FALSE) %>% 
  mutate(max = GDPHRS)

# Percent of change
gdp_change <- gdp_min %>%
  inner_join(gdp_max, by = "Country") %>% 
  select(!c("GDPHRS.x", "decade.x", "GDPHRS.y", "decade.y")) %>% 
  mutate(dife = max - min,
         change = scales::percent((max - min) / min, 
                                  accuracy = 0.01)) %>% 
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
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "The Most Productive Work Systems",
         subtitle = "Top 5 Countries",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD. <i>Gross domestic product per hour worked
         2017 - 2020</i><br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::dollar_format()) -> wp_05

# Save plot
ggsave("wp_05", plot = wp_05, device = "tiff")


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
    geom_col(fill = "darkred",
             alpha = 0.65) +
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
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "The Least Productive Work Systems",
         subtitle = "Bottom 5 Countries",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD. <i>Gross domestic product per hour worked
         2017 - 2020</i><br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::dollar_format()) -> wp_06

# Save plot
ggsave("wp_06", plot = wp_06, device = "tiff")


# Plot Work productivity 2020
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
    theme(text = element_text(family = "Optima"),
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
        plot.caption.position = "plot") +
    labs(title = "Productivity Of Work Systems",
         subtitle = "Comparison in USD Current PPPs",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD. <i>Gross domestic product per hour worked
         1970 - 2020</i><br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::dollar_format()) -> wp_07

# Save plot
ggsave("wp_07", plot = wp_07, device = "tiff")



## Gross National Income per hour worked
# Data frame
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

# 2020 GNI per hour
df02 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME == 2020) %>% 
  drop_na(GNIHRS) %>%
  ggplot(aes(x = GNIHRS,
             y = fct_reorder(Country, GNIHRS))) + 
    geom_col(fill = "#2471A3",
             alpha = 0.7) +
    geom_text(aes(label = paste("$", 
                                format(round(GNIHRS, 2), 
                                       big.mark = ",")),
                  hjust = 1.3),
              color = "grey85",
              size = 2) +
    theme(text = element_text(family = "Optima"),
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "How Much Workers Make Per Hour?",
         subtitle = "Comparison in USD Current PPPs",
         x = "USD Current PPPs",
         y = NULL,
         caption = "Source: OECD. <i>Gross national income per 
         hour worked 2020</i><br>Visualization: Juan L. Bretón, PMP
         (@BretonPmp)") +
    scale_x_continuous(labels = scales::dollar_format())


# iPhone Index (8 hour work journey)
df02 %>% 
  filter(!LOCATION %in% c("OECD", "BRIICS", "G-7", 
                          "EU27_2020", "EA19")) %>% 
  filter(TIME == 2020) %>% 
  drop_na(GNIHRS) %>%
  mutate(iPhone_index = (999 / GNIHRS) / 8) %>% 
  ggplot(aes(x = iPhone_index,
             y = fct_reorder(Country, iPhone_index))) + 
    geom_point(color = "#2471A3",
             alpha = 0.7,
             size = 2) +
    geom_segment(aes(x = 0,
                     xend = iPhone_index,
                     yend = fct_reorder(Country, iPhone_index)),
                 color = "#2471A3",
                 alpha = 0.7) +
    geom_text(aes(label = format(round(iPhone_index, 1), 
                                 big.mark = ","),
                  hjust = -0.8),
              color = "grey45",
              size = 3.0) +
    theme(text = element_text(family = "Optima"),
          panel.background = element_rect(fill = "#F9F9F9"),
          panel.grid.major.y = element_blank(),
          axis.line = element_line(color = "darkgrey"),
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "How Much Working Time Is Necessary To Buy An iPhone?",
         subtitle = "Number of 8-hour-work journeys",
         x = "8-hour-work journey",
         y = NULL,
         caption = "Source: OECD. <i>Gross national income per 
         hour worked 2020. Values in 
         USD Current PPPs. Iphone 12 priced at USD $999.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") +
    scale_x_continuous(limits = c(0, 6)) -> wp_08

# Save plot
ggsave("wp_08", plot = wp_08, device = "tiff")


# Evolution of Gross Income per Hour
df02 %>% 
  filter(Country %in% c("Mexico", "United States",
                        "Canada", "Chile", "Ireland",
                        "Norway", "Spain", "Switzerland",
                        "Korea", "Russia")) %>% 
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
    theme(text = element_text(family = "Optima"),
          panel.background = element_rect(fill = "#F9F9F9"),
          panel.grid.major.x = element_blank(),
          axis.line = element_line(color = "darkgrey"),
          legend.position = "none",
          plot.title.position = "plot",
          plot.caption = element_markdown(color = "darkgrey",
                                          hjust = 0),
          plot.caption.position = "plot") +
    labs(title = "Evolution Of Gross National Income Per Hour Worked",
         subtitle = "Comparable USD from 1970 to 2020",
         x = NULL,
         y = "USD current PPPs",
         caption = "Source: OECD. <i>Gross national income per 
         hour worked 1970 - 2020. Figures in USD PPPs. Selected countries.</i>
         <br>Visualization: Juan L. Bretón, PMP (@BretonPmp)") -> wp_09

# Save plot
ggsave("wp_09", plot = wp_09, device = "tiff")


# Data frame for as percent
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

















    

