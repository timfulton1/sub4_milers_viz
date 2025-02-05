## Load packages and data ------------------------------------------------------

# Packages
library(readr)  # reading 
library(dplyr)  # cleaning
library(tidyr)  # cleaning
library(glue)  # cleaning
library(janitor)  #cleaning
library(lubridate)  # cleaning dates and times
library(forcats)  # for factors
library(ggplot2)  # plotting
library(ggtext)  # plotting
library(showtext)  # plotting with fonts
library(patchwork)  # plotting

# Data
milers_raw <- read_csv("data/sub4_data.csv", col_names = c("name", "time", "location", "date", "extra"))
universities <- read_csv("data/universities.csv", col_names = c("university"))
facet_colors <- read_csv("data/facet_colors.csv")

## Cleaning --------------------------------------------------------------------
# Separate text in columns (will get some warnings but they are OK)
milers <- milers_raw %>% 
  separate(name, c("year", "name"), "\\.") %>% 
  separate(name, c("name", "affiliation"), "\\(") %>% 
  separate(time, c("time", "place"), "\\ ") 

# Sub unneeded characters with "" to remove them
milers$affiliation <-  gsub("\\)", "", milers$affiliation)
milers$time <-  gsub("\\i", "", milers$time)
milers$time <-  gsub("\\*", "", milers$time)

# Fill years and remove unwanted columns
milers <- milers %>% 
  mutate(year = as.numeric(year)) %>%  # convert to year to use ifelse in next step
  mutate(year = ifelse(year < 1000, NA, year)) %>%  # keep the year values and put NA for anything else
  mutate(year = as.numeric(year)) %>% # convert to numeric to use fill in next step
  fill(year) %>%  # fill year down
  filter(!is.na(name))  %>%  # filter out rows with NA in name
  select(-place) 

# Append a 0 to the hundredths place for times that have only tenths
milers$time <- gsub("(\\.\\d{1}$)", "\\1\\0", milers$time)

# Function to convert time string to seconds
time_to_seconds <- function(time_str) {
  
  time_components <- strsplit(time_str, "[:.]")[[1]]
  minutes_to_seconds <- as.numeric(time_components[1]) * 60
  seconds_to_seconds <- as.numeric(time_components[2])
  milliseconds_to_seconds <- as.numeric(time_components[3]) / 100
  total_seconds <- minutes_to_seconds + seconds_to_seconds + milliseconds_to_seconds
  
  return(total_seconds)
}

# Apply the time conversion function to each element of the time column
milers$seconds <- sapply(milers$time, time_to_seconds)

# Create a new column for decades
milers$decade <- (milers$year %/% 10) * 10

# Cleaned data frame
milers_cleaned <- milers


## Prep for figures -------------------------------------------------------------
# Create a new data frame with running count
running_count <- milers_cleaned %>%
  group_by(year, affiliation) %>%
  summarise(count = n()) %>%
  group_by(affiliation) %>%
  mutate(running_count = cumsum(count)) %>%
  select(-count)

# Create a new data frame with only universities as there are some professional athletes and we want to exclude those
running_count_uni <- running_count %>% 
  filter(affiliation %in% universities$university)


# For loop to add future years if a university's last sub 4 was before 2023. This is so each affiliation ends at 2023.
# Initialize df with years and empty df to use in the loop
years_df <- tibble(year = 1957:2023)
running_count_uni_temp <- data.frame()

# Run for loop
for(school in universities$university){
  
  # filter by school
  running_count_uni_filtered <- running_count_uni %>% 
    filter(affiliation == school)
  
  # create df with all years
  temp_years <- years_df %>% 
    left_join(running_count_uni_filtered, by = "year") %>% 
    fill(running_count) %>% 
    fill(affiliation) %>% 
    filter(!is.na(affiliation))
  
  # bind rows together in main data frame
  running_count_uni_temp <- rbind(running_count_uni_temp, temp_years)
  
}

# Only include unis with 5 or more milers to simplify the figure, 
# add max counts, and title labels and colors for facet_plot
running_count_uni_min5 <- running_count_uni_temp %>% 
  group_by(affiliation) %>%
  filter(max(running_count) > 4) %>% 
  mutate(max_count = max(running_count),
         facet_title_label = glue("{affiliation} ({max_count})")) %>% 
  left_join(facet_colors)


## Create line plot ------------------------------------------------------------

# Add fonts
font_add_google("Source Sans 3")
showtext_auto()

# Plotting
sub4_milers_plot <- running_count_uni_min5 %>% 
  ggplot(mapping = aes(x = year, y = running_count, group = factor(affiliation))) +
  theme_classic() +
  theme(
    text = element_text(family = "Source Sans 3"),
    plot.title = element_markdown(size = 22, hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, hjust = 0, margin = margin(b = -2)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
    axis.title = element_blank(), 
    axis.text.x = element_text(size = 14, color = "black", margin = margin(t = -2)),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "<span style = 'color:#154733;'>**Oregon**</span> is Miler U", 
    subtitle = "Number of sub-4 milers by team (minimum 5)",
  ) +
  geom_line(
    color = "gray50", 
    linewidth = 1.1
  ) +
  geom_line(
    inherit.aes = FALSE,
    data = running_count_uni_min5 %>% filter(affiliation == "Oregon"), 
    mapping = aes(x = year, y = running_count),
    color = "#154733",
    linewidth = 2.2
  ) +
  geom_point(
    inherit.aes = FALSE,
    data = running_count_uni_min5 %>% filter(affiliation == "Oregon" & year == 2023), 
    mapping = aes(x = year, y = running_count),
    color = "#154733",
    size = 10
  ) +
  geom_text(
    inherit.aes = FALSE,
    data = running_count_uni_min5 %>% filter(affiliation == "Oregon" & year == 2023),
    mapping = aes(x = year, y = running_count, label = running_count),
    size = 5,
    color = "white",
    fontface = "bold"
  ) + 
  scale_x_continuous(
    limits = c(1957, 2023), 
    breaks = c(1957, seq(1960, 2020, 10), 2023),
    labels = c(1957, "'60", "'70", "'80", "'90", "'00", "'10", "'20", 2023),
  )



## Facet Plot ------------------------------------------------------------------

# Add fonts
font_add_google("Source Sans 3")
showtext_auto()

# Function for plotting
custom_facet_plot <- function(school){
  
  temp_data <- running_count_uni_min5 %>% filter(affiliation == school)
  
  ind_plot <- ggplot(data = temp_data, mapping = aes(x = year, y = running_count, group = factor(facet_title_label), color = factor(hex_color))) +
    theme_classic() +
    theme(
      text = element_text(family = "Source Sans 3"),
      plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 0), color = temp_data$hex_color),
      axis.title = element_blank(), 
      axis.text.x = element_text(size = 8, color = "black", margin = margin(t = -2)),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
      legend.position = "none",
      panel.spacing = unit(1, "lines"),
      strip.background = element_blank(),
      strip.text = element_markdown(size = 9, face = "bold")
    ) +
    labs(subtitle = temp_data$facet_title_label) +
    scale_x_continuous(
      limits = c(1957, 2023), 
      breaks = c(1957, 2023),
      labels = c(1957, 2023),
      expand = expansion(add = c(3, 3))
    ) +
    scale_y_continuous(
      limits = c(0, 44), 
      breaks = c(0, 10, 20, 30, 40)
    ) + 
    geom_ribbon(
      inherit.aes = FALSE, aes(x = year, ymin = 0, ymax = running_count, fill = hex_color),
      alpha = 0.5
    ) +
    geom_line(
      linewidth = 0.8
    ) + 
    scale_color_identity() +
    scale_fill_identity()

  return(ind_plot)
}

custom_caption_plot <- function(school){
  
  temp_data <- running_count_uni_min5 %>% filter(affiliation == school)
  
  ind_plot <- ggplot(data = temp_data, mapping = aes(x = year, y = running_count, group = factor(facet_title_label), color = factor(hex_color))) +
    theme_void() +
    theme(plot.caption = element_text(size = 8, face = "italic", hjust = 1, margin = margin(t = 5))) +
    labs(caption = c("Data: Track & Field News\nViz: Tim Fulton, PhD"))
  
  return(ind_plot)
}


# Add the plots together
sub_4_facet_plot <- sub4_milers_plot / plot_spacer() / (custom_facet_plot("Oregon")  + 
                                          custom_facet_plot("Stanford") + 
                                          custom_facet_plot("Georgetown") + 
                                          custom_facet_plot("Indiana") + 
                                          custom_facet_plot("Washington") +
                                          custom_facet_plot("Arkansas") + 
                                          custom_facet_plot("Colorado") + 
                                          custom_facet_plot("Virginia") + 
                                          custom_facet_plot("BYU") + 
                                          custom_facet_plot("Cal") + 
                                          custom_facet_plot("Texas") + 
                                          custom_facet_plot("Wisconsin") + 
                                          custom_facet_plot("Michigan") + 
                                          custom_facet_plot("Penn") + 
                                          custom_facet_plot("Princeton") + 
                                          custom_facet_plot("Villanova") + 
                                          custom_facet_plot("Mississippi") + 
                                          custom_facet_plot("Duke") + 
                                          custom_facet_plot("Kentucky") + 
                                          custom_facet_plot("Northeastern") +
                                          custom_facet_plot("Oklahoma") + 
                                          custom_facet_plot("Syracuse") + 
                                          custom_facet_plot("UCLA") +
                                          custom_caption_plot("Oregon") +
                                          plot_layout(ncol = 6, axes = "collect")) + 
  plot_layout(heights = c(7, 0.3, 7))


# save facet plot
ggsave("plots/sub_4_facet_plot.png",
       plot = sub_4_facet_plot,
       scale = 1,
       width = 11,
       height = 14.3,
       dpi = 600,
       units = "in")
