#----------
# title: uk museums
# author: Maria Granell Ruiz
# date: 22 nov 2022
# ---------

setwd("/Users/mariagranell/Repositories/tidytuesday/uk_museums_w47/")


# packages -------------
library(tidyverse)
library(tidytuesdayR)
# make the plot
library(ggplot2)
library(ggthemes) # has a theme for maps
# plot a map
library(maps)
# moving graph
library(gganimate)
library(gifski) # package for gif output
# library(av) # package for video output


# Get the Data Manually ---------------------
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# Explore years years --------------
museums$Year_opened[1]
# regex explained, select the : and all number (\\d, notice that in R you have to put two \\) at the end of the text ($)
museums$year_open <- as.numeric(gsub(":\\d{4}$", "", x=museums$Year_opened)) # gsub selects the end of the string and replaces it with nothing
museums$year_closed <- as.numeric(gsub(":\\d{4}$", "", x=museums$Year_closed)) # gsub selects the end of the string and replaces it with nothing

# museums that are still open put current date
museums$year_closed <- as.numeric(gsub("9999","2022", museums$year_closed))

# Map plot -------------

# load the uk map
uk <- map_data("world") %>%
  filter(region == "UK")

# data with the open and closing year, longitude, latutide and size of the museums
data <- museums[,c("year_open", "year_closed", "Latitude", "Longitude", "Size")]
data$Size <- as.numeric(ifelse(data$Size == "large", 3,
                               ifelse(data$Size == "small", 1,
                                      ifelse(data$Size == "medium", 2,
                                             ifelse(data$Size == "huge",4, "NA")))))
# remove values with na
na.omit(data)

# make the years and int
data$year_closed <- as.integer(data$year_closed)
data$year_open <- as.integer(data$year_open)

# see if the data is cooler from the 2000
data <- data[data$year_open > 1800,]

# colors
col_dots <- "white"
col_background <- "#11344F"

# static ggplot
static <- ggplot() +
  geom_polygon(data = uk, aes(x=long, y=lat, group= group), fill="grey", alpha = 0.3) +
  geom_point(data = data, aes(x=Longitude, y= Latitude, size = Size, color= Size, alpha = Size)) +
  scale_size_continuous(name= "Museum size", breaks = c(1,2,3,4), labels = c("Small","Medium","Large","Huge")) +
  scale_alpha_continuous(name =  "Museum size", breaks = c(1,2,3,4), labels = c("Small","Medium","Large","Huge"), range = c(0.3,0.05)) +
  scale_color_gradientn(name = "Museum size", breaks =c(1,2,3,4), labels = c("Small","Medium","Large","Huge"), colours = col_dots) +
  ylim(50,60.5)+# How high do i want the graph?
   xlim(-8,10) + # How on the side do I want it? you can also change that with plot.margins in theme
  coord_map(projection = "mercator") + # make a map, mercator is for nice scale
  labs(
     subtitle = "Year",
  ) +
  theme_void() +
  guides( colour = guide_legend()) + #to have one legend
   annotate("text", x = 5, y = 60, size = 7.5, fontface = "bold", hjust = 0.5, color = "white",
            label = "Uk museums over time") +
   annotate("text", x = 0, y = 59, size = 4.5, fontface = "bold", hjust = 0, color = "white",
            label = "Location of the open Uk museums
             \nin relation to the year of opening and
             \nclosing, dot size represents museum size.",
   lineheight = 0.5) + #line light says how close the lines are withing the text
   annotate("text", x = 10, y = 50, size = 3, fontface = "bold", hjust = 1, color = "white",
            label = "Source: MuseWeb #TidyTuesday week 47· Graphic: Maria Granell Ruiz") +

  theme(legend.position = c(0.8,0.5),
        plot.margin = margin(0, 0.5, 0, 0, "cm"),
        text = element_text(colour= col_dots, size = 18),
        plot.background = element_rect(fill= col_background, color = NA),
        panel.background = element_rect(fill= col_background, color = NA),
        legend.background = element_rect(fill= col_background, color = NA),
        plot.subtitle = element_text(size= 18, hjust = 0.6, vjust = -42)
  )

#ggsave("test.png", bg = col_background, h = 5)
static
# animated plot --------------
in_len <- as.integer(1)
static + transition_events(start = year_open, end = year_closed, enter_length = in_len, exit_length = in_len) +
  labs(subtitle = "Year: {round(frame_time, 0)}")+
  theme(
    plot.subtitle = element_text(size= 18, hjust = 0.6, vjust = -42)
  )

