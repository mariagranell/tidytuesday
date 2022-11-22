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
library(ggthemr) #cool theme
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
                                      ifelse(data$Size == "medium", 2, "NA"))))
# remove values with na
na.omit(data)

# make the years and int
data$year_closed <- as.integer(data$year_closed)
data$year_open <- as.integer(data$year_open)

# see if the data is cooler from the 2000
data <- data[data$year_open > 1999,]

# colors
col_dots <- "white"
col_background <- "#11344F"

# static ggplot
static <- ggplot() +
  geom_polygon(data = uk, aes(x=long, y=lat, group= group), fill="grey", alpha = 0.3) +
  geom_point(data = data, aes(x=Longitude, y= Latitude, size = Size, color= Size, alpha = Size)) +
  scale_size_continuous(name= "Museum size", breaks = c(1,2,3), labels = c("Small","Medium","Large")) +
  scale_alpha_continuous(name =  "Museum size", breaks = c(1,2,3), labels = c("Small","Medium","Large"), range = c(0.3,0.05)) +
  scale_color_gradientn(name = "Museum size", breaks =c(1,2,3), labels = c("Small","Medium","Large"), colours = col_dots) +
  ylim(50,59)+
  coord_map() + # make a map
  labs(
     title = "Uk museums over time",
     subtitle = "Location of the different museums of the Uk, when they \noppened and which size they were
     \n year",
     caption = "Source: · Graphic: Maria Granell Ruiz"
     #  tag = "WHATAFUCK"
  ) +
  theme_void() +
  guides( colour = guide_legend()) + #to have one legend instead of two
  theme(legend.position = c(1,0.7),
        text = element_text(colour= col_dots),
        plot.background = element_rect(fill=col_background, color = col_background),
        panel.background = element_rect(fill=col_background, color = col_background),
        legend.background = element_rect(fill=col_background, color = col_background),
        plot.title = element_text(size = 20, face="bold"),
        plot.subtitle = element_text(size= 12)
       # plot.tag = element_text()
  )

#ggsave("test.png", bg = col_background, h = 5)
static
# animated plot --------------
in_len <- as.integer(1)
static + transition_events(start = year_open, end = year_closed, enter_length = in_len, exit_length = in_len) +
  labs(subtitle = "Location of the different museums of the Uk, when they \noppened and which size they were
     \n Year: {round(frame_time, 0)}")
