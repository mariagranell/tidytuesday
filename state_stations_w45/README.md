Radio Stations
================

My first contribution to [tidy
tuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-08)!!
Even though there is plenty of room for improvement, I´ve had a lot of
fun creating this graph.

The data had information on radio stations across states and cities of
the U.S. The original post created a map with this information. In my
case, I got interested in how many radio stations used AM vs FM
frequencies. After a quick google, I learned that FM frequencies are
higher quality but are more expensive. I wanted to know if radio
stations in poorer states prefer to use AM frequencies.

So, I loaded the data from the [U.S. Department of
Commerce](https://apps.bea.gov/regional/downloadzip.cfm). I didn´t do
the stats, but as you can see in the graph, it doesn´t seem like there
is a relationship between the difference between radio stations with FM
or AM and the GDP per each state.

![graph](/Users/mariagranell/Repositories/tidytuesday/state_stations_w45/AMvsFM.png "AM vs FM")

Find at the end of this document the minimum code I needed to create the graph and the graph code. In the file `state_stations.R`, you can find all my data exploration:

``` r
#Directory
setwd("/Users/mariagranell/Repositories/tidytuesday/state_stations_w45")

#packages
library(ggplot2)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(tidytuesdayR) # important to load tidy tuesday
library(wesanderson) # nice colours
pal <- wes_palette("Zissou1")

# Select the data set of the week, ISO-8601 date or year/week works!
tuesdata <- tidytuesdayR::tt_load(2022, week = 45)
state_stations <- tuesdata$state_stations

#create a factor with AM and FM frequencies
state_stations$module <- ifelse(grepl('FM$', state_stations$frequency), "FM", "AM")
state_stations$module <- as.factor(state_stations$module)
state_stations$call_sign <- as.character(state_stations$call_sign)

#extract the number of the frequencies
state_stations$num_frequencies <- as.numeric(gsub("[ FMA]", "", state_stations$frequency))

#so apparently fm is straight better than am. and more expensive. but am covers more distance
#I would like to extract gdp per state and see amount of fm/am

#load the gdp of each state. I downloaded from here: https://apps.bea.gov/regional/downloadzip.cfm
ranking <-read.csv("SAGDP1__ALL_AREAS_1997_2021.csv")
ranking <- ranking[ranking$Description == "Real GDP (millions of chained 2012 dollars)  ",] # selection for only GDP data

#compare if we have the same number of states
ranking$GeoName <- as.factor(ranking$GeoName)
state_stations$state <- as.factor(state_stations$state)

#we have more states in the economical data. so when we pull we have to make sure is the same
paste(nlevels(ranking$GeoName), nlevels(state_stations$state))

#prepare for merging, create a state row to use for merging
ranking$state <- ranking$GeoName
ranking$gdp <- ranking$X2021
#merging both data frames
state_stations<-merge(state_stations, ranking[, c("gdp","state")], by="state", all.x= F)

#creating a percentage of am vs fm per state
df <-as.data.frame(
  state_stations %>%
  group_by(state,module)%>%
  summarise(cnt=n())%>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>%
  arrange(desc(freq)))

# adding it to the data frame
df<-merge(df, ranking[, c("gdp", "state")], by="state", all.x= F)

#create data frame to make the graph
# you need to create a difference between AM and FM
# I combined ifelse to remove ohio that only has AM. Then by grouping by state and asking df into the variable tada!
b<-df%>%
  group_by(state)%>%
  mutate(difference= ifelse(state == "Ohio", cnt, (diff(cnt)*-1)))
#but after merging we have to give Ohio a - becuase they only have AM stations
b[b$state== "Ohio","difference"] <- b[b$state== "Ohio","difference"]*-1

am <-b[b$module=="AM",]
data<-data.frame(gdp=(am$gdp)/1000, y=am$difference, x=am$state) #I divided gdp by 1000 so it´s smaller

#I´m going to add a fake data point at the end so i can paint the scale
data$x <- as.character(data$x) #you cannot add a row to factor when that level is not there
data[nrow(data)+1,] <- c(min(data$gdp)-0.1, 0, "")
data <-data%>%
  mutate(
  x= as.factor(x),
  y =as.numeric(y),
  gdp = as.numeric(gdp)
  )
```

The graph:

``` r
#graph
 plot <-ggplot(data, aes(y=reorder(x,gdp), x=y)) +
   geom_segment(
     aes(y=reorder(x,gdp), yend=reorder(x,gdp), # use reorder to put the states from +gdp to -gdp
         x= 0, #ifelse(x == "", -200, 0), # use ifelse to create segments of the legend on the graph. it was easier to do with segment
         xend= y),#ifelse(x == "", 200, y)), # use ifelse "
     color= "grey"#ifelse(data$x == "", "orange", "grey")
   ) +
   geom_point(size = 3,
              aes(x= ifelse(x == "", NA,y), # to not plot the dot of the fake row
                  color=gdp)) +
   theme_light() +
   theme(
     panel.grid.major.x = element_blank(),
     panel.border = element_blank(),
     axis.ticks.x = element_blank(),
     axis.text.y = element_text(size= 6),
     axis.text.x = element_text(size= 10),
     plot.title = element_text(face = "bold", size = 20, margin = margin(0, 0, 0, 0)),
     plot.subtitle = element_text(size = 12),
     plot.caption = element_text(margin = margin(10, 0, 0, 0))
   ) +
   labs(
     title = "AM vs FM",
     subtitle = "Difference in the number of radio stations with AM vs FM  \nfrequencies in relation to the wealth (GDP) of each state",
     caption = "Source: Wikipedia and U.S. Department of Commerce · Graphic: Maria Granell Ruiz"
   ) +
   xlab("Number of stations with FM frequencies minus AM frequencies") +
   ylab("U.S. States")+
   geom_vline(xintercept=0,lwd=0.5,colour="grey")+
   scale_color_gradient(low="orange", high=pal[5], "GDP divided \nby 1000")

plot +
  annotate("segment",
           x = -200,
           xend = 200,
           y = data[data$x == "","y"]+1, yend = data[data$x == "","y"]+1,
           arrow = arrow(ends= "both", length = unit(.2,"cm")))+
  annotate("text",
           x = -350,
           y = data[data$x == "","y"]+1,
           label = "More AM radios",
           size= 3)+
  annotate("text",
           x = 350,
           y = data[data$x == "","y"]+1,
           label = "More FM radios",
           size= 3)
```

![](/Users/mariagranell/Repositories/tidytuesday/state_stations_w45/README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
