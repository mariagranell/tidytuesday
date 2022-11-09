#Directory
setwd("/Users/mariagranell/Repositories/tidytuesday/state_stations_w45")

#packages
library(ggplot2)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(wesanderson) # nice colours
library(tidytuesdayR) # important to load tidy tuesday
pal <- wes_palette("Zissou1")

# Select the data set of the week, ISO-8601 date or year/week works!
tuesdata <- tidytuesdayR::tt_load(2022, week = 45)

state_stations <- tuesdata$state_stations
state_stations %>%
  group_by(state, format)%>%
  tally()

state_stations %>%
  ggplot(aes( x=frequency))+
  geom_histogram(stat="count")

#create a factor with AM and FM frequencies
state_stations$module <- ifelse(grepl('FM$', state_stations$frequency), "FM", "AM")
state_stations$module <- as.factor(state_stations$module)
state_stations$call_sign <- as.character(state_stations$call_sign)

state_stations %>%
  group_by(state,module)%>%
  summarise(n_module=n())%>%
  ggplot(aes(x=state, y=n_module, fill= module))+
  geom_bar(stat="identity",position = position_dodge())

#extract the number of the frequencies
state_stations$num_frequencies <- as.numeric(gsub("[ FMA]", "", state_stations$frequency))
# make the cool violin graph stuff

#plotting them didn´t reveal much
state_stations[state_stations$module == "AM",]%>%
  ggplot(aes(x=num_frequencies))+
  geom_histogram(bins=30)

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
#checking the gdp spread
df%>%
  ggplot(aes(x=state, y=gdp))+
  geom_bar(stat="identity")

#creating a percentage of am vs fm per state
df <-as.data.frame(
  state_stations %>%
  group_by(state,module)%>%
  summarise(cnt=n())%>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>%
  arrange(desc(freq)))

# adding it to the data frame
df<-merge(df, ranking[, c("gdp", "state")], by="state", all.x= F)

df %>%
  ggplot(aes(x=gdp, y=freq, colour=module))+
  #geom_point()
  geom_bar(stat= "identity")

#create data frame to make the graph
# you need to create a difference between AM and FM
# I combined ifelse to remove ohio that only has AM. Then by grouping by state and asking df into the variable tada!
b<-df%>%
  group_by(state)%>%
  mutate(difference= ifelse(state == "Ohio", cnt, (diff(cnt)*-1)))
#but after merging we have to give Ohio a -
b[b$state== "Ohio","difference"] <- b[b$state== "Ohio","difference"]*-1

am <-b[b$module=="AM",]
data<-data.frame(gdp=(am$gdp)/1000, y=am$difference, x=am$state) #I divided gdp by 1000 so it´s smaller

#I´m going to add a fake data point at the end so i can paint the scale
data$x <- as.character(data$x) #you cannot add a row ti factor when that level is not there
data[nrow(data)+1,] <- c(min(data$gdp)-0.1, 0, "")
data <-data%>%
  mutate(
  x= as.factor(x),
  y =as.numeric(y),
  gdp = as.numeric(gdp)
  )

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
     caption = "Source: Wikipedia and U.S. Department of commerce · Graphic: Maria Granell Ruiz"
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


