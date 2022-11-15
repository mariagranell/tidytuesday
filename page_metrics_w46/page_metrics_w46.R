#----------
# title: page metrics
# author: Maria Granell Ruiz
# date: 15 nov 2022
# ---------

# packages -------------
library(tidyverse)
library(jsonlite)
library(tidytuesdayR)
# to make a RIDGELINE plot
library(ggridges)
library(ggplot2)
library(ggthemr) #cool theme
# Get the Data Manually---------------------

#ally scores
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')

#speed size
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')


#Creating a data frame with the 90p from bytes, speed and ally----------
mixdf <- ally_scores[,  "date"]
mixdf$ally <- ally_scores$p90

#merge only one collumn to a df in relation to date!
mixdf <- merge(mixdf, bytes_total[, c("date","p90")], by="date", all=T)
mixdf <- merge(mixdf, speed_index[, c("date","p90")], by="date", all=T)

#rename the columns
colnames(mixdf) <- c("date", "ally", "bytes", "speed")

#scale the data around the mean so we can compare how they change over time
mixdf <- mixdf%>%
  mutate(Ally = scale(ally),
         Speed = scale(speed),
         Bytes = scale(bytes))

#transform the data frame from wide to long
mix_long <- gather(mixdf, "Measurment", "p90", Ally, Bytes, Speed)

#create the column year (by recognizing the date as date
mix_long$year <- format(as.Date(mix_long$date, format="%Y_%m_%d"),"%Y")

#theme!
ggthemr('solarized', type = 'outer', layout = "minimal", spacing =2)

# Ridge plot ---------
rp_ally <-
  ggplot(mix_long, aes(x= p90, y= year, fill=  Measurment))+
  geom_density_ridges_gradient(scale=3, rel_min_height = 0.01)+
  labs(title= "Webpages metrics over the years",
       subtitle = str_wrap("Webpages have become more complex over the years.
       The overall number of bytes displayed seems to have increased almost linearly,
       a pattern followed by ally scores and the speed indexes", 80),
       caption = "Source: hhtparchive.org · Graphic: Maria Granell Ruiz",
       y = "Year",
       x = "Scaled percentile 90")

rp_ally +
  theme(
    #text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=20), #change font size of axis titles
        plot.title=element_text(size=30), #change font size of plot title
        plot.subtitle=element_text(size= 12), #change font size of subtitle
        plot.caption=element_text(size= 10), #change font size of caption
        legend.text=element_text(size=17), #change font size of legend text
        legend.title=element_text(size=18), #change font size of legend title

    #leyend
    legend.position = c(0.85,0.15) #where to put the leyend in the plot
  )


