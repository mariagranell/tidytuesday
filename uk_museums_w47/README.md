# UK museums over time

For this week I decided to do an animated graph! I visually investigated which years museums opened and 
closed in the UK from the 1800 until now. I used the data from [MuseWeb by way of Data Is Plural](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-22)
available in the Github of [Tidy Tuesday week 47](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-22)


Because so many opened in the last recent years and not that many closed it seems like 
there´s only museums closing from year 2000 when it seems like there´s a 
more even opening- closing- rate. Besides plotting which period of time they were open
and the location within the UK, the dot size represents the size of the museum.

To create this graph I had several references,
firstly I followed the recommendations of the [Rgallery](https://r-graph-gallery.com/bubble-map.html) to create my first map plot, 
secondly I animated my plot following this [blog](https://www.alexcookson.com/post/2020-10-18-building-an-animation-step-by-step-with-gganimate/) , 
lastly I got visually inspired by this [plot](https://erdavis.com/2020/01/04/visualizing-the-geography-of-fm-radio/) about radio stations.

Here you have the static graph, go to my twitter to find the gif, and below for the graph code!

!()[]

```
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
   xlim(-8.4,10.4) + # How on the side do I want it? you can also change that with plot.margins in theme
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
animated <- static + transition_events(start = year_open, end = year_closed, enter_length = in_len, exit_length = in_len) +
  labs(subtitle = "Year: {round(frame_time, 0)}")+
  theme(
    plot.subtitle = element_text(size= 18, hjust = 0.6, vjust = -42)
  )

# save, it will save it as a collection on images,
# when uploading it to twitter is animated
anim_save("animated.gif", animated)

```




