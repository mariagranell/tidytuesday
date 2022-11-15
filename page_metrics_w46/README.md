Web page metrics
================

For this week, I investigated data about [web page metrics](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-15). As expected, web pages are getting increasingly complex as the usage increase and technology develops. A problem that developers are dealing with nowadays to make sure that the loading speed is reasonable and that our devices can still load them; further discussion in: ["Why web pages can have a size problem"](https://blog.datawrapper.de/why-web-pages-can-have-a-size-problem/)

There were several measurements. I plotted the variables whose values were presented in percentiles, i.e. `ally_scores`, `bytes total` and `speed_index`, and I selected the [cumulative 90 percentile](https://www.dnv.com/article/terminology-explained-p10-p50-and-p90-202611). Maybe an overestimation of the distribution, but I wanted the most data. To be able to compare between measurements, I [*scaled*](https://en.wikipedia.org/wiki/Standard_score) the values and focused on the change in each variable over the years.

I wanted to do a ridge plot for the first time, though I would have used regression lines for the sake of data investigation. I wonder if the differences in densities seen in Speed and Ally are driven by Computer vs Phone displays. A future project?

I also used ggthemr package for the first time. I will keep using ggthemer though I don´t understand how to modify the plot parameters very well.

![graph](https://github.com/mariagranell/tidytuesday/blob/main/page_metrics_w46/webpagemetrics.png)

Find at the end of this document the minimum code I needed to create the graph and the graph code. In the file `page _metrics_w46.R`, you can find all my data exploration:

```r
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
```