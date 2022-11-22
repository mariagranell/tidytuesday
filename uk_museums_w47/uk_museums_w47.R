#----------
# title: uk museums
# author: Maria Granell Ruiz
# date: 22 nov 2022
# ---------

setwd("/Users/mariagranell/Repositories/tidytuesday/uk_museums_w47/")


# packages -------------
library(tidyverse)
library(jsonlite)
library(tidytuesdayR)
# make the plot
library(ggplot2)
library(ggthemr) #cool theme


# Get the Data Manually---------------------
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

