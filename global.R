library(shiny)
library(shinythemes)
library(plotly)
library(googleVis)
library(tidyverse)
library(DT)

clean = read.csv("./data/Energy/clean11.csv")
clean$X.1=NULL
