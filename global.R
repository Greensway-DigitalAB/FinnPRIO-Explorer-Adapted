v <- numeric_version("0.0.1")
library(shiny)
library(shinythemes)
library(shinyhelper)
library(shinycssloaders)
library(tidyverse)
# library(dplyr)
library(data.table)
library(DT)
# library(ggplot2)
library(gghighlight)
library(treemapify)


# source("functions.R")

# The colors used in the Plot and in the Tables are from the palette of Ruokavirasto (2022).

# Read data tables:
cleanfinnprioresults <- fread("data/cleanfinnprioresults.csv") # "cleanfinnprioresults.csv" contains FinnPRIO scores./It is used in tabs 1. Plot pests on a graph and 2. Show pests in data table/ The pest's names are used in tab 3. Compare pests by questions
pestquestions <- fread("data/pestquestions_est3.csv")          # "pestquestions_est3.csv" contains FinnPRIO assessments. Each pest is in a column!/ It is used in tab 3. Compare pests by questions
hv <- fread("data/hv.csv")                                     # "hv.csv" contains FinnPRIO hypervolume scores./It is used in tab 4. Rank pests

