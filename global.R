
# 1. PACKAGES ----------------------------------------------------------------

# detach("package:plyr", unload=TRUE) must be unloaded : unwanted interference with {dplyr}

library(scroller) 
# to install : remotes::install_github("lgnbhl/scroller")

library(shiny)
library(shinydashboard)

library(shinyBS) # Note : package not maintaned anymore
library(shinycssloaders) 
library(shinyjs) 
library(shinyWidgets) 

library(stringr)
library(lubridate)
library(dplyr)
library(highcharter)
library(data.table)
library(DT)

# library(flexdashboard)

# feather : Faster than RDS if not compromised by storage
# More info : https://appsilon.com/fast-data-loading-from-files-to-r/
library(feather)

library(leaflet)



# 2. Load Data + functions  --------------------------------------------------

source("R/helpers.R")
Data <- feather::read_feather("Data/Data-Decathlon.feather") %>% as.data.table()
cities <- read.csv2("Data/Cities-lat-long.csv",encoding = "UTF-8")

 

# 3. Extra  ------------------------------------------------------------------

# JS object that I use frequently in all my highcharts functions
# For enabling + customizing the exporting button
JS_enable_exporting <- JS(('{
                          contextButton: {
                              symbolStroke: "white",
                              theme: {
                      fill:"#3C8DBC"
                      }
                          }
                      }'))

