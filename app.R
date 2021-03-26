
# 1.1 PACKAGES ----------------------------------------------------------

#detach("package:plyr", unload=TRUE) must be unloaded : unwanted interference with {dplyr}

library(scroller) # to install : remotes::install_github("lgnbhl/scroller")

library(shiny)
library(shinydashboard)

library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

library(lubridate)
library(dplyr)
library(highcharter)
library(data.table)
library(DT)

library(feather) # Faster than RDS if not compromised by storage. https://appsilon.com/fast-data-loading-from-files-to-r/

# 1.2 Load Data + plotting functions  -----------------------------------

source("helpers.R")
Data <- read_feather("Data-Decathlon.feather")

# 2.1 SERVER  -----------------------------------------------------------

server <- function(input, output, session) {
  
  # 2.2 Reactive filtered Data  -----------------------------------------
  DT <- eventReactive(input$submit, ignoreNULL = FALSE,{
    subset(
      Data,
      item_name %in% input$item_name &
        store_name %in% input$store_name &
        prices >= input$prices[1] &
        prices <= input$prices[2] &
        the_date_transaction >= input$date[1] &
        the_date_transaction  <=  input$date[2]
    )
  })
  
  # 2.3 Filter interdependence ------------------------------------------
  
  ### {item} depends on <= {store} & {price range} ###
  ### {Date}: independent filter                   ###
  ####################################################
  
  # Update {item} based on {store} with respecting the chosen {price range}
  observeEvent(input$store_name,ignoreInit = TRUE, {
    item_names_update <- unique(Data$item_name[Data$store_name %in% input$store_name &
                                                 Data$prices >= input$prices[1] & # to respect the chosen {price range}
                                                 Data$prices <= input$prices[2] ])# to respect the chosen {price range}
    updatePickerInput(session, "item_name", selected = item_names_update, choices = item_names_update)
  })
  
  # Update {item} based on {price range} with respecting the chosen {store}
  observeEvent(input$prices,ignoreInit = TRUE, {
    item_names_update <- unique(Data$item_name[Data$store_name %in% input$store_name & # to respect chosen {store}
                                                 Data$prices >= input$prices[1] & 
                                                 Data$prices <= input$prices[2] ])
    updatePickerInput(session, "item_name", selected = item_names_update, choices = item_names_update)
  })
  
  
  # 2.4 Items/stores correlation heat map  ------------------------------
  
  output$heat_map_correlation <- renderHighchart({
    Make_HC_Heatmap_Correlation(DT = DT(), X = 'store_name',Y = 'item_name',
                                intensity = input$intensity_1, # default input='the_transaction_id' for transaction occurrence
                                transaction_type = input$transaction_type_1 # default input='return'
    ) 
  })
  
  # 2.5 Chronological comparison heat map  -----------------------------
  
  output$heat_map_chronologic <- renderHighchart({
    Make_HC_Chronological_heatmap(DT= DT(), 
                                  X = input$display, # default input = 'day'
                                  intensity = input$intensity_2,  # default input='the_transaction_id' for transaction occurrence
                                  transaction_type = input$transaction_type_2) # default input='sale' 
  })
  
  # 2.6 Stores comparison barplot  -------------------------------------
  
  output$Bar_plot_comparaison <- renderHighchart({
    Make_HC_Barplot(DT = DT(), 
                    X = "store_name",
                    Y = input$barplot_y_axis, # default input='the_transaction_id' for transaction occurrence
                    group = "tdt_type_detail",
                    percent = input$percent, # default input = FALSE 
                    horizontal = input$horizontal) # default input = FALSE 
  })
}


# 3.1 HEADER -----------------------------------------------------------
header <- 
  dashboardHeader(title = "Sales Analysis")

# 3.2 SIDEBAR ----------------------------------------------------------
sidebar <- 
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Main Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
      menuItem("Filtered data", tabName = 'Filtered_Data', icon = icon('table'))
    ),
    
    br(),
    
    h4(tags$u("Filter Data:"),align = "center"),
    h6(tags$u("note"),": please filter from top to bottom, hover filters fore more info.", style='margin-left: 1.4em; color:red;'),
    
    ## Stores filter
    tipify(pickerInput(
      inputId = "store_name",
      label = "Stores",
      choices = unique(Data$store_name),
      multiple = TRUE,
      selected = unique(Data$store_name)[1:6],
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 5, dropdownAlignRight = T)
    ),"Dependence info: this filter acts on {Items} filter","right"),
    
    ## Price range filter
    tipify(sliderInput(
      inputId = "prices",
      label = "Range of item price in Euro",
      min = round(min(Data$prices))-1,
      max = round(max(Data$prices))+1, 
      value = c(1,500)
    ),"Dependence info: this filter acts on {Items} filter","right"),
    
    ## Items filter
    tipify(pickerInput(
      inputId = "item_name",
      label = "Items",
      choices = unique(Data$item_name),
      selected = unique(Data$item_name)[c(2,3,5,6,7,8,10,14,15)],
      multiple = TRUE,
      options = list(`actions-box` = TRUE,`live-search` = TRUE,size = 5)
    ),"Dependence info: this filter does NOT act on any filter!","right"),
    
    ## Date range filter
    tipify(dateRangeInput('date',
                          'Date range (2015-2019)',
                          start = min(Data$the_date_transaction),
                          end = ymd("2015-04-05")
    ),"Dependence info: this filter is independent","right"),
    
    br(),
    
    div(align = 'center',
        
        ## Filter button
        tipify(actionBttn(
          inputId = "submit",
          label = "Filter",
          style = "stretch", 
          color = "primary",
          icon = icon("sliders")
        ), "Apply filters", placement = "top"),
        
        br(),br(),br(),
        
        ## Scroll down button
        a(tags$b("Scroll Down",style="color: black;"),
          type="button", class = "btn btn-primary",
          href = "#.down", icon("arrow-down",class = "arrow_down"))
    )
  )

# 3.3 DASHBOARD BODY -------------------------------------------------
body <- dashboardBody( 
  
  ## Set scrolling options
  scroller::use_scroller(animationLength = 265),
  
  ## Import styling CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css4.css") 
  ),
  
  # 3.3 DASHBOARD BODY  ----------------------------------------------
  
  tabItems(
    tabItem(tabName = 'dashboard',
            
            h3("Heatmap of chronological  this when you groupped by thus
               and the beaut of heat and hor were i am in love with you 
               bro etc. of this when you groupped by thus and the beaut 
               of heat and hor were i am in love with you bro etc..",
               style="margin-left: 0.65em;"),
            
            br(),
            
            # 3.4 chronological comparison heatmap box --------------
            
            box(title = HTML("<div class='w-h-t-circle'> </div> 
                     <b class='boxtitle' > Heatmap of chronogical evolution </b>"),
                fluidRow(
                  column(1,
                         br(),br(),
                         tipify(actionBttn(inputId = "null", style = "stretch", color = "primary",
                                           icon = icon("info"),size = "sm",block="T"),
                                "it is preferable for this heatmap to choose a single item or a signle category of items for a meaningful analysis",
                                "bottom")
                  ),
                  column(2,
                         radioGroupButtons("transaction_type_2", label = h5("Transaction type"), status = "primary",
                                           choices=c("Sales" = "sale" ,"Returns" = "return"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(6,
                         radioGroupButtons("intensity_2", label = h5("Intensity variable"), status = "primary",
                                           choices=c("Total transactions" = "the_transaction_id",
                                                     "Total turnover" = "turnover",
                                                     "Total quantity" = "quantity"),
                                           selected = "turnover",
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(2,
                         tipify(radioGroupButtons("display", label = h5("Display by"), status = "primary",
                                                  choices=c("Days"="day","Months"='month', "Quarters"='quarter'), justified =T,width = "100%",
                                                  size = "xs", individual = TRUE
                         ),"If aggregated by months or quarters : make sure you filter by a large period of time (more than one year) so that you can visualize the effect of different categories of month/quarter on the sales",
                         "bottom"
                         )
                  ),
                  column(1),
                ),
                
                highchartOutput("heat_map_chronologic") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1)
                ,width=12,status="primary", solidHeader = T),
            
            
            
            
            # correlation heatmap :
            box(title = HTML("<div class='w-h-t-circle'> </div> 
                     <b class='boxtitle' > Heatmap of items by stores </b>"),
                fluidRow(
                  column(1),
                  column(2,
                         radioGroupButtons("transaction_type_1", label = h5("Transaction type"),
                                           status = "primary", choices=c("Sales" = "sale" ,"Returns" = "return"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE,
                                           selected = 'return'
                         )
                  ),
                  column(1),
                  column(6,
                         radioGroupButtons("intensity_1", label = h5("Intensity variable"), status = "primary",
                                           choices=c("Number of transactions" = "the_transaction_id",
                                                     "Total turnover" = "turnover",
                                                     "Total quantity" = "quantity"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(2)
                ),
                highchartOutput("heat_map_correlation") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1)
                ,width=12,status="primary", solidHeader = T),
            
            box(title = HTML("<div class='w-h-t-circle'> </div> 
                            <b class='boxtitle' > Barplot comparaison of sales in
                             different stores </b>"),
                fluidRow(
                  column(1),
                  column(2,h5("Horizontal bars",style="margin-bottom: 1.15em;"),
                         materialSwitch(
                           inputId = "horizontal",
                           value = FALSE,
                           status = "primary",
                         )
                  ),
                  column(2,h5("Show percentage",style="margin-bottom: 1.15em;"),
                         materialSwitch(
                           inputId = "percent",
                           value = FALSE,
                           status = "primary",
                         )
                  ),
                  column(6,
                         radioGroupButtons("barplot_y_axis", label = h5("Display by"), status = "primary",
                                           choices=c("Number of transactions" = "the_transaction_id",
                                                     "Total turnover" = "turnover",
                                                     "Total quantity" = "quantity"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(1)
                ),
                highchartOutput("Bar_plot_comparaison") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1)
                ,width=12,status="primary", solidHeader = T),
            
            # Button to scroll to the top (toggle sidebar)
            div(a("Scroll to top ", type="button", class = "btn btn-primary down",
                  href = "#.sidebar-toggle", icon("arrow-up")),
                align = "center")
            
            
    )
    
  ),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<center> <img src = "logo.jpg" height = 51></center>\');
      })
     '))
)



## put UI together --------------------
ui <- 
  dashboardPage(header, sidebar, body )


# 3.0 RUN APP -------------------------------------------------------------

shinyApp(ui, server)
