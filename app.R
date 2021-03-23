

#shiny manager

#install packages matnsach

# Explique chaque famille de packages sert a quoi !!

# {plyr} package must be unloaded : bad interference with {dplyr} functions
# detach("package:plyr", unload=TRUE)
library(scroller) # to install : remotes::install_github("lgnbhl/scroller")
library(shinydashboard)
library(pander)
library(shiny)
library(dplyr)
library(highcharter)
library(data.table)
library(feather) # Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
library(shinyWidgets)
library(DT)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
# Reteb tes librairies m3a b3dyathom b tariqa shiha

source("helpers.R")

# ALLL7777WWWAAAA =)
# commenter had lqlawi fl app ;)
#golih had nwita f mail anak z3ma wa3R f js donc sehlat 3lik lqadya

Data <- read_feather("Data-Decathlon.feather")

server <- function(input, output, session) {
  
  # 2.0 Filtered data to visualize  ----
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
  
  
  
  # 2.1 Filter interdependence ----
  
  ### Explanation of dependence between filters  ###
  ### {price, item} depend on <= {store}         ###
  ### {price} <=> {item} : interdependent        ###
  ### {Date}: independent                        ###
  ##################################################
  
  # Update prices and item_names based on store_names
  observeEvent(input$store_name,ignoreInit = TRUE, {
    # Update prices based on store_names
    prices_updated <- c(round(min(Data$prices[Data$store_name %in% input$store_name])-1),
                        round(max(Data$prices[Data$store_name %in% input$store_name])+1)
    )
    updateSliderInput(session, "prices", value = prices_updated)
    # Update item_names based on store_names
    item_names_update <- unique(Data$item_name[Data$store_name %in% input$store_name &
                                                 Data$prices >= input$prices[1] & # to respect the updated prices
                                                 Data$prices <= input$prices[2] ])# to respect the updated prices
    updatePickerInput(session, "item_name", selected = item_names_update)
  })
  
  # Update prices based on item_names
  observeEvent(input$item_name,ignoreInit = TRUE, {
    prices_updated <- c(round(min(Data$prices[Data$item_name %in% input$item_name & 
                                                Data$store_name %in% input$store_name])-1), # to respect the actual input$name_tore
                        round(max(Data$prices[Data$item_name %in% input$item_name & 
                                                Data$store_name %in% input$store_name ])+1) # to respect the actual input$name_tore
                        
    )
    updateSliderInput(session, "prices", value = prices_updated)
  })
  
  # Update item_names based on prices 
  observeEvent(input$prices,ignoreInit = TRUE, {
    item_names_update <- unique(Data$item_name[Data$store_name %in% input$store_name & # to respect the actual input$name_tore
                                                 Data$prices >= input$prices[1] & 
                                                 Data$prices <= input$prices[2] ])
    updatePickerInput(session, "item_name", selected = item_names_update)
    })
  
  output$heat_map_correlation <- renderHighchart({
    Make_HC_Heatmap_Correlation(DT(), X = 'store_name',Y = 'item_name',
                         intensity = input$intensity_1, #default input='length(the_transaction_id)' for transaction occurrence
                         transaction_type = input$transaction_type_1) #default input='sales'
  })
  
  output$heat_map_chronologic <- renderHighchart({
    Make_HC_Chronological_heatmap(DT(),
                                intensity = input$intensity_2,
                                display_by = input$display, # default 
                                transaction_type = input$transaction_type_2) #default input='sales'
  })
}


## 1. header ------------------------------
header <- 
  dashboardHeader(title = "Sales Analysis")

## 2. sidebar ------------------------------
sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Main Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
      menuItem("Filtered data", tabName = 'Filtered Data?icon', icon = icon('table'))
      
      ),
    
    br(),
    
    h4(tags$u("Filter Data:"),align = "center"),
    
    tipify(pickerInput(
      inputId = "store_name",
      label = "Stores",
      choices = unique(Data$store_name),
      multiple = TRUE,
      selected = unique(Data$store_name)[1:5],
      options = list(`actions-box` = TRUE,`live-search` = TRUE, size = 5)
    ),"Dependence info: acts on the {Range Price} and {Items} filter","right"),
    tipify(sliderInput(
      inputId = "prices",
      label = "Range of item price in Euro",
      min = round(min(Data$prices))-1,
      max = round(max(Data$prices))+1,
      value = c(1,500)
    ),"Dependence info: acts on {Items} filter","right"),
    tipify(pickerInput(
      inputId = "item_name",
      label = "Items",
      choices = unique(Data$item_name),
      selected = unique(Data$item_name)[1:5],
      multiple = TRUE,
      options = list(`actions-box` = TRUE,`live-search` = TRUE,size = 5)
    ),"Dependence info: acts on {Range Price} filter","right"),
    tipify(dateRangeInput('date',
                          'Date range',
                          start = min(Data$the_date_transaction),
                          end = max(Data$the_date_transaction)
    ),"Dependence info: independent filter","right"),
    
    br(),
    
    div(align = 'center',
        tipify(actionBttn(
          inputId = "submit",
          label = "Filter",
          style = "stretch", 
          color = "primary",
          icon = icon("sliders")
          ),
          "Apply filters",
          placement = "top"
        ))
    )

## 3. body --------------------------------
body <- dashboardBody( 
  
  
  scroller::use_scroller(),
  # styling bla bla comment
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
  ),
  
  ## 3.1 Dashboard body --------------
  tabItems(
    ## 3.1 Main dashboard ---------------------------------------------------------- 
    tabItem(tabName = 'dashboard',
            
            # chronological evolution heatmap :
            box(title = HTML("<div class='w-h-t-circle'> </div> 
                     <b class='boxtitle' > Heatmap of chronogical evolution </b>"),
                fluidRow(
                  column(1),
                  column(2,
                         radioGroupButtons("transaction_type_2", label = h5("Transaction type"), status = "primary",
                                           choices=c("Sales" = "sale" ,"Returns" = "return"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(5,
                         radioGroupButtons("intensity_2", label = h5("Intensity variable"), status = "primary",
                         choices=c("Number of transactions" = "number_of_transaction",
                                   "Total turnover" = "turnover",
                                   "Total quantity" = "quantity"),
                         justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(2,radioGroupButtons("display", label = h5("Intensity variable"), status = "primary",
                         choices=c("Months", "Quarters"), justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(1,h6(style="margin-bottom: 3.3em;"),
                         bsButton("q1", label = "", icon = icon("info"),
                                  style = "primary", size = "extra-small", action = "toggle"),
                         bsPopover(id = "q1", title = "info", placement = "bottom", trigger = "hover",
                                   content = ("For a relevent analysis, select one single item or items of the same category "),
                         )),
                  column(1)
                ),
                highchartOutput("heat_map_chronologic") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1)
                ,width=12,status="primary", solidHeader = T, height = 520),
            
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
                                           choices=c("Number of transactions" = "length(the_transaction_id)",
                                                     "Total turnover" = "sum(turnover)",
                                                     "Total quantity" = "sum(quantity)"),
                                           justified =T,width = "100%", size = "xs", individual = TRUE
                         )
                  ),
                  column(2)
                ),
                highchartOutput("heat_map_correlation") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1)
                ,width=12,status="primary", solidHeader = T, height = 520),            
            
            # Button to scroll to the top (toggle sidebar)
            fluidRow(a("Scroll to top ", type="button", class = "btn btn-primary",
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
