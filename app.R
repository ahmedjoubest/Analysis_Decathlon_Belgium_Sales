#shiny manager

#install packages matnsach

# Explique chaque famille de packages sert a quoi !!

# {plyr} package must be unloaded : bad interference with {dplyr} functions
# detach("package:plyr", unload=TRUE)

library(pander)
library(shiny)
library(dplyr)
library(highcharter)
library(data.table)
library(feather) # Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
library(shinyWidgets)
library(DT)
library(shinyBS)

source("helpers.R")

# ALLL7777WWWAAAA =)
# commenter had lqlawi fl app ;)
#golih had nwita f mail anak z3ma wa3R f js donc sehlat 3lik lqadya

Data <- read_feather("Data-Decathlon.feather")

server <- function(input, output, session) {
  
  # 2.0 Filtered data to visualize  ----
  DT <- eventReactive(input$submit,{
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
                                intensity = input$intensity,
                                transaction_type = input$transaction_type)
    
  })   
}
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
  ),

  titlePanel("iris"),
  sidebarLayout(
    sidebarPanel(
      
      tipify(pickerInput(
        inputId = "store_name",
        label = "Stores",
        choices = unique(Data$store_name),
        multiple = TRUE,
        selected = unique(Data$store_name)[1:5],
        options = list(`actions-box` = TRUE,`live-search` = TRUE)
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
        options = list(`actions-box` = TRUE,`live-search` = TRUE)
      ),"Dependence info: acts on {Range Price} filter","right"),
      tipify(dateRangeInput('date',
                            'Date range',
                            start = min(Data$the_date_transaction),
                            end = max(Data$the_date_transaction)
      ),"Dependence info: independent filter","right"),
      
      br(),br(),
      
      div(align = 'center',
          tipify(actionBttn(
            inputId = "submit",
            label = "Filter",
            style = "stretch", 
            color = "primary",
            icon = icon("sliders"),
            size = 'sm'),
            "Apply filters",
            placement = "top"
          ))
    ),

    mainPanel(
  
      # reste a faire : Revoir les couleurs du CSS en fonction des couleurs de dashbaord ;)
      radioGroupButtons("transaction_type", label = "Transaction type", status = "primary",
                        choices=c("Sales" = "sale" ,"Returns" = "return")
                        ),
      radioGroupButtons("intensity", label = "Intensity variable", status = "primary",
                        choices=c("Number of transactions" = "length(the_transaction_id)" ,
                                  "Total turnover" = "sum(turnover)",
                                  "Total quantity" = "sum(quantity)")
                        ),
      
      highchartOutput("heat_map_correlation"),
      tableOutput("logs_table")
      
    )
    
  )
)

# 3.0 RUN APP -------------------------------------------------------------

shinyApp(ui, server)
