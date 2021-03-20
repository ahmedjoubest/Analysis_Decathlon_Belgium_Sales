#shiny manager

#install packages matnsach

# Explique chaque famille de packages sert a quoi !!

library(shiny)
library(dplyr)
library(highcharter)
library(data.table)
library(feather) # Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
library(shinyWidgets)
library(DT)
library(shinyBS)

# ALLL7777WWWAAAA =)
# commenter had lqlawi fl app ;)
#golih had nwita f mail anak z3ma wa3R f js donc sehlat 3lik lqadya

Data <- read_feather("Data-Decathlon.feather")

server <- function(input, output, session) {
  
  # 2.0 Data to visualize  ----
  DT <- eventReactive(input$submit,{
    # Filter application
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
  observeEvent(input$prices,ignoreInit = TRUE, {isolate({
    item_names_update <- unique(Data$item_name[Data$store_name %in% input$store_name & # to respect the actual input$name_tore
                                                 Data$prices >= input$prices[1] & 
                                                 Data$prices <= input$prices[2] ])
    updatePickerInput(session, "item_name", selected = item_names_update)})
    })
  
  output$plot <- renderHighchart({
    
    fntltp <- JS("
function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
              }
          return  'Decathlon '  + '<b>' + getPointCategoryName(this.point, 'x') + '</b>' +'<br>'+
          '<b>' + getPointCategoryName(this.point, 'y') + '</b> <br>'+
          '<b>' + this.point.value + '</b> transactions <br><b>' + '</b>';
    }")
    
    # fait gaffe khass takhod sales merra w returns mra okhra =)
    hchart(DT() %>%
             group_by(store_name, item_name) %>% 
             summarize(number_of_transaction = length(the_transaction_id)),
           "heatmap",
           hcaes(x = store_name, y = item_name, value = number_of_transaction),
           name = "Number of transctions",
           marginTop = 0,
           marginBottom = 0) %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_yAxis(title= "null") %>%
      hc_xAxis(title= "null") %>%
      hc_tooltip(formatter = fntltp) %>%
      hc_colorAxis(
        min = 0,
        minColor= '#FFFFFF',
        maxColor= '#07AE6B'
      ) %>%
      hc_legend(
        align= 'right',
        layout= 'vertical',
        margin= 0,
        verticalAlign= 'top',
        y= 25,
        symbolHeight= 310
      )})   
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
            "Explication des filtres",
            placement = "top"
          ))
      
      
      
    ),
    
    mainPanel(  
      highchartOutput("plot"),
      dataTableOutput("datatable")
    )
    
  )
)
shinyApp(ui, server)
