
# 1. HEADER ------------------------------------------------------------

header <- 
  dashboardHeader(title = "Sales Analysis")

# 2. SIDEBAR -----------------------------------------------------------

sidebar <- 
  dashboardSidebar(
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Main Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
      menuItem("Filtered data", tabName = 'Filtered_Data', icon = icon('table'))
    ),
    
    br(),
    
    h4(tags$u("Filter Data:"),align = "center"),
    
    ### Remark
    h6(tags$u("note"),": please filter Data everytime from top to bottom, hover 
       filters fore more info.", style='margin-left: 1.4em; color:red;'),
    
    ### Stores filter
    shinyBS::tipify(
      pickerInput(
        inputId = "store_names",label = "Stores",
        choices = unique(Data$store_name),
        multiple = T,
        selected = unique(Data$store_name),
        options = pickerOptions(actionsBox = T, liveSearch = T, size = 5,
                                dropdownAlignRight = T)
      ),
      "Dependence info: this filter acts on {Items} filter",
      "right"
    ),
    
    ### Prices range filter
    shinyBS::tipify(
      sliderInput(
        inputId = "prices_range",
        label = "Range of item price in Euro",
        min = round(min(Data$prices))-1,
        max = round(max(Data$prices))+1,
        value = c(30,760)
      ),
      "Dependence info: this filter acts on {Items} filter",
      "right"
    ),
    
    ### Items filter
    shinyBS::tipify(
      pickerInput(
        inputId = "item_names",
        label = "Items",
        choices = unique(Data$item_name),
        selected = unique(Data$item_name)[c(1:10)],
        multiple = T,
        options = list(`actions-box` = T,`live-search` = T,size = 5)
      ),
      "Dependence info: this filter does NOT act on any filter! And get updated each time the first two filters are modified to make sure that you do not miss items",
      "right"
    ),
    
    ### Dates range filter
    shinyBS::tipify(
      radioButtons(
        "radio_date_format",
        "Date range (2012-2021)",
        choices = c("All time" = 10000,
                    "Last 30 days" = 30,
                    "Last 90 days" = 90,
                    "Last year" = 365,
                    "Last 3 years" = 365*3,
                    "Pick a range of date" = 0
        ),
        selected = 0
      ),
      "Dependence info: this filter is independent",
      "right"),
    # pick date range filter
    conditionalPanel(
      condition = "input.radio_date_format == '0'",
      dateRangeInput(
        'dates_range',
        '',
        start = max(Data$the_date_transaction) -30*4, # Default to last 5 months
        end = max(Data$the_date_transaction)
      )
    ),
    
    br(),
    
    div(
      align = 'center',
      
      ### Submit button
      shinyBS::tipify(
        actionBttn(
          inputId = "submit",
          label = "Filter",style = "stretch",
          color = "primary",
          icon = icon("sliders")
        ),
        "Apply filters",
        "top"),
      
      br(),br(),br(),br(),br(),
      
      ### Scroll down button
      a(tags$b("Scroll Down",style="color: black;"),
        type="button",class = "btn btn-primary",
        href = "#.down", icon("arrow-down",class = "arrow_down"))
    )
  )


# 3. DASHBOARD BODY ----------------------------------------------------

body <- dashboardBody( 
  
  ### Set scrolling options
  scroller::use_scroller(animationLength = 265),
  
  ### Import styling CSS file + Decathlon favicon
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_custom.css") ,
    tags$link(rel="icon",type="image/x-icon", href="https://joinus.decathlon.be/generated_contents/favicon/RynXW55Z/apple-touch-icon-57x57.png")
  ),
  
  ### Set logo  on navbar
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<center> <img src = "logo.jpg" height = 51></center>\');
      })
     ')),
  
  tabItems(
    
    # 3.1. MAIN DASHBOARD BODY  ----------------------------------
    
    tabItem(tabName = 'dashboard',
            
            # 3.1.1 Title  ------------------------------------------------
            
            uiOutput('title') %>% 
              withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.5),  
            
            br(),
            
            
            # 3.1.2 Value boxes  ------------------------------------------
            
            div(align='center',h2(tags$u("Overview - Total sales"),align = 'align')),
            
            # Total turnover
            h3("Total turnover"),
            fluidRow(
              shinydashboard::valueBoxOutput("valuebox_turnover_sales",width = 6) %>% 
                withSpinner(color="#3C8DBC",type=4, proxy.height = "128px"),
              shinydashboard::valueBoxOutput("valuebox_turnover_returns",width = 6) %>% 
                withSpinner(color="#ECF0F5",type=0,proxy.height = "0px")
            ),
            
            # Total number of transaction
            h3("Total number of transactions"),
            fluidRow(
              shinydashboard::valueBoxOutput("valuebox_transactions_sales",width = 6) %>% 
                withSpinner(color="#3C8DBC",type=4, proxy.height = "128px"),
              shinydashboard::valueBoxOutput("valuebox_transactions_returns",width = 6) %>% 
                withSpinner(color="#ECF0F5",type=0, proxy.height = "0px")
            ),
            
            br(),
            
            # 3.1.3 KPI Evolution Gauges  ---------------------------------
            
            
            ### Title
            div(align='center',h2(tags$u("KPIs Evolution"),align = 'align')),
            
            br(),
            
            # 3.1.4 KPI text description ---------------------------------- 
            
            uiOutput('KPI_text_explication') %>% 
              withSpinner(color="#3C8DBC",type=4, proxy.height = "97px",size = 0.75),
            
            br(),
            
            # 3.1.5 KPI boxes flexdashboard -------------------------------
            
            fluidRow(
              
              box(
                title = HTML("<div class='w-h-t-circle'> </div>
                <b class='boxtitle' > Average Basket
                             </b>"),
                flexdashboard::gaugeOutput("Gauge_AB")%>%
                  withSpinner(color="#3C8DBC",type=4),
                br(),br(),
                width=4,status="primary", solidHeader = T,align='center'),
              
              box(
                title = HTML("<div class='w-h-t-circle'> </div>
                <b class='boxtitle' > Mean Transactions per Day
                             </b>"),
                flexdashboard::gaugeOutput("Gauge_TPD")%>%
                  withSpinner(color="#3C8DBC",type=4),
                br(),br(),
                width=4,status="primary", solidHeader = T,align='center'),
              
              box(
                title = HTML("<div class='w-h-t-circle'> </div>
                <b class='boxtitle' > Mean Sold units per Day
                               </b>"),
                flexdashboard::gaugeOutput("Gauge_QPD") %>%
                  withSpinner(color="#3C8DBC",type=4, size = 1.1),
                br(),br(),
                width=4,status="primary", solidHeader = T,align='center')
            ),
            
            br(),br(),
            
            # 3.1.6 Leaflet Map  ------------------------------------------
            
            ### Title
            div(
              align='center',
              h2(tags$u("Geographical distribution of sales in Belgium"),
                 align = 'align')
            ),
            
            br(),
            
            ### Explanations
            h3("Here you can visualize in a map the distribution of the sales performances
            of the different Decathlon stores according to the total turnover, the total 
            number of transactions or the total quantity of products sold."),
            
            h3(
              tags$u((tags$b("Note:"))),
              " Click on the circle to see the name of the store and its corresponding value."
            ),
            
            br(),br(),
            
            ### Leaflet box
            box(
              
              title = HTML("<div class='w-h-t-circle'> </div> 
            <b class='boxtitle' > 
            Geographical distribution of sales indicators 
                         </b>"),
              
              # Filter buttons
              fluidRow(
                
                column(2),
                
                column(
                  3,
                  radioGroupButtons(
                    "transaction_type_leaflet",
                    label = h5("Transaction type"), status = "primary",
                    choices=c("Sales" = "sale" ,"Returns" = "return"),
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(
                  6,
                  radioGroupButtons(
                    "radius", label = h5("Color intensity variable"),
                    status = "primary",
                    choices=c("Total transactions" = "the_transaction_id",
                              "Total turnover" = "turnover",
                              "Total quantity" = "quantity"),
                    selected = "turnover",
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(1)
              ),
              
              #### Leaflet output
              leafletOutput("leaflet_Map",width="95%") %>%
                withSpinner(color="#3C8DBC",type=4, size = 1.1),
              br(),
              status="primary", solidHeader = T, width = 12,align='center'),
            
            # Quick&Dirty debug (to be reviewed later)
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            
            # 3.1.6 Temporal evolution Heat Map -----------------------------
            
            ### Title
            div(
              align='center',
              h2(tags$u("Temporal evolution Heat map"))
            ),
            
            ### explanations
            h3("In this Heat Map you can see a graphic representation of the temporal 
            evolution of the totals of the 3 indicators seen before (turnover, product
            quantity or number of transaction),that are aggregated and superimposed in 
               a chosen period of time (days of the week, months or quarters)."),
            h4(tags$u((tags$b("Purpose:"))),
               "This type of heatmap is relevant to detect the possible effects of periodic events 
               impacting the sales performance (week end effect, peak sales seasons...), with reference to an item
               or a category of items. Therefore, it is more preferable to filter by a 
               single item or category of items for this graph to derive relevant information."),
            h4(tags$u((tags$b("Note:"))),
               "Click on the exporting button at the top right of Highcharts to view in full screen,
               to export Data or download the plot."),
            
            
            br(),br(),
            
            ### Box + plot
            box(
              title = HTML("<div class='w-h-t-circle'> </div>
            <b class='boxtitle' >
            Heat Map : Periodic visualization of temporal evolution of sales performance
                             </b>"),
              
              #### Filters
              fluidRow(
                
                column(
                  1,
                  br(),br(),
                  shinyBS::tipify(
                    actionBttn(
                      inputId = "null", style = "stretch", color = "primary",
                      icon = icon("info"),size = "sm",block="T"
                    ),
                    "it is more preferable for this heatmap to choose a single item or a signle category of items for a meaningful analysis",
                    "bottom")
                ),
                
                column(
                  2,
                  radioGroupButtons(
                    "transaction_type_2", label = h5("Transaction type"), status = "primary",
                    choices=c("Sales" = "sale" ,"Returns" = "return"),
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(
                  6,
                  radioGroupButtons(
                    "intensity_2", label = h5("Color intensity variable"), status = "primary",
                    choices=c("Total transactions" = "the_transaction_id",
                              "Total turnover" = "turnover",
                              "Total quantity" = "quantity"),
                    selected = "the_transaction_id", justified =T,width = "100%", size = "xs",
                    individual = T
                  )
                ),
                
                column(
                  2,
                  shinyBS::tipify(
                    radioGroupButtons(
                      "display", label = h5("Display by"), status = "primary",
                      choices=c("Days"="day","Months"='month', "Quarters"='quarter'),
                      justified =T,width = "100%", size = "xs", individual = T
                    ),
                    "If aggregated by months or quarters : make sure you filter by a large period of time (more than one year) so that you can visualize the effect of different categories of month/quarter on the sales",
                    "bottom"
                  )
                ),
                
                column(1),
                
              ),
              
              # Heat map output
              highchartOutput("heat_map_chronologic") %>%
                withSpinner(color="#3C8DBC",type=4, size = 1.1),
              width=12,status="primary", solidHeader = T),
            
            
            # 3.1.7 Temporal evolution lines --------------------------------
            
            # Quick&Dirty debug (to be reviewed later)
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(
              align='center',
              h2(tags$u("Temporal evolution By items"),
                 align = 'align')
            ),
            
            ### Explanation
            h3("Since the last graph provides a periodic but mainly categorical visualization,
            this one, which is by the way complementary to the one before, can be useful to
            follow the evolution in time of the item sales in an individual way, 
               and even more to compare them."),
            
            br(),
            
            ### Box
            box(
              title = HTML("<div class='w-h-t-circle'> </div> 
                     <b class='boxtitle' >
                         Lines : Temporal evolution by item
                         </b>"),
              
              # filters
              fluidRow(
                
                column(1),
                
                column(
                  2,
                  radioGroupButtons(
                    "transaction_type_line_HC", label = h5("Transaction type"),
                    status = "primary", choices=c("Sales" = "sale" ,"Returns" = "return"),
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(
                  5,
                  radioGroupButtons(
                    "Y_line_HC", label = h5("Y variable"), status = "primary",
                    choices=c("Total transactions" = "the_transaction_id",
                              "Total turnover" = "turnover",
                              "Total quantity" = "quantity"),
                    selected = "turnover",
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(
                  3,
                  radioGroupButtons(
                    "aggregation_line_HC", label = h5("Aggregate by"), status = "primary",
                    choices=c("Days"="days",
                              "Weeks"='weeks',
                              "Months"='months',
                              "Years"="years"
                    ),
                    selected = 'weeks', justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(1),
              ),
              
              # Lines output
              highchartOutput("Build_Date_line_charts") %>%
                withSpinner(color="#3C8DBC",type=4, size = 1.1),
              width=12,status="primary", solidHeader = T),
            
            
            
            # 3.1.8 Correlation store/item Heat Map -------------------------
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(align='center',h2(tags$u("Heatmap: item/store"),align = 'align')),
            
            br(),
            
            ### explanation
            h3("This Heat Map is well suited to visualize and compare massively the sales
            performance of different items in different stores. In addition, it is usually
               useful to detect anomalies."),
            
            br(),br(),
            
            ### Box
            box(
              title = HTML("<div class='w-h-t-circle'> </div>
                         <b class='boxtitle' > Heatmap of items by stores
                         </b>"),
              
              # filters
              fluidRow(
                column(1),
                
                column(
                  2,
                  radioGroupButtons(
                    "transaction_type_1", label = h5("Transaction type"), status = "primary",
                    choices=c("Sales" = "sale" ,"Returns" = "return"), justified =T,
                    width = "100%", size = "xs", individual = T, selected = 'return'
                  )
                ),
                
                column(1),
                
                column(
                  6,
                  radioGroupButtons(
                    "intensity_1", label = h5("Intensity variable"), status = "primary",
                    choices=c("Number of transactions" = "the_transaction_id",
                              "Total turnover" = "turnover",
                              "Total quantity" = "quantity"),
                    selected = "quantity",
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                column(2)
              ),
              
              # Heat Map output
              highchartOutput("heat_map_correlation") %>%
                withSpinner(color="#3C8DBC",type=4, size = 1.1),
              width=12,status="primary", solidHeader = T),
            
            
            # 3.1.9 Bar Plot Comparison stores ------------------------------
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(align='center',h2(tags$u("Comparaison of sales in different stores"),align = 'align')),
            
            ### explanation
            h3("This graph is also very complementary to the one before. It allows you to compare the sales performance
               of the different stores by summing up the three indicators for all the items selected in the filter 
               in a categorical way."),
            
            br(),br(),
            
            ### Box
            box(
              title = HTML("<div class='w-h-t-circle'> </div>
            <b class='boxtitle' > Barplot: Comparaison of sales in
                         different stores </b>"),
              # filters
              fluidRow(
                column(1),
                
                column(
                  2,
                  h5("Horizontal bars",style="margin-bottom: 1.15em;"),
                  materialSwitch(
                    inputId = "horizontal",
                    value = F,
                    status = "primary",
                  )
                ),
                
                column(
                  2,
                  h5("Show percentage",style="margin-bottom: 1.15em;"),
                  materialSwitch(
                    inputId = "percent",
                    value = F,
                    status = "primary",
                  )
                ),
                
                column(
                  6,
                  radioGroupButtons(
                    "barplot_y_axis", label = h5("Display by"), status = "primary",
                    choices=c("Number of transactions" = "the_transaction_id",
                              "Total turnover" = "turnover",
                              "Total quantity" = "quantity"),
                    selected = "turnover",
                    justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
                
                column(1)
              ),
              
              # Bar Plot output
              highchartOutput("Bar_plot_comparaison") %>%
                withSpinner(color="#3C8DBC",type=4, size = 1.1),
              width=12,status="primary", solidHeader = T),
            
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ###. Button to scroll to the top (toggle sidebar)
            div(
              a("Scroll to top ", type="button", class = "btn btn-primary down",
                href = "#.sidebar-toggle", icon("arrow-up")),
              align = "center"
            )
    ),
    
    # 3.2. Filtered Data  -------------------------------------------------
    
    tabItem(
      
      tabName = "Filtered_Data",
      
      ### Download button
      fluidRow(
        align = 'center',
        downloadButton('Download', 'Download Filtered Data', class = "btn-primary btn down")
      ),
      
      br(),
      
      ### Box containing the Filtered Data
      box(
        title = HTML("<div class='w-h-t-circle'> </div>
      <b class='boxtitle' > Filered Data </b>"),
        br(),
        # Data table output
        DT::dataTableOutput(outputId = "DT") %>%
          withSpinner(color="#3C8DBC",type=4, size = 1.1),
        width=12,status="primary", solidHeader = T),
      
      br()
    )
  )
)

# 3.3 put UI together ---------------------------------------------------

ui <- 
  dashboardPage(header, sidebar, body )
