# Testing function at the bottom of the file

# 0. Building value box function ------------------------------------------

### Arguments : DT, the data set.

Build_valuebox <- function(DT){
  
  # turnover of sales
  turnover_sales <- 
    (sum(
      DT[tdt_type_detail=='sale'][,c(turnover)]
    ) / 10^6) %>% 
    round(3) %>% 
    as.character() %>% 
    paste0(" m")
  
  # turnover of returns
  turnover_returns <- 
    (sum(
      DT[tdt_type_detail=='return'][,c(turnover)]
    ) / 10^6) %>% 
    round(3) %>% 
    as.character() %>% 
    paste0(" - ",.," m")
  
  # Number of transactions of sales
  N_transactions_sales <- 
    (nrow(DT[tdt_type_detail=='sale']) /10^3) %>% 
    round(3) %>% 
    as.character() %>% 
    paste0(" k")
  
  # Number of transactions of returns
  N_transactions_returns <-
    (nrow(DT[tdt_type_detail=='return']) / 10^3) %>% 
    round(3) %>% 
    as.character() %>% 
    paste0(" - ",.," k")
  
  # Value box functions
  valuebox_turnover_sales <-
    shinydashboard::valueBox(value = turnover_sales, subtitle = tags$span("Total turnover of sales",style="font-size: 1.5em;"),
                             icon = icon("euro-sign"),color = "green")
  
  valuebox_turnover_returns <-
    shinydashboard::valueBox(value = turnover_returns, subtitle = tags$span("Total turnover of returns",style="font-size: 1.5em;"),
                             icon = icon("euro-sign"),color = "red")
  
  valuebox_transactions_sales <-
    shinydashboard::valueBox(value = N_transactions_sales, subtitle = tags$span("Total transaction of sales",style="font-size: 1.5em;"),
                             icon = icon("shopping-cart"),color = "green")
  
  valuebox_transactions_returns <-
    shinydashboard::valueBox(value = N_transactions_returns, subtitle = tags$span("Total transaction of returns",style="font-size: 1.5em;"),
                             icon = icon("shopping-cart"),color = "red")
  
  # Return list of value box functions
  list(
    valuebox_turnover_sales = valuebox_turnover_sales,
    valuebox_turnover_returns = valuebox_turnover_returns,
    valuebox_transactions_sales = valuebox_transactions_sales,
    valuebox_transactions_returns = valuebox_transactions_returns
  )
}





# 1.a. Build Evolution KPIs Gauges -------------------------------------------

### Arguments :
### DT = Filtered Data
### DT_Reference = The initial table (= {Data} in global.R)
### I use this one to create the filtered data for the reference period of time
### for which I would like to calculate the old KPI's and therefore be able to
### calculate the evolution of the KPI's for the actual DT

Build_KPI_Evolution_gauge <- function(DT, DT_Reference){
  
  # 0. First o fall : Filter by sales
  DT <- filter(DT, tdt_type_detail=="sale")
  DT_Reference <- filter(DT_Reference, tdt_type_detail=="sale")
  
  # 1.1 Filter Data for the last period of same length ---------------------
  
  ### I start by calculating the length of DT period
  time_diff <-
    difftime(
      max(DT$the_date_transaction),min(DT$the_date_transaction)
    )
  
  ### I then apply the filter
  DT_Reference <-
    filter(
      DT_Reference,
      item_name %in% unique(DT$item_name) &
        store_name %in% unique(DT$store_name) &
        the_date_transaction >= min(DT$the_date_transaction) - time_diff &
        the_date_transaction  <=  min(DT$the_date_transaction)
    )
  
  # 1.2 Calculate the KPI for DT and DT_reference and therefore the evolution ---- 
  
  ### Mean turnover by total number of transactions (Average Basket, AB)
  
  KPI_AB_DT <- 
    sum(DT$turnover) / nrow(DT)
  
  KPI_AB_DT_Reference <- 
    sum(DT_Reference$turnover) / nrow(DT_Reference)
  
  KPI_AB_Evolution <- KPI_AB_DT / KPI_AB_DT_Reference
  
  ### Mean number of Transactions per day (TPD)
  
  KPI_TPD_DT <- 
    nrow(DT) / unique(DT$the_date_transaction) %>% length()
  
  KPI_TPD_DT_Reference <- 
    nrow(DT_Reference) / unique(DT_Reference$the_date_transaction) %>% length()
  
  KPI_TPD_Evolution <- KPI_TPD_DT / KPI_TPD_DT_Reference
  
  ### Mean number of Quantities per day (QPD, )
  
  KPI_QPD_DT <- 
    sum(DT$quantity) / length(unique(DT$the_date_transaction))
  
  KPI_QPD_DT_Reference <- 
    sum(DT_Reference$quantity) / length(unique(DT_Reference$the_date_transaction))
  
  KPI_QPD_Evolution <- KPI_QPD_DT / KPI_QPD_DT_Reference
  
  # 1.3 Building Gauges with flexdashboard ---------------------------------
  
  # KPI_AB
  
  Gauge_AB <-
    flexdashboard::gauge(
      value = round(
        abs(1 - KPI_AB_Evolution)*100,
        2
      ),
      min = 0,
      max = 100,
      sectors = 
        flexdashboard::gaugeSectors(
          colors =
            ifelse(KPI_AB_Evolution < 1,
                   "red",
                   "green")
        ),
      symbol = " %",
      label = ''
    )
  
  # KPI_TPD
  
  Gauge_TPD <-
    flexdashboard::gauge(
      value = round(
        abs(1 - KPI_TPD_Evolution)*100,
        2
      ),
      min = 0,
      max = 100,
      sectors = 
        flexdashboard::gaugeSectors(
          colors =
            ifelse(KPI_TPD_Evolution < 1,
                   "red",
                   "green")
        ),
      symbol = " %",
      label = ''
    )
  
  # KPI_QPD
  
  Gauge_QPD <-
    flexdashboard::gauge(
      value = round(
        abs(1 - KPI_QPD_Evolution)*100,
        2
      ),
      min = 0,
      max = 100,
      sectors = 
        flexdashboard::gaugeSectors(
          colors =
            ifelse(KPI_QPD_Evolution < 1,
                   "red",
                   "green")
        ),
      symbol = " %",
      label = ''
    )
  
  ### Return the list of gauges
  
  list(
    Gauge_AB = Gauge_AB,
    Gauge_TPD = Gauge_TPD,
    Gauge_QPD = Gauge_QPD
  )
}


# 1.b Building leaflet map function ------------------------------------------

### Arguments : 
### DT = Filtered Data
### radius = The performance sales indicator that corresponds to the circle radius
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"

Build_leaflet_map <- function(DT, radius, transaction_type){
  
  # 1.1. Filter by transaction_type, Group by store_name and summarize by radius------
  DT <- 
    filter(
      DT,
      tdt_type_detail==transaction_type
    ) %>%
    group_by(store_name) %>% 
    summarize(value = 
                ifelse(
                  # If the argument 'radius' we summarize through is a qualitative variable in DT, then calculate
                  # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
                  is.character(.data[[radius]]),
                  length(.data[[radius]]),
                  sum(.data[[radius]]))) %>%
    as.data.frame()
  
  # 1.2. Attribution of Lat/Long to stores (to save storage)--------------------------
  
  # Get latitude and longitude Data (defined in global.R)
  cities <- cities
  names(cities)[1] <- "store_name"
  
  # Add columns of latitude and longitude
  DT <- merge(cities,DT)
  
  # 1.3. Build leaflet map depending on arguments -------------------------------------
  
  # Palette 
  pal <- colorNumeric(palette = ifelse(transaction_type=='sale',
                                       'Greens',
                                       'Reds'),
                      DT$value)
  
  # Map (Note : Carto-Positron is licensed !)
  leaflet(DT) %>%
    addProviderTiles(providers$ CartoDB.Positron) %>%
    addCircles(lng = ~Long, lat = ~Lat, fillColor = ~pal(value), fillOpacity = 0.73,
               color = ifelse(transaction_type=='sale','green', 'red'),
               stroke = TRUE , weight = 1, radius = 5000,
               popup = ~paste0("Decathlon ", store_name, ' - ',
                               
                               case_when(
                                 radius=='turnover' ~ paste0(
                                   round(value/10^3,2),
                                   " K EUR"),
                                 radius=='the_transaction_id' ~ paste0(
                                   value, " Transactions"),
                                 radius=='quantity' ~ paste0(
                                   value," Units ",
                                   ifelse(transaction_type=='sale','Sold','Returned'))
                               )
               )
    ) %>% 
    clearBounds()
}

# 2. Temporal heatmap function------------------------------------------------

### Arguments : 
### DT = Filtered Data
### X = X axis of heat map -- e.g. : "month"
### intensity = The Heat Map intensity variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"

Build_HC_Temporal_heatmap <- function(DT,X,intensity,transaction_type){
  
  # 2.1 Filtering Data depending on the transaction type ---------------------
  DT <- filter(
    DT,
    tdt_type_detail == transaction_type
  )
  
  # 2.2 Color of heatmap depending on type of transaction --------------------
  HT_color <- ifelse(transaction_type=='sale','#07AE6B','red')
  
  # 2.3. If X is equal to 'month' or 'quarter' -------------------------------
  # then Y will  be expressed on years
  # In fact, the whole plot approach is not the same as for X = 'days'
  # Consequence : two different highchart functions depending on the value of X
  
  if(X %in% c('month','quarter')){
    
    ### we add day/month/year/quarter/ columns
    DT[,month:= lubridate::month(the_date_transaction,label = TRUE, locale = "English")]
    DT[,year:= as.character(
      lubridate::year(the_date_transaction)
    )]
    DT[,quarter:= paste0("Qr.",
                         quarter(the_date_transaction)
    )]
    
    # Y Axis
    Y = "year"
    
    # 2.3.1 Java Script function to format the heatmap hovering box ---------
    
    tooltip_formater <- JS(paste0("function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
              }
          return '<b>' + getPointCategoryName(this.point, 'x') + '-' +
          getPointCategoryName(this.point, 'y') + '</b>' +'<br>'+
          'Total  of {",intensity,"} : ' + '<b>' + this.point.value + '<b>';
    }"))
    
    # 2.3.3 Highcharter -----------------------------------------------------
    DT %>%
      group_by(.data[[X]],.data[[Y]]) %>%
      summarize(intensity= ifelse(
        # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
        # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
        is.character(.data[[intensity]]),
        length(.data[[intensity]]),
        sum(.data[[intensity]])
      )) %>%
      hchart("heatmap",
             hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
             marginTop = 0,
             marginBottom = 0) %>%
      # Customizing exporting button (global.R)
      hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'),
                   buttons = JS_enable_exporting) %>%
      hc_yAxis(title= "null", reversed = T) %>%
      hc_xAxis(title= "null") %>%
      hc_tooltip(formatter = tooltip_formater,
                 borderWidth = 3.5) %>%
      hc_colorAxis(
        min = 0,
        minColor= '#FFFFFF',
        maxColor= HT_color # Intensity Color
      ) %>%
      hc_legend(
        align= 'right',
        layout= 'vertical',
        margin= 0,
        verticalAlign= 'top',
        y= 25,
        symbolHeight= 320
      )
  } else{
    
    # 2.4. If X is equal to 'Date'-------------------------------------------
    
    # Y Axis will be expressed in weeks
    # We add then the the weeks column
    DT[,aggregated_date_week:= floor_date(the_date_transaction, "week",
                                          week_start = getOption("lubridate.week.start", 1))
    ]
    Y = "aggregated_date_week"
    # We also add the week days column
    # (Attribute for every day the "Monday" date of the week it's a part)
    DT[,day:= lubridate::wday(the_date_transaction, label = TRUE, locale = "English")]
    # Correct the weekday level of Lubridate (starting with Monday)
    DT[,day:= factor(day,levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))]
    
    # 2.4.1 JS functions to format the heatmap ------------------------------
    
    ### format the tooltip (hovering box)
    tooltip_formater <- JS(paste0(
      "
        // correction of aggregated week dates generated by {lubridate::floor_date}
        // In the generating random Data script
        // I want to have on hovering the exact date of the hovered day
        // Not the floor_date Date.
         
        function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
            }
              var day_w = getPointCategoryName(this.point, 'x');
              var date = getPointCategoryName(this.point, 'y');
              var day = parseInt(date.substring(8,10));
              var month = parseInt(date.substring(5,7)) - 1; // JS begin from 0
              var year = parseInt(date.substring(0,4));
              var date_utc = Date.UTC(year, month, day);
              if(day_w==='Tue'){
              date_utc = date_utc + 3600000*24*1;
              }
              if(day_w==='Wed'){
              date_utc = date_utc + 3600000*24*2;
              }
              if(day_w==='Thu'){
              date_utc = date_utc + 3600000*24*3;
              }
              if(day_w==='Fri'){
              date_utc = date_utc + 3600000*24*4;
              }
              if(day_w==='Sat'){
              date_utc = date_utc + 3600000*24*5;
              }
              if(day_w==='Sun'){
              date_utc = date_utc + 3600000*24*6;
              }
              var date_normal = new Date(date_utc);
              var formated_date = date_normal.toLocaleDateString();
          return '<b>' + getPointCategoryName(this.point, 'x') + ' - ' +
          formated_date + '</b>' +'<br>'+
          'Total  of {",intensity,"} : ' + '<b>' + this.point.value + '<b>';
    }
               ")
    )
    
    ### format the Xaxis label (We want a yyyy/mm) format instead of aggregated date week
    Yaxis_formater <- JS("
                      // We want to show YYYY/MM instead of aggregated date week
                      
                      function () {
                      var monthNames = [ 'null', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];
                      var month = monthNames[
                                  parseInt(this.value.substring(5,7))];
                      var year = this.value.substring(0,4);
                      return year.concat('-',month);}"
    )
    
    ### easing the display of Xaxis labels when the date range is too large
    ### Depending of the number of week to display
    n_weeks <- length(unique(DT$aggregated_date_week))
    tickPositions <- case_when(
      n_weeks<35                   ~    list(seq(0 , n_weeks-1 , by=1)),
      n_weeks>=35 & n_weeks<80     ~    list(unique(c(seq(0 , n_weeks-1 , by=3),n_weeks-1))),
      n_weeks>=80 & n_weeks<120    ~    list(unique(c(seq(0 , n_weeks-1 , by=5),n_weeks-1))),
      n_weeks>=120 & n_weeks<160   ~    list(unique(c(seq(0 , n_weeks-1 , by=7),n_weeks-1))),
      n_weeks>=160                 ~    list(unique(c(seq(0 , n_weeks-1 , by=9),n_weeks-1))),
    )
    tickPositions <- tickPositions[[1]]
    
    # 2.4.2 Highcharter ---------
    DT %>%
      group_by(.data[[X]],.data[[Y]]) %>%
      summarize(intensity= ifelse(
        # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
        # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
        is.character(.data[[intensity]]),
        length(.data[[intensity]]),
        sum(.data[[intensity]])
      )) %>%
      hchart("heatmap",
             hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
             marginTop = 0,
             marginBottom = 0) %>%
      # Customizing exporting button
      hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'),
                   buttons = JS_enable_exporting) %>% 
      hc_xAxis(title= "") %>%
      hc_yAxis(title= "",
               labels = 
                 list(formatter= Yaxis_formater
                 ),
               reversed = T,
               tickPositions = tickPositions
      ) %>%
      hc_tooltip(formatter = tooltip_formater,
                 borderWidth = 3.5) %>%
      hc_colorAxis(
        min = 0,
        minColor= '#FFFFFF',
        maxColor= HT_color # Intensity Color
      ) %>%
      hc_legend(
        align= 'right',
        layout= 'vertical',
        margin= 0,
        verticalAlign= 'top',
        y= 25,
        symbolHeight= 320
      )
  }
}

# 3. Line chart function-----------------------------------------------------

### Arguments : 
### DT = Data
### X = X axis of Line chart, Default = 'the_date_transaction'
### Y = The Y Axis lines variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### group = the variable we group DT through, always = 'item_name' 
### transaction_type : The type of transaction, takes either "sale" or "return"
### aggregation_period : The aggregation date label (by days, months, years...)

Build_Date_line_charts <- function(DT, X, Y, group, transaction_type, aggregation_period){
  
  # 3.1 Filtering Data depending on the transaction type --------------------
  DT <- filter(
    DT,
    tdt_type_detail == transaction_type
  )
  
  # 3.3 Highcharter ---------------------------------------------------------
  DT %>% group_by(.data[[group]],
                  Date = floor_date(.data[[X]], aggregation_period)
  ) %>% 
    summarize(value=ifelse(
      # If the argument 'Y' we summarize through is a qualitative variable in DT, then calculate
      # the occurrence (e.g: the_transaction_id). otherwise, calculate the sum (e.g: turnover or quantity)
      is.character(.data[[Y]]),
      length(.data[[Y]]),
      sum(.data[[Y]])
    )) %>%  
    hchart("spline",
           hcaes(x = Date, y = value, group = .data[[group]]),
           marginTop = 100,
           marginBottom = 0
    ) %>% 
    hc_xAxis(title = "") %>% 
    hc_yAxis(title = list(text=paste0('Total of {',Y,'}')))%>% 
    hc_tooltip(shared = TRUE,
               crosshairs = TRUE,
               followPointer = T,
               borderColor = "grey")%>%
    # Customizing exporting button
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'), 
                 buttons = JS_enable_exporting)
}


# 4. correlation heatmap function--------------------------------------------

### Arguments : 
### DT = Filtered Data
### X = X axis of heat map -- e.g. : "store_name"
### Y = Y axis of heat map -- e.g. : "item_name"
### intensity = The Heat Map intensity variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"

Build_HC_Heatmap_Correlation <- function(DT,X,Y,intensity,transaction_type){
  
  # 4.0 Filtering Data depending on the transaction type  -------------------
  DT <- filter(
    DT,
    tdt_type_detail == transaction_type
  )
  
  # 4.1 Color of heatmap depending on type of transaction -------------------
  HT_color <- ifelse(transaction_type=='sale','#07AE6B','red')
  
  # 4.2 Some JS functions to format the hovering box (hc_tootlip) -----------
  
  tooltip_formater <- JS(paste0("
        function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
              }
          return  '",X,": '  + '<b>' + getPointCategoryName(this.point, 'x') + '</b>' +'<br>'+
          '",Y,": ' + '<b>' + getPointCategoryName(this.point, 'y') + '</b> <br>'+
          'Total  of {",intensity,"} : ' + '<b>' + this.point.value + '<b>';
    }"))
  
  # 4.3 Highcharter ---------------------------------------------------------
  DT %>%
    group_by(.data[[X]],.data[[Y]]) %>%
    summarize(intensity= ifelse(
      # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
      # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
      is.character(.data[[intensity]]),
      length(.data[[intensity]]),
      sum(.data[[intensity]])
    )) %>%
    hchart("heatmap",
           hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
           marginTop = 0,
           marginBottom = 0) %>%
    # Customizing exporting button
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'),
                 buttons = JS_enable_exporting) %>% 
    hc_yAxis(title= "") %>%
    hc_xAxis(title= "") %>%
    hc_tooltip(formatter = tooltip_formater,
               borderWidth = 3.5) %>%
    hc_colorAxis(
      min = 0,
      minColor= '#FFFFFF',
      maxColor= HT_color # Intensity Color
    ) %>%
    hc_legend(
      align= 'right',
      layout= 'vertical',
      margin= 0,
      verticalAlign= 'top',
      y= 25,
      symbolHeight= 320
    )
}



# 5. Bar plot for store_names comparaison-------------------------------------

### Arguments : 
### DT = Filtered Data
### X = X axis of heat map -- e.g. : "store_name"
### Y = The Bar plot Y Axis variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### group : The variable we group through, default = tdt_type_detail
### Percent = whether or not you want to display in percentage
### horizontal = whether or not you want to display horizontal lines


Build_HC_Barplot <- function(DT, X, Y, group, percent, horizontal){
  # (Testing Function bellow)
  
  type_HC <- ifelse(horizontal, 'bar','column')
  
  DT %>%
    group_by(.data[[X]],.data[[group]]) %>%
    summarize(value = ifelse(
      # If the argument 'Y' we summarize through is a qualitative variable in DT, then calculate
      # the occurrence (e.g: the_transaction_id). otherwise, calculate the sum (e.g: turnover or quantity)
      is.character(.data[[Y]]),
      length(.data[[Y]]),
      sum(.data[[Y]])
    )) %>% 
    hchart(type_HC,
           hcaes(x = .data[[X]], y = value, group= .data[[group]]),
           marginTop = 100,
           marginBottom = 0
    ) %>%
    hc_xAxis(title = '') %>%
    hc_yAxis(title = 
               list(text = paste0('Total {',Y,'}')
               )
    )%>% 
    hc_colors(c("red", "#07AE6B")) %>%
    # Customizing exporting button
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'),
                 buttons = JS_enable_exporting) %>% 
    hc_plotOptions(series = list(pointPadding=0.05, groupPadding= 0.09,
                                 stacking = ifelse(percent,
                                                   list('percent'),
                                                   list(NULL))[[1]]
    )
    ) %>% 
    hc_tooltip(shared = TRUE,
               crosshairs = TRUE,
               followPointer = T,
               borderColor = "grey")
}




# TESTING FUNCTIONS -------------------------------------------------------

# Data <- read_feather("Data-Decathlon.feather")


# Build_valuebox(Data)


# Build_leaflet_map(DT = Data,
#                   radius = 'turnover',
#                   transaction_type = 'sale')


# Build_HC_Temporal_heatmap(DT = Data, X = 'month', intensity = 'turnover',
#                                 transaction_type = 'return')


# Build_Date_line_charts(DT = Data,
#                        X = 'the_date_transaction',
#                        Y = 'quantity',
#                        group = 'item_name',
#                        transaction_type = 'return',
#                        aggregation_period = 'years')


# Build_HC_Heatmap_Correlation(DT = Data, X = 'store_name',Y = 'item_name',
#                             intensity = 'the_transaction_id',
#                              transaction_type = 'sale')


# Build_HC_Barplot(DT = Data, X = "store_name", Y = "quantity",
#                 group= "tdt_type_detail", percent = F,horizontal=F)
