##############################################################################
### Context:                                                               ###
### To keep the app.R maintainable, here are created the graphic functions ###


# 1. correlation heatmap function--------------------------------------------

### Arguments : 
### DT = Data
### X = X axis of heat map -- e.g. : "store_name"
### Y = Y axis of heat map -- e.g. : "item_name"
### intensity = The Heat Map intensity variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"
### P.S: except DT, all other arguments are strings

Make_HC_Heatmap_Correlation <- function(DT,X,Y,intensity,transaction_type){
# (Testing Function bellow)

  # 1.0 Filtering Data depending on the transaction type  ------
  DT <- subset(
    DT,
    tdt_type_detail == transaction_type
  )

  # 1.1 Color of heatmap depending on type of transaction ------
  HT_color <- ifelse(transaction_type=='sale','#07AE6B','red')
  
  # 1.2 JS function to format the heatmap hovering box (hc_tootltip)------
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
  # 1.3 Highcharter -------
    hchart(DT %>%
             group_by(.data[[X]],.data[[Y]]) %>%
             summarize(intensity= ifelse(
               # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
               # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
               is.character(.data[[intensity]]),
               length(.data[[intensity]]),
               sum(.data[[intensity]])
             )),
         "heatmap",
         hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
         marginTop = 0,
         marginBottom = 0) %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'), # Customizing exporting button
                 buttons = JS('{
                contextButton: {
                    symbolStroke: "white",
                    theme: {
            fill:"#3C8DBC"
        }
                }
            }')) %>% 
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


# 2. Chronological heatmap function-----------------------------------------

### Arguments : 
### DT = Data
### X = X axis of heat map -- e.g. : "month"
### intensity = The Heat Map intensity variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"
### P.S: except DT, all other arguments are strings

Make_HC_Chronological_heatmap <- function(DT,X,intensity,transaction_type){
  # (Testing function bellow)
  
  # 2.1 Filtering Data depending on the transaction type  ------
  DT <- subset(
    DT,
    tdt_type_detail == transaction_type
  )
  
  # 2.2 Color of heatmap depending on type of transaction  ------
  HT_color <- ifelse(transaction_type=='sale','#07AE6B','red')
  
  # 2.3. If X is equal to 'month' or 'quarter', then Y will  be expressed on years -----
        # In fact, the whole highcharting approach is very different between this two cases
        # Consequence : a different charting function depending on the value of X
  
  if(X %in% c('month','quarter')){
    
  Y = "year"
  
  # 2.3.1 JS functions to format the heatmap hovering box (hc_tooltip) -----
  tooltip_formater <- JS(paste0("
        function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
              }
          return '<b>' + getPointCategoryName(this.point, 'x') + '-' +
          getPointCategoryName(this.point, 'y') + '</b>' +'<br>'+
          'Total  of {",intensity,"} : ' + '<b>' + this.point.value + '<b>';
    }
               "))
  # 2.3.2 Highcharter --------
  hchart(DT %>%
           group_by(.data[[X]],.data[[Y]]) %>%
           summarize(intensity= ifelse(
             # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
             # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
             is.character(.data[[intensity]]),
             length(.data[[intensity]]),
             sum(.data[[intensity]])
           )),
         "heatmap",
         hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
         marginTop = 0,
         marginBottom = 0) %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'), # Customizing exporting button
                 buttons = JS('{
                contextButton: {
                    symbolStroke: "white",
                    theme: {
            fill:"#3C8DBC"
        }
                }
            }')) %>% 
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
  
    
  # 2.4 If X is equal to 'month' or 'quarter', then Y will  be expressed on years --------
  
  Y = "aggregated_date_week"
  
  # 2.4.1 JS functions to format the heatmap -------
  ### format the tooltip(hovering box)
  tooltip_formater <- JS(paste0(
  "
        //correction of aggregated week dates
        //(we want to have on hovering the exact date of the hovered day)
         
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
  hchart(DT %>%
           group_by(.data[[X]],.data[[Y]]) %>%
           summarize(intensity= ifelse(
             # If the argument 'intensity' we summarize through is a qualitative variable in DT, then calculate
             # the occurrence (e.g: the_transaction_id). otherwise : calculate the sum (e.g: turnover or quantity)
             is.character(.data[[intensity]]),
             length(.data[[intensity]]),
             sum(.data[[intensity]])
           )),
         "heatmap",
         hcaes(x = .data[[X]], y = .data[[Y]], value= intensity),
         marginTop = 0,
         marginBottom = 0) %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'), # Customizing exporting button
                 buttons = JS('{
                contextButton: {
                    symbolStroke: "white",
                    theme: {
            fill:"#3C8DBC"
        }
                }
            }')) %>% 
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

# 3. Barplot for store_names comparaison--------------------------------------

### Arguments : 
### DT = Data
### X = X axis of heat map -- e.g. : "store_name"
### Y = Y axis of heat map -- e.g. : "item_name"
### intensity = The Heat Map intensity variable, value token for summarizing the grouped DT
###         if the variable is qualitative (i.e. "the_transaction_id"), then it calculates the occurrence
###         which means: {length(the_transaction_id))}. otherwise, if the argument is quantitative
###          (i.e. "turnover"), then it calculates the total sum, which means: sum(turnover).
### transaction_type : The type of transaction, takes either "sale" or "return"
### P.S: except DT, all other arguments are strings

Make_HC_Barplot <- function(DT, X, Y, group, percent, horizontal){
  # (Testing Function bellow)
  
  type_HC <- ifelse(horizontal, 'bar','column')
  
  hchart(DT %>%
           group_by(.data[[X]],.data[[group]]) %>%
           summarize(value = ifelse(
             # If the argument 'group' we *summarize* through is a qualitative variable in DT, then calculate
             # the occurrence (e.g: the_transaction_id). otherwise, calculate the sum (e.g: turnover or quantity)
             is.character(.data[[Y]]),
             length(.data[[Y]]),
             sum(.data[[Y]])
             )
             ),
         type_HC,
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
    hc_exporting(enabled = TRUE, formAttributes = list(target = '_blank'), # Customizing exporting button
                 buttons = JS('{
                contextButton: {
                    symbolStroke: "white",
                    theme: {
            fill:"#3C8DBC"
        }
                }
            }')) %>% 
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

# Make_HC_Heatmap_Correlation(DT = Data, X = 'store_name',Y = 'item_name',
#                             intensity = 'the_transaction_id',
#                              transaction_type = 'sale')

# Make_HC_Chronological_heatmap(DT = Data, X = 'month', intensity = 'turnover',
#                                 transaction_type = 'return')

# Make_HC_Columns(DT = Data, X = "store_name", Y = "quantity",
#                 group= "tdt_type_detail", percent = F)



# ship_name = 'ADASTRA'
# test <- get_data(ship_name)
# 
# data <- test %>%
#   get_longest_distance_between_two_points()

# 
# long1 = 55.73122
# lat1 = 20.80305
# long2 = 55.69270
# lat2 = 21.13447
# 
# a <- matrix(long1, lat1)
# b <- paste(long2, lat2)
# 
# distHaversine(a,b)
# 
# get_geo_distance(long1, lat1, long2, lat2, 'meter')  

