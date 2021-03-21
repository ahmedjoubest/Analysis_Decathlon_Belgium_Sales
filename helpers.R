##############################################################################
### Context:                                                               ###
### To keep the app.R maintainable, here are created the graphic functions ###


# 0. PACKAGES ------------------------------------------------------------

library(pander) # To transform strings to expressions (access columns as arguments)


# 1. correlation heatmap function--------------------------------------------

### Arguments : 
### DT = Data
### X = X axis of heat map -- e.g. : "store_name"
### Y = Y axis of heat map -- e.g. : "item_name"
### intensity = The Heat Map intensity, value token for summarizing the grouped DT -- e.g : "length(the_transaction_id)"
### transaction_type : The type of transaction -- e.g. : "sale"
### P.S: except DT, all arguments are strings

Make_HC_Heatmap_Correlation <- function(DT,X,Y,intensity,transaction_type){
# (Testing function bellow)

  # 1.0 Filtering Data depending on the transaction type  ------
  DT <- subset(
    DT,
    tdt_type_detail == transaction_type
  )
  
  # Getting the temporary function environment that we need later for our evals function
  # for more info : ?pander::evals
  env_function = new.env()
  
  # 1.1 Capturing Evaluation information of strings  ------
  
  # To access columns from arguments (#quick&dirty):
  # We start by capturing evaluation information of the group_by DT with pander::evals
  DT_group_by_Eval <- pander::evals(
    paste0('DT %>% group_by(',X,',',Y,') %>% summarize( intensity= ',intensity,')'),
    env = env_function
    )[[1]]$result
  # Same for hcaes() function
  hcaes <- pander::evals(paste0('hcaes(x = ',X,', y = ',Y,', value = intensity)'),
                         env = env_function
                         )[[1]]$result

  # 1.3 Color of heatmap depending on type of transaction  ------
  HT_color <- ifelse(transaction_type=='sale','#07AE6B','red')
  
  # 1.4 JS function to format the heatmap hovering box
  JS_Formater <- JS(paste0("
function () {
            function getPointCategoryName(point, dimension) {
              var series = point.series,
              isY = dimension === 'y',
              axis = series[isY ? 'yAxis' : 'xAxis'];
              return axis.categories[point[isY ? 'y' : 'x']];
              }
          return  '",X,": '  + '<b>' + getPointCategoryName(this.point, 'x') + '</b>' +'<br>'+
          '",Y,": ' + '<b>' + getPointCategoryName(this.point, 'y') + '</b> <br>'+
          '",intensity,": ' + '<b>' + this.point.value + '<b>';
    }"))
  # 1.5 Highcharting using the results of pander evals
  hchart(DT_group_by_Eval,
         "heatmap",
         hcaes,
         marginTop = 0,
         marginBottom = 0) %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_yAxis(title= "null") %>%
    hc_xAxis(title= "null") %>%
    hc_tooltip(formatter = JS_Formater,
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
      symbolHeight= 310
    )
  }

# TESTING FUNCTIONS -------------------------------------------------------
# Make_HC_Heatmap_Correlation(DT = Data, X = 'store_name',Y = 'item_name',
#                             intensity = 'length(the_transaction_id)',
#                              transaction_type = 'sale')
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

