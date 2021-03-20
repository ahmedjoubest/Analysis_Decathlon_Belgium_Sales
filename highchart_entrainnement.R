#install packages matnsach
library(dplyr)
library(highcharter)
library(data.table)
library(feather) # Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
# ALLL7777WWWAAAA =)
# commenter had lqlawi fl app ;)
#golih had nwita f mail anak z3ma wa3R f js donc sehlat 3lik lqadya

setwd("C:/Users/ajoue/Desktop/DECATHLON_offre/Decathlon_projets/")


DT <- data.table(read_feather("Data-Decathlon.feather"))
DT <- DT[sku_idr_sku %in% unique(DT$sku_idr_sku)[1:5],]

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
hchart(DT %>%
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
  )






















# 1. Create data with missing month












heatmap_data <- data.table(Fruit = c('apple', 'banana', 'orange', 'pear'),
                           Month = c('2019-01-01', '2019-03-01', '2019-04-01', '2019-05-01'),
                           Sales = c(2, 4, 6, 8))
heatmap_data
highcharter::hchart(heatmap_data,
                    type = "heatmap",
                    highcharter::hcaes(x = Month, y = Fruit, value = Sales))


# 2. Attempt with dummy entry included

heatmap_data <- data.table(Fruit = c('apple', NA, 'banana', 'orange', 'pear'),
                           Month = c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01'),
                           Sales = c(2, NA, 4, 6, 8))
heatmap_data
highcharter::hchart(heatmap_data,
                    type = "heatmap",
                    highcharter::hcaes(x = Month, y = Fruit, value = Sales)) %>%
  
  highcharter::hc_tooltip(borderWidth = 4)













heatmap_data <- data.table(Fruit = c('apple', 'banana', 'orange', 'pear'),
                           Month = c(0, 2, 3, 4),
                           Sales = c(2, 4, 6, 8))
heatmap_data
hchart(heatmap_data,
       type = "heatmap",
       ) %>%
  hc_xAxis(categories = month.abb)









heatmap_data <- data.table(Fruit = c('apple', 'banana', 'orange', 'pear'),
                           Month = c('2019-01-01', '2019-03-01', '2019-04-01', '2019-05-01'),
                           Sales = c(2, 4, 6, 8))
heatmap_data$Month <- datetime_to_timestamp(as.Date(heatmap_data$Month, format = "%Y-%m-%d")) 
heatmap_data



highchart() 
  hchart(heatmap_data,
         type = "heatmap",
         hcaes(x = Month, y = Fruit, value = Sales)) %>%
  hc_add_series(
    name = "zbi",
    data = heatmap_data,
    type = 'heatmap',
    hcaes(x = Month, y = Fruit, value = Sales),
    colsize = 24 * 3600 * 1000 * 30,
    tooltip = list(
      pointFormat = '{point.x:%b, %Y}, {point.y}: {point.value}'
    ),
    showInLegend = FALSE,
    borderWidth = 0
  ) %>%
  hc_xAxis(
    type = 'datetime'
  ) %>%
  hc_yAxis(
    categories = heatmap_data$Fruit
  )
.
