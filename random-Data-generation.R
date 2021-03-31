
# Packages -----------------------------------------------

library(data.table)
library(stringi)
library(feather)
library(lubridate)

# arguments for the function -----------------------------

item_names <- c("shaker_A","shaker_B","shaker_C","shaker_D","supplement_A","supplement_B","supplement_C","supplement_D","supplement_E","supplement_F","bag_A","bag_B","bag_C","bag_D","bag_E","shoes_A","shoes_B","shoes_C","shoes_D","shoes_E","shoes_F","shoes_G","shoes_H","bike_A","bike_B","bike_C","bike_D","bike_E","bike_F","bike_G","bike_H","bike_I","bike_J","bike_K","bike_L","scooter_A","scooter_B","scooter_C","scooter_D","scooter_E","scooter_F","scooter_G","scooter_H","scooter_I","scooter_J","Dumbbell_A","Dumbbell_B","Dumbbell_C","Dumbbell_D","Dumbbell_E","jacket_A","jacket_B","jacket_C","jacket_D","jacket_E","watch_A","watch_B","watch_C","watch_D","watch_E","watch_F","watch_G","watch_H","treadmill_A","treadmill_B","treadmill_C","treadmill_D","treadmill_E","treadmill_F","treadmill_G","w_machine_A","w_machine_B","w_machine_C","w_machine_D","w_machine_E","w_machine_F","w_machine_G","w_machine_H","w_machine_I","w_machine_J")
store_names <- c("Evere","Charleroi","Mons","Verviers","Wavre","Louviere","Bruxelles","Namur","Nivelles","Hasselt","Alleur","OLEN","Ghent", "Roeselare","Dunkerque","Maldegem","Willebroek","KORTRIJK","TOURNAI","MAASMECHELEN")

# Pair Numbers !
# Number of stores
Nb_store <- 20 # 20 Max !
# Total number of items in all stores
Nb_items <- 70 # 80 max !
# Number of day of the period analyzed  
Nb_jours <- 365*9
# Mean number of transaction per day, per store and per item 
mean_Nb_Transactions <- 0.442
# Random seed
seed <- 100

# Function -----------------------------------------------

Generate_random_data <- function(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                                 seed, store_names ,item_names){
  
  # Set random seed
  set.seed(seed)
  
  # Total number of transaction (= number of row of the data frame)
  Nb_Transactions <- Nb_items * Nb_jours * mean_Nb_Transactions * Nb_store
  print(paste0("nrow = ", as.character(Nb_Transactions)))
  ### Initialization
  set.seed(seed)
  DT = data.table(
    ### Store ID
    but_idr_business_unit = sample(stri_pad(1:Nb_store, 4, pad = "0"),
                                   size =  Nb_Transactions,
                                   replace = TRUE, # sampling be with replacement
                                   prob = c(abs(rnorm(Nb_store/2,4,3)), # sampling in different proportions
                                            abs(rnorm(Nb_store/2,5,2))
                                   ) 
    ),
    ### Transaction ID
    the_transaction_id = stri_pad(1:Nb_Transactions,
                                  8, pad = "0"),
    ### Item ID
    sku_idr_sku = sample(stri_pad(1:Nb_items, 5, pad = "0"),
                         size =  Nb_Transactions,
                         replace = TRUE, # sampling be with replacement
                         prob = c(abs(rnorm(Nb_items/2,3,2)), # sampling in different proportions
                                  abs(rnorm(Nb_items/2,6,4))
                         )
    ),
    ### Transaction Type
    tdt_type_detail = sample(c("sale","return"),
                             size = Nb_Transactions,
                             replace = TRUE, # sampling be with replacement
                             prob = c(.75,.25) # assuming that 25% of transactions concern items returned
    )
  )
  print("Data.table created")
  
  ### add store names (for more realistic visualizations)
  DT[,store_name:= factor(but_idr_business_unit)]
  levels(DT$store_name) <- store_names[1:Nb_store]
  DT[,store_name:=as.vector(store_name)]
  print("store_name added")
  
  ### add item names (for more realistic visualizations)
  DT[,item_name:= factor(sku_idr_sku)]
  levels(DT$item_name) <- item_names[sample(1:Nb_items)]
  DT[,item_name:=as.vector(item_name)]
  print("item_name added")
  
  ### add Prices of items
  DT[,prices:= factor(sku_idr_sku)]
  levels(DT$prices) <- sample(1:1000,
                              Nb_items,
                              prob = sort(abs(rnorm(1000,0,50))
                                          , decreasing = T)
  )
  DT[,prices:=as.numeric(as.vector(prices))]
  print("prices added")
  
  
  ### Add Dates (with accentuating the weekend/covid-19 Crisis/ Summer&spring months effects
  ### So that we have a better visualization on our heatmap
  
  Dates <- seq(from = as.Date("2021-03-31") - Nb_jours, # Historic data ending at 2021-03-31
               to = as.Date("2021-03-31"),
               by ="day")
  
  # Creating probability vector giving advantage to the Week end
  days <- lubridate::wday(Dates)
  probs_weekend <- sapply(days,function(day){
    case_when(
      day==1 | day == 7  ~ 0.57, #(weekends)
      TRUE  ~ 0.43,
    )
  })
  
  # Creating probability vector reducing sales for covid-19
  years <- lubridate::year(Dates)
  probs_covid <- sapply(years,function(year){
    case_when(
      year==2020 ~ 0.7, #(Covid 19 year)
      TRUE  ~ 0.97,
    )
  })
  
  # Creating probability vector giving advantage to spring/summer months
  months <- lubridate::month(Dates)
  probs_month <- sapply(months,function(month){
    case_when(
      month %in% c(4,5,6,7,8) ~ 1.15, #(Spring/summer months effect)
      TRUE  ~ 0.95,
    )
  })
  
  # Putting all probabilities together
  probs <- probs_month * probs_covid * probs_weekend
  
  
  DT[,the_date_transaction:= sample(Dates,
                                    size = Nb_Transactions,
                                    replace = TRUE, # sampling be with replacement
                                    prob = probs
  )
  ]
  
  
  ### Add quantities
  DT[,quantity:= round(abs((rnorm(Nb_Transactions,0,3.8)))+0.51)]
  
  ### Adding the Covid-19 Crisis year effect on quantities as well
  DT[lubridate::year(the_date_transaction)==2020, quantity := round(quantity * 0.80)]
  
  ### Adding random effect between different years
  for (y in unique(lubridate::year(DT$the_date_transaction))){
    DT[lubridate::year(the_date_transaction) == y,
       quantity := round(quantity * runif(1,0.7,1.3))]
  }
  
  ### Adding spring and summer effect
  DT[lubridate::month(the_date_transaction) %in% c(4,5,6,7,8),
     quantity := round(quantity * 1.5)]
  
  ### Adding randomness between different months
  for (m in unique(lubridate::month(DT$the_date_transaction))){
    DT[lubridate::year(the_date_transaction) == y,
       quantity := round(quantity * runif(1,0.7,1.3))]
  }
  
  print("quantities added")
  
  ### Add Turnover
  DT[,turnover := quantity*prices]
  print("turnovers added")
  
  ### shuffle by rows
  DT <- DT[sample(nrow(DT))]
  
  ### save Feather
  setwd(choose.dir(caption = "Select a folder to save your Data.feather"))
  write_feather(DT,"Data-Decathlon.feather")
  print("Data saved")
  
  return("done")
}


# Test function ------------------------------------

Generate_random_data(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                     seed, store_names = store_names, item_names = item_names )

