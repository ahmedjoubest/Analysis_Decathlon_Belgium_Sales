library(data.table)
library(stringi)
library(feather)
library(lubridate)

# Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
item_names <- c("shaker_A","shaker_B","shaker_C","shaker_D","supplement_A","supplement_B","supplement_C","supplement_D","supplement_E","supplement_F","bag_A","bag_B","bag_C","bag_D","bag_E","shoes_A","shoes_B","shoes_C","shoes_D","shoes_E","shoes_F","shoes_G","shoes_H","bike_A","bike_B","bike_C","bike_D","bike_E","bike_F","bike_G","bike_H","bike_I","bike_J","bike_K","bike_L","scooter_A","scooter_B","scooter_C","scooter_D","scooter_E","scooter_F","scooter_G","scooter_H","scooter_I","scooter_J","Dumbbell_A","Dumbbell_B","Dumbbell_C","Dumbbell_D","Dumbbell_E","jacket_A","jacket_B","jacket_C","jacket_D","jacket_E","watch_A","watch_B","watch_C","watch_D","watch_E","watch_F","watch_G","watch_H","treadmill_A","treadmill_B","treadmill_C","treadmill_D","treadmill_E","treadmill_F","treadmill_G","w_machine_A","w_machine_B","w_machine_C","w_machine_D","w_machine_E","w_machine_F","w_machine_G","w_machine_H","w_machine_I","w_machine_J")
store_names <- c("Evere","Charleroi","Mons","Verviers","Wavre","Louviere","Bruxelles","Namur","Nivelles","Hasselt","Alleur","OLEN")


# Number of stores
Nb_store <- 12 # donner des maximum azbi !
# Total number of items in all stores (80 max)
Nb_items <- 80 # 80 max !
# Number of day of the period analyzed
Nb_jours <- 365*4
# Mean number of transaction per day, per store and per item 
mean_Nb_Transactions <- 0.2
# Random seed
seed <- 100

Generate_random_data <- function(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                                 seed, store_names ,item_names){
  
  # Set random seed
  set.seed(seed)
  
  # Total number of transaction (= number of row of the data)
  Nb_Transactions <- Nb_items * Nb_jours * mean_Nb_Transactions * Nb_store
  print(paste0("nrow = ", as.character(Nb_Transactions)))
  ### Initialization
  set.seed(seed)
  DT = data.table(
    ### Store ID
    but_idr_business_unit = sample(stri_pad(1:Nb_store, 4, pad = "0"),
                                    size =  Nb_Transactions,
                                    replace = TRUE, # sampling be with replacement
                                    prob = c(15:(15+Nb_store-1)) # sampling in different proportions
    ),
    ### Item ID
    sku_idr_sku = sample(stri_pad(1:Nb_items, 5, pad = "0"),
                          size =  Nb_Transactions,
                          replace = TRUE, # sampling be with replacement
                          prob = c(5:(5+Nb_items-1)) # sampling in different proportions
    ),
    ### Transaction ID
    the_transaction_id = stri_pad(1:Nb_Transactions,
                                   8,
                                   pad = "0"),
    ### Transaction Date
    the_date_transaction = sample(seq(from = as.Date("2015-01-01"), # Data starting from 2015-01-01
                                       to = as.Date("2015-01-01") + Nb_jours -1,
                                       by ="day"),
                                   size = Nb_Transactions,
                                   replace = TRUE # sampling be with replacement
    ),
    ### Transaction Type
    tdt_type_detail = sample(c("sale","return"),
                              size = Nb_Transactions,
                              replace = TRUE, # sampling be with replacement
                              prob = c(.90,.10) # assuming that 10% of transactions have items returned
    )
    )
    
  print("Data.table created")

  ### add store name (We take advantage of the fixed seed in order to have the same sample division of store ID ;)
  set.seed(seed)
  DT[,store_name:= sample( store_names[1:Nb_store],
                           size =  Nb_Transactions,
                           replace = TRUE, # sampling be with replacement
                           prob = c(15:(15+Nb_store-1))) # sampling in different proportions
  ]
  
  ### add item names (We take advantage of the fixed seed in order to have the same sample division of item ID ;)
  # sort based on prices (the elements of the vector item_name are in an increasing order)
  DT[,item_name:= sample( item_names[1:Nb_items],
                          size =  Nb_Transactions,
                          replace = TRUE, # sampling be with replacement
                          prob = c(5:(5+Nb_items-1))) # sampling in different proportions
  ]
  
  
  
  ### add Price of items
  # (We affect naturally low prices to items with big number of transaction)
  # We want to affect the range [0.5,300] Euro to the 70% most redundant items, [300, 1200] Euro to the next 25%
  # And [1200,3500] Euro for the last 5%
  DT <- DT[order(-sku_idr_sku)] # sorting based on items ID
  Counts_items <- DT[,.N,by=sku_idr_sku][,c(N)]
  # subset the vector to 3 parts based on that percentage
  Nb_item_70 <- 1:round((0.7*length(Counts_items)))
  Nb_item_25 <- round((0.7*length(Counts_items))+1):round((0.95*length(Counts_items)))
  Nb_item_05 <- round((0.95*length(Counts_items))+1):round((1.00*length(Counts_items)))
  # Generating random prices following the rules we fixed
  ID_items <- unique(DT$sku_idr_sku)
  DT[,prices := 
      unlist(sapply(ID_items,
                     function(X){
                       prices <- round(c(runif(length(Nb_item_70),
                                               0.5,
                                               300),
                                         runif(length(Nb_item_25),
                                               300,
                                               1200),
                                         runif(length(Nb_item_05),
                                               1200,
                                               3500)
                                            ),
                                      2)
                       Index_item <- which(X == ID_items)
                       rep(prices[Index_item],Counts_items[Index_item])
                     }))
       ]
  print("prices added")
  

  
  ### Add Quantity of items(same logic but we use sample instead of runif)
  # (We affect naturally big quantity to items with big number of transactions)
  # We want to affect the range [1,3] units to the 5% less redundant items, [1, 7] units to the next 25%
  # And [1,15] units for the last 70%
  DT <- DT <- DT[order(-sku_idr_sku)] # sorting based on items ID
  Counts_items <- DT[,.N,by=sku_idr_sku][,c(N)]
  # subset the vector to 3 parts based on that percentage
  Nb_item_70 <- 1:round((0.7*length(Counts_items)))
  Nb_item_25 <- round((0.7*length(Counts_items))+1):round((0.95*length(Counts_items)))
  Nb_item_05 <- round((0.95*length(Counts_items))+1):round((1.00*length(Counts_items)))
  # Generating random quantities following the rules we fixed
  DT[,quantity :=
       c(sample(1:15,
               size = sum(Counts_items[Nb_item_70]),
               replace = TRUE,
       ),
       sample(1:7,
              size = sum(Counts_items[Nb_item_25]),
              replace = TRUE,
       ),
       sample(1:3,
              size = sum(Counts_items[Nb_item_05]),
              replace = TRUE))
    ]
  print("quantities added")
  
  ### Add Turnover
  DT[,turnover := quantity*prices]
  print("turnovers added")
  
  ### add day/month/year/quarter columns
  DT[,month:= lubridate::month(the_date_transaction,label = TRUE, locale = "English")]
  DT[,year:= as.character(
    lubridate::year(the_date_transaction)
  )]
  DT[,quarter:= paste0("Qr.",
                       quarter(the_date_transaction)
  )]
  DT[,day:= lubridate::wday(the_date_transaction, label = TRUE, locale = "English")]
  print("day/month/year/quarter added")
  
  ### shuffle by rows
  DT <- DT[sample(nrow(DT))]
  
  ### save Rds
  setwd(choose.dir(caption = "Select a folder to save your Data.Rds"))
  write_feather(DT,"Data-Decathlon.feather")
  print("Data saved")
  
  ### Delete order
  
  #return(DT)
  return("done")
}

# test function .. (bhal li dar khona)

Generate_random_data(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                     seed, store_names = store_names, item_names = item_names )

