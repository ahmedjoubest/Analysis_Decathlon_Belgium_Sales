
# Global Call
source("global.R")

#  SERVER  -------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1 Reactive filtered Data  ------------------------------------------
  
  DT <- eventReactive(input$submit, ignoreNULL = F,{
    
    # If the user want to pick two dates for the range date
    if(input$radio_date_format=='0'){
      filter(
        Data,
        item_name %in% input$item_names &
          store_name %in% input$store_names &
          prices >= input$prices_range[1] &
          prices <= input$prices_range[2] &
          the_date_transaction >= input$dates_range[1] &
          the_date_transaction  <=  input$dates_range[2]
      )
    } else { 
      filter(
        Data,
        item_name %in% input$item_names &
          store_name %in% input$store_names &
          prices >= input$prices_range[1] &
          prices <= input$prices_range[2] &
          the_date_transaction >= max(the_date_transaction) - as.numeric(input$radio_date_format)
      )
    }
    # Note : For this shiny app I use {dplyr} to filter/group/summarize etc. 
    # but generally for shiny I prefer {data.table} because every millisecond counts
  })
  
  
  # 2 Filter interdependence --------------------------------------------
  
  ### {item}: depends on <= {store} & {price range} ###
  ### {Date}: independent filter                    ###
  
  # Update {item} based on {store} with respecting the chosen {price range}
  observeEvent(
    input$store_names,ignoreInit = T,
    {
      item_names_update <- 
        unique(Data$item_name[Data$store_name %in% input$store_names &
                                # with respect the chosen {price range}:
                                Data$prices >= input$prices_range[1] &
                                Data$prices <= input$prices_range[2] ] 
        )
      updatePickerInput(
        session, "item_names", choices = item_names_update, 
        selected = item_names_update[1]
      )
    }
  )
  
  # Update {item} based on {price range} with respecting the chosen {store}
  observeEvent(
    input$prices_range,
    ignoreInit = T,
    {
      item_names_update <- 
        unique(Data$item_name[
          Data$prices >= input$prices_range[1] &
            Data$prices <= input$prices_range[2] &
            # with respect chosen {store}:
            Data$store_name %in% input$store_names
        ]
        )
      updatePickerInput(
        session, "item_names", choices = item_names_update,
        selected = item_names_update[1]
      )
    }
  )
  
  # 3. render UI of h1 title dashboard body ---------------------------------
  
  observeEvent(input$submit, ignoreNULL = F,{
    
    # parameters
    N_stores <-  unique(DT()$store_name) %>% length()
    N_items <- unique(DT()$item_name) %>% length()
    Date_1 <- min(DT()$the_date_transaction)
    Date_2 <- max(DT()$the_date_transaction)
    
    # render h1
    output$title <- renderUI({
      tagList(
        tags$h1("Decathlon Sales Performance Report for ",tags$b(N_items),
                " item(s) in ", tags$b(N_stores)," different store"),
        tags$h1("Between ",
                tags$b(lubridate::month(Date_1, label=T, locale = "English")),
                tags$b(year(Date_1)),
                " and ",
                tags$b(lubridate::month(Date_2, label=T, locale = "English")),
                tags$b(year(Date_2)),':')
      )
    })
  })

  # 4. Total sales indicators value boxes outputs -------------------------
  
  ### I Create from the function 'Build_valuebox' the desired outputs
  
  observeEvent(DT(), {
    valuebox <- Build_valuebox(DT())
    lapply(names(valuebox), function(name) {
      output[[name]] <- renderValueBox(valuebox[[name]])
    })
  })
  
  # 5. KPI Evolution Gauges outputs --------------------------------------
  
  ### Same Concept as (4.)
  
  observeEvent(DT(), {
    Gauge <- Build_KPI_Evolution_gauge(DT(), Data)
    lapply(names(Gauge), function(name) {
      output[[name]] <- flexdashboard::renderGauge(Gauge[[name]])
    })
  })
  
  # 6. Gauge text description --------------------------------------------
  
  observeEvent(input$submit, ignoreNULL = F,{
    
    # Set parameters
    Date_1 <- min(DT()$the_date_transaction)
    Date_2 <- max(DT()$the_date_transaction)
    
    # The duration of the reference period to compare with
    duration <- difftime(Date_2, Date_1)
    
    # Output
    output$KPI_text_explication <- renderUI({
      tagList(
        h3("These gauges display in percentage the evolution of some key 
         performance indicators compared to the last ", tags$b(duration), " Days."),
        h4(tags$u("Note 1: "), tags$i(" Red "), "for decreasing KPI and ", 
           tags$i("Green "), "for increasing KPI."),
        h4(tags$u("Note 2: "), " If you filter by 'All Time', the gauges may not
           be credible !")
      )
    })
  })
  
  # 7. Stores cartography leaflet map --------------------------------------
  
  leaflet_Map <- 
    eventReactive(
      c(input$submit,
        input$radius,
        input$transaction_type_leaflet),
      ignoreNULL = F,{
        Build_leaflet_map(DT= DT(),
                          radius = input$radius,
                          transaction_type = input$transaction_type_leaflet)
      }
    )
  
  output$leaflet_Map <- renderLeaflet(leaflet_Map())  
  
  # 8. Temporal evolution Heat Map  ----------------------------------------
  
  output$heat_map_chronologic <-
    renderHighchart({
      Build_HC_Temporal_heatmap(DT= DT(),
                                X = input$display,
                                # default input = 'day'
                                intensity = input$intensity_2,
                                # default input='the_transaction_id' for transaction occurrence
                                transaction_type = input$transaction_type_2
                                # default input='sale' 
      )
    })
  
  # 9. Temporal evolution lines  -------------------------------------------
  
  output$Build_Date_line_charts <- 
    renderHighchart({
      Build_Date_line_charts(DT= DT(),
                             X = "the_date_transaction",
                             Y = input$Y_line_HC, 
                             # default input='the_transaction_id' for transaction occurrence
                             group = "item_name",
                             transaction_type = input$transaction_type_line_HC,
                             # default input='sale'
                             aggregation_period = input$aggregation_line_HC
                             # Default to "week"
      )
    })
  
  # 10. Items/stores correlation heat map  ----------------------------------
  
  output$heat_map_correlation <- 
    renderHighchart({
      Build_HC_Heatmap_Correlation(DT = DT(), X = 'store_name',Y = 'item_name',
                                   intensity = input$intensity_1,
                                   # default input='quantity' for total quantites
                                   transaction_type = input$transaction_type_1
                                   # default input='return'
      )
    })
  
  # 11. Stores comparison Bar Plot  ----------------------------------------
  
  output$Bar_plot_comparaison <- 
    renderHighchart({
      Build_HC_Barplot(DT = DT(),
                       X = "store_name",
                       Y = input$barplot_y_axis,
                       # default input='turnover' for total turnover
                       group = "tdt_type_detail",
                       percent = input$percent,
                       # default input = FALSE
                       horizontal = input$horizontal
                       # default input = FALSE 
      )
    })
  
  # 12. Display + Download Data --------------------------------------------
  
  ### Download button
  output$Download <-
    downloadHandler(
      paste0("Filtred-Data-", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        fwrite(DT(), file, sep = ";", row.names = F)
      }
    )
  
  ### Data Table
  output$DT <-
    DT::renderDataTable(
      datatable(DT(),
                filter = 'top',
                rownames = F,
                options = list(scrollY = '360px')
      )
    )
}

