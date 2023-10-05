# 17 march 2023
# app.R
# kurtis bertauche

# libraries

library(shiny)
library(shinyjqui)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(ggplot2)
library(ggrepel)

# pre-processing

file_list <- list.files("./data/fleetReports", pattern = "*.csv", full.names = FALSE)
file_list <- append(file_list, "Select a file", after = 0)



# ui

ui <- fluidPage(theme = shinytheme("paper"),
 
  headerPanel("AC/MFC Fleet Tools"),
  
  sidebarPanel(
    
    h4("Choose Two Reports to Compare"),
    selectInput(
      inputId = "file_select_input_1",
      label = "Choose file 1 (Older Date):",
      choices = file_list
    ),
    selectInput(
      inputId = "file_select_input_2",
      label = "Choose file 2 (Newer Date):",
      choices = file_list
    ),
    h4("Import new Files"),
    p("I didn't do this part yet.")
  ),
  
  mainPanel(
    
    tabsetPanel(
      id = "main_hide",
      type = "hidden",
      
      tabPanel("noData", 
               p("You have to select the files first."),
               tableOutput("table")),
      tabPanel("sameFileErr",
               p("Can't use same file dum dum"),),
      tabPanel("data",
               
               tabsetPanel(
                 id = "main",
                 type = "tabs",
                 
                 tabPanel("Circulation Changes", 
                          br(),
                          tabsetPanel(
                            id = "circulationPanel",
                            tabPanel("Top 20",
                                     br(),
                                     h4("Top 20 Most Popular Aircraft"),
                                     tableOutput("top20popularTable"),
                                     textOutput("oldTotalNumber"),
                                     textOutput("newTotalNumber"),
                                     textOutput("changeTotalNumber")),
                            tabPanel("Biggest Popularity Changes",
                                     br(),
                                     h4("Top 20 Changes in Popularity"),
                                     tableOutput("top20poptable"),),
                            tabPanel("Fastest Growing",
                                     br(),
                                     h4("Top 20 Fastest Growing Aicraft"),
                                     tableOutput("top20growingCircTable")),
                            tabPanel("Fastest Shrinking",
                                     br(),
                                     h4("Top 20 Fastest Shrinking Aircraft"),
                                     tableOutput("top20shrinkingCircTable")),
                            tabPanel("Raw Data",
                                     br(),
                                     h4("Raw Data"),
                                     DTOutput("allCirculationTable"))
                          )

                          ),
                 
                 tabPanel("Fleet Changes", 
                          br(),
                          tabsetPanel(
                            id = "fleetChangesPanel",
                            tabPanel("Top 20",
                                     h4("Top 20 Largest Fleets"),
                                     tableOutput("top20fleetTable")
                                     ),
                            tabPanel("Fastest Growing",
                                     h4("Fastest Growing Fleets"),
                                     tableOutput("fastestGrowingTable")
                                     ),
                            tabPanel("Fastest Shrinking",
                                     h4("Fastest Shrinking Fleets"),
                                     tableOutput("fastestShrinkingTable")
                                     ),
                            tabPanel("Raw Data",
                                     h4("Raw Data"),
                                     DTOutput("allFleetTable"),
                                     h6("Looking for more details for individual airlines?"),
                                     h6("Try the \"Airline Search\" Tab."))
                            )
                          ),
                 
                 tabPanel("Discord Message",
                          br(),
                          h4("Discord message for Fleet Report:"),
                          verbatimTextOutput("discordMessage"),
                          verbatimTextOutput("discordMessage2")),
                 
                 tabPanel("Visualize",
                          br(),
                          h5("This tab is very underwhelming..."),
                          plotOutput("airlinePlot"),
                          plotOutput("planePlot")),
                 
                 tabPanel("Airline Search",
                          br(),
                          h4("Search for an Airline for more details:"),
                          textInput(inputId = "airlineSearch", "Enter airline name..."),
                          tabsetPanel(
                            id = "airlineSearchPanel",
                            type = "hidden",
                            tabPanel("noSearch",
                                     h4("You haven't searched for anything yet.")),
                            tabPanel("invalidSearch",
                                     br(),
                                     textOutput("suggestionTitle"),
                                     h4("Suggestions:"),
                                     withSpinner(textOutput("suggestions"), type = 6)),
                            tabPanel("validSearch",
                                     textOutput("validHeader"),
                                     br(),
                                     tableOutput("airlineDetail"),
                                     h4("Current Fleet:"),
                                     plotOutput("fleetPlot"))
                          )),
                 tabPanel("Aircraft Search",
                          br(),
                          h4("Search for an Aircraft for more details:"),
                          textInput(inputId = "aircraftSearch", "Enter aircraft model..."),
                          tabsetPanel(
                            id = "aircraftSearchPanel",
                            type = "hidden",
                            tabPanel("noSearch",
                                     h4("You haven't searched for anything yet.")),
                            tabPanel("invalidSearch",
                                     br(),
                                     textOutput("suggestionTitleAC"),
                                     h4("Suggestions:"),
                                     withSpinner(textOutput("suggestionsAC"), type = 6)),
                            tabPanel("validSearch",
                                     textOutput("validHeaderAC"),
                                     br(),
                                     h4("Ownwership Distribution"),
                                     plotOutput("aircraftPlot"),
                                     tableOutput("aircraftDetail"),
                                     p("Note: Don't use these numbers to calculate the in-game ownership percentage. In-game ownership percentage uses aircraft on the used market which are not included here.")
                                     )
                          )
                          )
               ))
    )
  )
)


# server

server <- function(input, output, session){
  
  ########## -- sidebar -- ##########
  
  observeEvent(ignoreInit = TRUE,
               c(input$file_select_input_1, input$file_select_input_2),
               {
                 if(input$file_select_input_1 == input$file_select_input_2)
                 {
                   updateTabsetPanel(inputId = "main_hide", selected = "sameFileErr")
                 }
                 else if(input$file_select_input_1 == "Select a file" | input$file_select_input_2 == "Select a file")
                 {
                   updateTabsetPanel(inputId = "main_hide", selected = "noData")
                 }
                 else
                 {
                   updateTabsetPanel(inputId = "main_hide", selected = "data")
                 }
               })
  
  oldData <- reactive({
    if(input$file_select_input_1 != "Select a file")
    {
      read.csv(paste("./data/fleetReports/", input$file_select_input_1, sep = ""))
    }
  })
  
  newData <- reactive({
    if(input$file_select_input_2 != "Select a file")
    {
      read.csv(paste("./data/fleetReports/", input$file_select_input_2, sep = ""))
    }
  })
  
  ########## -- circulation changes -- ##########
  
  #### -- element renders -- ####
 
  output$allCirculationTable <- renderDT(datatable(rawCirculationChangesDF()))
  output$top20poptable <- renderTable(head(circulationChangesDF(), 20)) 
  output$top20growingCircTable <- renderTable(head(growingCirDF(), 20))
  output$top20shrinkingCircTable <- renderTable(head(shrinkingCirDF(), 20))
  output$top20popularTable <- renderTable(head(top20DF(), 20))
  
  output$newTotalNumber <- renderText({
    paste("New Total Number of Aircraft:", newTotalNumberInCirculation())
  })
  
  output$oldTotalNumber <- renderText({
    paste("Old Total Number of Aircraft", oldTotalNumberInCirculation())
  })
  
  output$changeTotalNumber <- renderText({
    paste("Change in Total Number of Aircraft:", newTotalNumberInCirculation() - oldTotalNumberInCirculation())
  })

  
  #### -- event handlers -- ####
  
  #### -- data manipulation -- ####
  
  showingRawCirculationFlag <- reactiveVal(FALSE)
  
  oldCondensed <- reactive({
    req(oldData())
    old <- oldData()
    old$Airline <- NULL
    old$Total <- NULL
    old <- old[, order(names(old))]
    oldCondensed <- data.frame(Airplane = character(),
                               Total = integer())
    for(i in 1:ncol(old))
    {
      oldCondensed <- rbind(oldCondensed, c(colnames(old)[i], sum(old[,i])))
    }
    colnames(oldCondensed) <- c("Aircraft", "Total")
    oldCondensed
  })
  
  newCondensed <- reactive({
    req(newData())
    new <- newData()
    new$Airline <- NULL
    new$Total <- NULL
    new <- new[, order(names(new))]
    newCondensed <- data.frame(Airplane = character(),
                               Total = integer())
    for(i in 1:ncol(new))
    {
      newCondensed <- rbind(newCondensed, c(colnames(new)[i], sum(new[,i])))
    }
    colnames(newCondensed) <- c("Aircraft", "Total")
    newCondensed
  })
  
  circulationChangesDF <- reactive({
    req(top20DF())
    
    circ <- top20DF()
    circ <- circ[order(strtoi(abs(circ$Change)), decreasing = TRUE),]
    circ$Rank <- rank(-strtoi(abs(circ$Change)), ties.method = "max")
    circ
  })
  
  top20DF <- reactive({
    req(newCondensed())
    req(oldCondensed())
    
    new <- newCondensed()
    old <- oldCondensed()
    colnames(new) <- c("Aircraft", "New Total")
    colnames(old) <- c("Aircraft", "Old Total")
    
    together <- merge(new, old, by = "Aircraft")
    together$Change <- strtoi(together$`New Total`) - strtoi(together$`Old Total`)
    together <- together[order(strtoi(together$`New Total`), decreasing = TRUE),]
    together$Rank <- rank(-strtoi(together$`New Total`), ties.method = "max")
    together <- together[, c("Rank", "Aircraft", "Old Total", "New Total", "Change")]
    together
  })
  
  growingCirDF <- reactive({
    req(top20DF())
    
    growing <- top20DF()
    growing <- growing[order(strtoi(growing$Change), decreasing = TRUE),]
    growing$Rank <- rank(-strtoi(growing$Change), ties.method = "max")
    growing
  })
  
  shrinkingCirDF <- reactive({
    req(top20DF())
    
    shrink <- top20DF()
    shrink <- shrink[order(strtoi(shrink$Change), decreasing = FALSE),]
    shrink$Rank <- rank(strtoi(shrink$Change), ties.method = "max")
    shrink
  })
  
  rawCirculationChangesDF <- reactive({
    req(newCondensed())
    req(oldCondensed())
    
    raw <- newCondensed()
    raw$`Old Total` <- oldCondensed()$Total
    raw$Change <- strtoi(raw$Total) - strtoi(raw$`Old Total`)
    colnames(raw) <- c("Aircraft", "New Total", "Old Total", "Change")
    raw
  })
  
  newTotalNumberInCirculation <- reactive({
    req(newCondensed())
    sum(strtoi(newCondensed()$Total))
  })
  
  oldTotalNumberInCirculation <- reactive({
    req(oldCondensed())
    sum(strtoi(oldCondensed()$Total))
  })
  
  ########## -- fleet changes -- ##########
  
  #### -- element renders -- ####
  
  output$top20fleetTable <- renderTable(largestFleetsDF())
  output$fastestGrowingTable <- renderTable(largestFleetsGrowingDF())
  output$fastestShrinkingTable <- renderTable(largestFleetsShrinkingDF())
  output$allFleetTable <- renderDT(datatable(largestFleetsAllDataDF()))
  
  #### -- event handlers -- ####
  
  #### -- data manipulation ####
  
  largestFleetsAllDataDF <- reactive({
    req(oldData())
    req(newData())
    
    oldLargest <- data.frame(matrix(ncol = 1, nrow = nrow(oldData())))
    oldLargest$Airline <- oldData()$Airline
    oldLargest$Count <- oldData()$Total
    oldLargest[1] <- NULL
    colnames(oldLargest) <- c("Airline", "Old Count")
    
    newLargest <- data.frame(matrix(ncol = 1, nrow = nrow(newData())))
    newLargest$Airline <- newData()$Airline
    newLargest$Count <- newData()$Total
    newLargest[1] <- NULL
    colnames(newLargest) <- c("Airline", "New Count")
    
    combined <- merge(newLargest, oldLargest, by = "Airline", all = TRUE)
    combined[is.na(combined)] <- 0
    combined$Change <- as.integer(combined$`New Count` - combined$`Old Count`)
    combined$`New Count` <- as.integer(combined$`New Count`)
    combined$`Old Count` <- as.integer(combined$`Old Count`)
    combined
  })
  
  largestFleetsDF <- reactive({
    req(largestFleetsAllDataDF())
    
    largestFleetsDF <- largestFleetsAllDataDF()
    largestFleetsDF <- largestFleetsDF[order(largestFleetsDF$`New Count`, decreasing = TRUE),]
    largestFleetsDF$Rank <- rank(-largestFleetsDF$`New Count`, ties.method = "max")
    largestFleetsDF <- largestFleetsDF[, c("Rank", "Airline", "New Count", "Old Count", "Change")]
    head(largestFleetsDF, 20)
    
  })
  
  largestFleetsGrowingDF <- reactive({
    req(largestFleetsAllDataDF())
    
    largestFleetsDF <- largestFleetsAllDataDF()
    largestFleetsDF <- largestFleetsDF[order(largestFleetsDF$Change, decreasing = TRUE),]
    largestFleetsDF$Rank <- rank(-largestFleetsDF$Change, ties.method = "max")
    largestFleetsDF <- largestFleetsDF[, c("Rank", "Airline", "New Count", "Old Count", "Change")]
    head(largestFleetsDF, 20)
  })
  
  largestFleetsShrinkingDF <- reactive({
    req(largestFleetsAllDataDF())
    
    largestFleetsDF <- largestFleetsAllDataDF()
    largestFleetsDF <- largestFleetsDF[order(largestFleetsDF$Change, decreasing = FALSE),]
    largestFleetsDF$Rank <- rank(largestFleetsDF$Change, ties.method = "max")
    largestFleetsDF <- largestFleetsDF[, c("Rank", "Airline", "New Count", "Old Count", "Change")]
    head(largestFleetsDF, 20)
  })
  
  ########## -- discord message -- ##########
  
  #### -- element renders -- ####
  
  output$discordMessage <- renderText({
    string = ""
    string <- paste("**PUT DATE HERE Fleet Report** (1/2)\n",
                    "NOTE GOES HERE",
                    "\n\n**REMINDER: **",
                    "I only use airlines that have been active in the past 30 days for this report. Why? It seems more relevant to me to know what airlines are up to today rather than 3 months ago.",
                    "\n\n**Top 10 Aircraft by Number Owned**\n",
                    sep = "")
    
    tenDF <- AircraftPopularityFrame()[AircraftPopularityFrame()$`New Rank` <= 10,] # these are already in the right order
    tenDF$Aircraft <- gsub(pattern = "\\.", replacement =  " ", x = tenDF$Aircraft)
    
    for(i in 1:nrow(tenDF))
    {
      if(tenDF[i,5] > tenDF[i,6])
      {
        # new rank higher than old rank
        string <- paste(string,
                        tenDF[i,5], # new rank
                        ". :small_red_triangle_down: ",
                        tenDF[i,1], # aircraft name
                        " - ",
                        tenDF[i,2], # new total
                        " | ",
                        sep = "")
      }
      else if(tenDF[i,5] < tenDF[i,6])
      {
        # new rank lower than old rank
        string <- paste(string,
                        tenDF[i,5], # new rank
                        ". :arrow_up_small: ",
                        tenDF[i,1], # aircraft name
                        " - ",
                        tenDF[i,2], # new total
                        " | ",
                        sep = "")
      }
      else
      {
        # new rank is the same as old rank
        string <- paste(string,
                        tenDF[i,5], # new rank
                        ". ",
                        tenDF[i,1], # aircraft name
                        " - ",
                        tenDF[i,2], # new total
                        " | ",
                        sep = "")
      }
      
      # THEN THE CHANGE PART
      if(tenDF[i,2] > tenDF[i,3])
      {
        # change is positive
        string <- paste(string,
                        ":arrow_up_small: ",
                        tenDF[i,4],
                        "\n",
                        sep = "")
      }
      else if(tenDF[i, 2] < tenDF[i, 3])
      {
        # change is negative
        string <- paste(string,
                        ":small_red_triangle_down: ",
                        abs(tenDF[i,4]),
                        "\n",
                        sep = "")
      }
      else
      {
        string <- paste(string,
                        "no change ",
                        "\n",
                        sep = "")
      }
      
    }
    
    string <- paste(string,
                    "\nTotal Aircraft Owned: ",
                    newTotalNumberInCirculation(),
                    " | ")
    if(newTotalNumberInCirculation() > oldTotalNumberInCirculation())
    {
      string <- paste(string,
                      ":arrow_up_small: ",
                      newTotalNumberInCirculation() - oldTotalNumberInCirculation(),
                      "\n\n")
    }
    else if(oldTotalNumberInCirculation() > newTotalNumberInCirculation())
    {
      string <- paste(string,
                      ":small_red_triangle_down: ",
                      abs(newTotalNumberInCirculation() - oldTotalNumberInCirculation()),
                      "\n\n")
    }
    else
    {
      string <- paste(string,
                      "no change\n\n")
    }
    
    tenDF <- AircraftPopularityFrame()
    tenDF$`Rank of Change` <- rank(-abs(tenDF$Change), ties.method = "max")
    tenDF <- tenDF[tenDF$`Rank of Change` <= 10,]
    tenDF <- tenDF[order(tenDF$`Rank of Change`, decreasing = FALSE),]
    tenDF$Aircraft <- gsub(pattern = "\\.", replacement =  " ", x = tenDF$Aircraft)
    
    string <- paste(string,
                    "**Top Changes in Popularity for Aircraft**\n",
                    sep = "")
    
    for(i in 1:nrow(tenDF))
    {
      if(tenDF[i,4] > 0)
      {
        # positive change
        string <- paste(string,
                        tenDF[i,7],
                        ". ",
                        tenDF[i,1],
                        " :arrow_up_small: ",
                        tenDF[i,4],
                        "\n",
                        sep = "")
      }
      else
      {
        # negative change
        string <- paste(string,
                        tenDF[i,7],
                        ". ",
                        tenDF[i,1],
                        " :small_red_triangle_down: ",
                        abs(tenDF[i,4]),
                        "\n",
                        sep = "")
      }
    }
    string <- paste(string,
                    "\nNOTE GOES HERE",
                    sep = "")
    string
  })
  
  output$discordMessage2 <- renderText({
    string = ""
    string <- paste("**PUT DATE HERE Fleet Report** (2/2)\n\n",
                    "**Top 10 Airlines by Fleet Size**\n\n",
                    "**WARNING: Do not make any attempts to manipulate this report by purchasing excess aircraft for the sole purpose of getting onto the report. Doing so is against game rules and will incur punishment.**\n\n",
                    sep = "")
    tenDF <- AirlineFleetFrame()
    tenDF <- tenDF[tenDF$`Rank` <= 10,]
    for(i in 1:nrow(tenDF))
    {
      if(tenDF[i,5] > tenDF[i,6])
      {
        # up a rank
        string <- paste(string,
                        tenDF[i,5],
                        ". ",
                        ":small_red_triangle_down: ",
                        tenDF[i,1],
                        " :flag_: ",
                        tenDF[i,2],
                        " | ",
                        sep = "")
      }
      else if(tenDF[i,5] < tenDF[i,6])
      {
        # down a rank
        string <- paste(string,
                        tenDF[i,5],
                        ". ",
                        ":arrow_up_small: ",
                        tenDF[i,1],
                        " :flag_: ",
                        tenDF[i,2],
                        " | ",
                        sep = "")
      }
      else
      {
        # no rank change
        string <- paste(string,
                        tenDF[i,5],
                        ". ",
                        tenDF[i,1],
                        " :flag_: ",
                        tenDF[i,2],
                        " | ",
                        sep = "")
      }
      if(tenDF[i,4] > 0)
      {
        # pos change
        string <- paste(string,
                        ":arrow_up_small: ",
                        tenDF[i,4],
                        "\n",
                        sep = "")
      }
      else if(tenDF[i,4] < 0)
      {
        # neg change
        string <- paste(string,
                        ":small_red_triangle_down: ",
                        abs(tenDF[i,4]),
                        "\n",
                        sep = "")
      }
      else
      {
        # no change
        string <- paste(string,
                        "no change ",
                        "\n",
                        sep = "")
      }
    }
    string <- paste(string,
                    "\n**Fastest Dropping Fleets**\n")
    tenDF <- AirlineFleetFrame()
    tenDF <- tenDF[order(tenDF$Change, decreasing = FALSE),]
    tenDF$`Drop Rank` <- rank(tenDF$Change, ties.method = "max")
    tenDF <- tenDF[tenDF$`Drop Rank` <= 10,]
    for(i in 1:nrow(tenDF))
    {
      string <- paste(string,
                      tenDF[i,7],
                      ". ",
                      tenDF[i,1],
                      " - ",
                      tenDF[i,2],
                      " | :small_red_triangle_down: ",
                      abs(tenDF[i,4]),
                      "\n",
                      sep = "")
    }
    
    string <- paste(string,
                    "\n**Fastest Growing Fleets**\n")
    tenDF <- AirlineFleetFrame()
    tenDF <- tenDF[order(tenDF$Change, decreasing = TRUE),]
    tenDF$`Grow Rank` <- rank(-tenDF$Change, ties.method = "max")
    tenDF <- tenDF[tenDF$`Grow Rank` <= 10,]    
    for(i in 1:nrow(tenDF))
    {
      string <- paste(string,
                      tenDF[i,7],
                      ". ",
                      tenDF[i,1],
                      " - ",
                      tenDF[i,2],
                      " | :arrow_up_small: ",
                      abs(tenDF[i,4]),
                      "\n",
                      sep = "")
    }
    
    string <- paste(string,
                    "\nPrevious Report:\nLINK GOES HERE\n(And of course, let me know if I made a mistake, and I'll fix it.)")
    string
  })
  
  #### -- event handlers -- ####
  
  #### -- data manipulation ####
  
  AircraftPopularityFrame <- reactive({
    req(oldCondensed())
    req(newCondensed())
    
    new <- newCondensed()
    old <- oldCondensed()
    colnames(new) <- c("Aircraft", "New Total")
    colnames(old) <- c("Aircraft", "Old Total")
    
    together <- merge(new, old, by = "Aircraft")
    together$Change <- strtoi(together$`New Total`) - strtoi(together$`Old Total`)
    together$`New Rank` <- rank(-strtoi(together$`New Total`), ties.method = "max")
    together$`Old Rank` <- rank(-strtoi(together$`Old Total`), ties.method = "max")
    together <- together[order(strtoi(together$`New Total`), decreasing = TRUE),]
    
  })
  
  AirlineFleetFrame <- reactive({
    req(largestFleetsAllDataDF())
    
    fleetFrame <- largestFleetsAllDataDF()
    fleetFrame$Rank <- rank(-strtoi(fleetFrame$`New Count`), ties.method = "max")
    fleetFrame$`Old Rank` <- rank(-strtoi(fleetFrame$`Old Count`), ties.method = "max")
    fleetFrame <- fleetFrame[order(fleetFrame$Rank, decreasing = FALSE),]
    fleetFrame
  })
  
  ########## -- visualize  -- ##########
  
  #### -- element renders -- ####
  
  output$airlinePlot <- renderPlot({
    barplot(height = head(strtoi(largestFleetsDF()$`New Count`), 10), 
            names.arg = head(largestFleetsDF()$`Airline`, 10),
            main = "Largest Airlines by Fleet Size",
            ylab = "Fleet Size",
            xlab = "Airline",
            col = "blue",
            cex.names = 0.5)
  })
  
  output$planePlot <- renderPlot({
    barplot(height = strtoi(head(top20DF()$`New Total`, 10)),
            names.arg = head(top20DF()$Aircraft, 10),
            main = "Most Popular Aircraft",
            ylab = "Number in Circulation",
            xlab = "Aircraft",
            col = "green",
            cex.names = 0.5
    )
  })
  
  #### -- event handlers -- ####
  
  #### -- data manipulation ####
  
  ########## -- airline search -- ##########
  
  #### -- element renders -- ####
  
  output$suggestionTitle <- renderText({
    paste(input$airlineSearch, "was not found.")
  })
  
  output$suggestions <- renderText({
    suggestedAirlines()
  })
  
  output$validHeader <- renderText({
    paste("Details for", input$airlineSearch, ":")
  })
  
  output$airlineDetail <- renderTable({
    airlineDetailDF()
  })
  
  output$fleetPlot <- renderPlot({
    dfHere <- airlineDetailDF()
    #print(which(dfHere$Aircraft == "Total"))
    dfHere <- dfHere[-which(dfHere$Aircraft == "Total"),]
    dfHere <- dfHere[dfHere$`New Count` > 0,]
    pie(strtoi(dfHere$`New Count`), labels = dfHere$Aircraft, col = c("blue", "aquamarine4", "brown4", "darkslateblue", "khaki4","mediumslateblue","springgreen4", "turquoise4","tomato","thistle3","seagreen3"))
  })
  
  #### -- event handlers -- ####
  
  observeEvent(input$airlineSearch, {
    if(input$airlineSearch %in% mergedChanges()$Airline)
    {
      updateTabsetPanel(session, inputId = "airlineSearchPanel", "validSearch")
    }
    else if(input$airlineSearch == "")
    {
      updateTabsetPanel(session, inputId = "airlineSearchPanel", "noSearch")
    }
    else
    {
      updateTabsetPanel(session, inputId = "airlineSearchPanel", "invalidSearch")
    }
  })
    
  #### -- data manipulation ####
  
  suggestedAirlines <- reactive({
    airlineVector <- mergedChanges()$Airline
    lowerAirlineVecotr <- tolower(airlineVector)
    idx <- agrep(tolower(input$airlineSearch), lowerAirlineVecotr, 0.05)
    string <- ""
    if(length(idx) == 1)
    {
      updateTextInput(session, "airlineSearch", value = airlineVector[idx])
    }
    else
    {
      for(x in airlineVector[idx])
        string <- paste(string, x, ", ", sep = "")
      string <- substr(string, 1, nchar(string) - 2)
    }
    string
  })
    
  mergedChanges <- reactive({
    req(oldData())
    req(newData())
    
    oldFull <- oldData()
    oldFull <- oldFull[, order(names(oldFull))]
    newFull <- newData()
    newFull <- newFull[, order(names(newFull))]
    combinedFull <- merge(oldFull, newFull, by = "Airline", all.x = TRUE, all.y = TRUE)
    combinedFull[is.na(combinedFull)] <- 0
    
    mergedOld <- combinedFull[2:ceiling(ncol(combinedFull)/2)]
    mergedNew <- combinedFull[(ceiling(ncol(combinedFull)/2) + 1):ncol(combinedFull)]
    mergedChanges <- mergedNew - mergedOld
    mergedChanges$Airline <- combinedFull$Airline
    mergedChanges$absTotal <- abs(mergedChanges$Total.y)
    mergedChanges <- mergedChanges[order(mergedChanges$absTotal, decreasing = TRUE),]
    
    mergedChanges
  })
  
  airlineDetailDF <- reactive({
    req(oldData())
    req(newData())
    
    oldFleet <- data.frame(Aircraft = character(),
                           `Old Count` = character())
    newFleet <- data.frame(Aircraft = character(),
                           `New Count` = character())
    
    for(i in 1:nrow(oldData()))
    {
      if(oldData()$Airline[i] == input$airlineSearch)
      {
        for(j in 1:ncol(oldData()))
        {
          if((oldData()[i,j] != 0) & (j != 1))
          {
            prettyName <- colnames(oldData())[j]
            prettyName <- gsub(pattern = "\\.", replacement =  " ", x = prettyName)
            
            newRow <- c(Aircraft = prettyName,
                        `Old Count` = oldData()[i,j])
            oldFleet <- rbind(oldFleet, newRow)
          }
        }
      }
    }

    for(i in 1:nrow(newData()))
    {
      if(newData()$Airline[i] == input$airlineSearch)
      {
        for(j in 1:ncol(newData()))
        {
          if((newData()[i,j] != 0) & (j != 1))
          {
            prettyName <- colnames(newData())[j]
            prettyName <- gsub(pattern = "\\.", replacement =  " ", x = prettyName)
            
            newRow <- c(Aircraft = prettyName,
                        `New Count` = newData()[i,j])
            newFleet <- rbind(newFleet, newRow)
          }
        }
      }
    }
    colnames(newFleet) <- c("Aircraft", "New Count")
    colnames(oldFleet) <- c("Aircraft", "Old Count")
    
    total <- merge(oldFleet, newFleet, by = "Aircraft", all = TRUE)
    total[is.na(total)] <- 0
    total$Change <- strtoi(total$`New Count`) - strtoi(total$`Old Count`)
    total
  })
  

  
  ########## -- airline search -- ##########
  
  #### -- element renders -- ####
  output$suggestionTitleAC <- renderText({
    paste(input$aircraftSearch, "was not found.")
  })
  
  output$suggestionsAC <- renderText({
    suggestedAircraft()
  })
  
  output$validHeaderAC <- renderText({
    paste("Operators of", input$aircraftSearch, ":")
  })
  
  output$aircraftDetail <- renderTable({
    specificAircraftDF()
  })
  
  output$aircraftPlot <- renderPlot({
    dfHere <- specificAircraftDF()
    #ggplot(dfHere, aes(x="", y=dfHere[,2], fill=Airline, label = Airline))+
    #  theme(legend.position = "none") +
    #  geom_bar(stat = "identity", width = 1) +
    #  coord_polar("y", start = 0) +
    #  geom_text_repel(max.overlaps = 999, min.segment.length = Inf, ylim = 5)
    if(nrow(dfHere) > 20)
    {
      dfHereTop20Only <- dfHere[1:15,]
      print(dfHereTop20Only)
      dfHereElse <- dfHere[16:nrow(dfHere),]
      totalSum <- sum(dfHereElse[,2])
      colnames(dfHereTop20Only) <- c("Airline", "Count")
      newRow <- data.frame(Airline = "Other", Count = totalSum)
      dfHereTop20Only <- rbind(dfHereTop20Only, newRow)
      return(pie(strtoi(dfHereTop20Only[,2]), labels = dfHereTop20Only$Airline, col = c("blue", "aquamarine4", "brown4", "darkslateblue", "khaki4","mediumslateblue","springgreen4", "turquoise4","tomato","thistle3","seagreen3")))
    }
    
      
    pie(strtoi(dfHere[,2]), labels = dfHere$Airline, col = c("blue", "aquamarine4", "brown4", "darkslateblue", "khaki4","mediumslateblue","springgreen4", "turquoise4","tomato","thistle3","seagreen3"))
    
  })

  #### -- event handlers -- ####
  
  observeEvent(input$aircraftSearch, {
    cols <- colnames(oldData())
    cols <- cols[3:length(cols)]
    cols <- gsub(pattern = "\\.", replacement =  " ", x = cols)
    if(tolower(input$aircraftSearch) %in% tolower(cols))
    {
      idx = match(c(tolower(input$aircraftSearch)), tolower(cols))
      updateTextInput(session, "aircraftSearch", value = cols[idx])
      updateTabsetPanel(session, inputId = "aircraftSearchPanel", "validSearch")
    }
    else if(input$aircraftSearch == "")
    {
      updateTabsetPanel(session, inputId = "aircraftSearchPanel", "noSearch")
    }
    else
    {
      updateTabsetPanel(session, inputId = "aircraftSearchPanel", "invalidSearch")
    }
  })
  
  #### -- data manipulation ####
  aircraftDF <- reactive({
    req(newData())
    
    thisDF <- newData()
    colnames(thisDF) <- gsub(pattern = "\\.", replacement =  " ", x = colnames(thisDF))
    thisDF
  })
  
  specificAircraftDF <- reactive({
    req(aircraftDF)
    
    full <- aircraftDF()
    indexOfPlane <- grep(input$aircraftSearch, colnames(full))
    #print("HIII")
    #print(indexOfPlane)
    smallerDF <- full[, c(1, indexOfPlane)]
    smallerDF <- smallerDF[apply(smallerDF!=0,1,all),]
    smallerDF <- smallerDF[order(smallerDF[,2], decreasing = TRUE),]
    smallerDF
  })
  
  suggestedAircraft <- reactive({
    aircraftVector <- colnames(newData())[3:ncol(newData())]
    aircraftVector <- gsub(pattern = "\\.", replacement =  " ", x = aircraftVector)
    lowerAircraftVector <- tolower(aircraftVector)
    idx <- agrep(tolower(input$aircraftSearch), lowerAircraftVector, 0.05)
    string <- ""
    if(length(idx) == 1)
    {
      updateTextInput(session, "aircraftSearch", value = aircraftVector[idx])
    }
    else if(tolower(input$aircraftSearch) %in% lowerAircraftVector)
    {
      thisIdx <- which(lowerAircraftVector == tolower(input$aircraftSearch))
      updateTextInput(session, "aircraftSearch", value = aircraftVector[thisIdx])
    }
    else
    {
      for(x in aircraftVector[idx])
        string <- paste(string, x, ", ", sep = "")
      string <- substr(string, 1, nchar(string) - 2)
    }
    string
  })

}
# shiny
shinyApp(ui, server)