library(shiny)
library(dplyr)
library(DiagrammeR)

server <- function(input, output, session) {
  
  observeEvent(input$createPools, {
    numPools <- input$numPools
    
    output$poolInputs <- renderUI({
      poolInputs <- list()
      poolNames <- LETTERS[1:numPools]
      
      for (i in 1:numPools) {
        poolInputs[[i]] <- tagList(
          h4(paste("Pool", poolNames[i])),
          textAreaInput(paste0("teamsPool", i), "Enter team names (one per line):", rows = 5)
        )
      }
      do.call(tagList, poolInputs)
    })
  })
  
  poolData <- reactive({
    input$generate
    isolate({
      numPools <- input$numPools
      poolNames <- LETTERS[1:numPools]
      data <- list()
      
      for (i in 1:numPools) {
        teamNames <- unlist(strsplit(input[[paste0("teamsPool", i)]], "\n"))
        if (length(teamNames) > 0) {
          data[[i]] <- data.frame(Team = teamNames, stringsAsFactors = FALSE)
        } else {
          data[[i]] <- data.frame(Team = character(0), stringsAsFactors = FALSE)
        }
      }
      
      maxTeams <- max(sapply(data, nrow))
      for (i in 1:length(data)) {
        if (nrow(data[[i]]) < maxTeams) {
          data[[i]] <- rbind(data[[i]], data.frame(Team = rep("", maxTeams - nrow(data[[i]]))))
        }
      }
      
      combinedData <- do.call(cbind, lapply(1:length(data), function(i) {
        setNames(data[[i]], paste("Pool", poolNames[i]))
      }))
      
      list(data = data, combinedData = combinedData)
    })
  })
  
  output$poolTable <- renderTable({
    req(poolData())
    poolData()$combinedData
  })
  
  observeEvent(input$generate, {
    req(poolData())
    numGroups <- input$numGroups
    poolNames <- LETTERS[1:input$numPools]
    groups <- vector("list", numGroups)
    
    for (i in 1:numGroups) {
      groups[[i]] <- data.frame(Team = character(), Pool = character(), stringsAsFactors = FALSE)
    }
    
    pools <- poolData()$data
    set.seed(123) # For reproducibility
    
    currentGroupIndex <- 1
    
    # Distribute teams from each pool to groups
    for (poolIndex in 1:length(pools)) {
      teams <- pools[[poolIndex]]$Team
      teams <- teams[teams != ""]
      teams <- sample(teams) # Randomize teams in the pool
      
      while (length(teams) > 0) {
        groups[[currentGroupIndex]] <- rbind(groups[[currentGroupIndex]], data.frame(Team = teams[1], Pool = poolNames[poolIndex]))
        teams <- teams[-1]
        currentGroupIndex <- currentGroupIndex %% numGroups + 1
      }
    }
    
    output$groupTables <- renderUI({
      groupTables <- list()
      
      for (i in 1:numGroups) {
        groupTables[[i]] <- column(
          3,
          h4(paste("Group", LETTERS[i])),
          tableOutput(paste0("groupTable", i))
        )
        
        local({
          my_i <- i
          output[[paste0("groupTable", my_i)]] <- renderTable({
            groups[[my_i]][, "Team", drop = FALSE]
          })
        })
      }
      
      do.call(fluidRow, groupTables)
    })
  })
  
  output$knockoutDiagram <- renderGrViz({
    req(input$generate)
    
    diagram <- "
    digraph knockout_rounds {
      rankdir=TB;
      
      node [shape=box, style=filled, color=lightblue];
      
      Q1 [label='Q1: Winner of Group A\nvs\nRunner up of Group C'];
      Q2 [label='Q2: Winner of Group D\nvs\nRunner up of Group B'];
      Q3 [label='Q3: Winner of Group B\nvs\nRunner up of Group D'];
      Q4 [label='Q4: Winner of Group C\nvs\nRunner up of Group A'];
      
      S1 [label='Semi-final 1'];
      S2 [label='Semi-final 2'];
      F [label='Final'];
      
      edge [arrowhead=none, style=dashed, color=gray];
      
      Q1 -> S1;
      Q2 -> S1;
      Q3 -> S2;
      Q4 -> S2;
      
      S1 -> F;
      S2 -> F;
    }
    "
    
    grViz(diagram)
  })
}
