library(shiny)
library(DiagrammeR)

ui <- fluidPage(
  titlePanel("Football Competition Pool and Group Creator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("numPools", "Number of Pools:", min = 1, max = 26, value = 4),
      numericInput("numGroups", "Number of Groups:", min = 1, max = 26, value = 4),
      actionButton("createPools", "Create Pools"),
      uiOutput("generateButton"),
      uiOutput("poolInputs")
    ),
    
    mainPanel(
      tableOutput("poolTable"),
      div(id = "group-tables-section", 
          uiOutput("groupTables")
      ),
      grVizOutput("knockoutDiagram")
    )
  )
)
