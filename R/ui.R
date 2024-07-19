library(shiny)
library(DiagrammeR)

ui <- fluidPage(
  titlePanel("Football Competition Pool and Group Creator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("numPools", "Number of Pools:", min = 1, max = 26, value = 4),
      numericInput("numGroups", "Number of Groups:", min = 1, max = 26, value = 4),
      actionButton("createPools", "Create Pools"),
      actionButton("generate", "Generate"),
      uiOutput("poolInputs")
    ),
    
    mainPanel(
      tableOutput("poolTable"),
      uiOutput("groupTables"),
      grVizOutput("knockoutDiagram")
    )
  )
)
