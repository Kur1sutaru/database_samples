library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyauthr)
library(shinythemes)
library(DBI)
library(RSQLite)

# Define database connection
con <- dbConnect(RSQLite::SQLite(), "database.db")

# Define user authentication
auth <- shinyauthr::auth()

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Database Management Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("View Data", tabName = "view", icon = icon("table")),
      menuItem("Add Data", tabName = "add", icon = icon("plus"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "view",
        fluidRow(
          column(width = 12, dataTableOutput("data_table"))
        )
      ),
      tabItem(
        tabName = "add",
        fluidRow(
          column(width = 12,
            textInput("name", "Name"),
            textInput("age", "Age"),
            actionButton("add_data", "Add Data")
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Function to view data
  view_data <- reactive({
    query <- "SELECT * FROM data"
    dbGetQuery(con, query)
  })
  
  # Render data table
  output$data_table <- renderDataTable({
    view_data()
  })
  
  # Function to add data
  observeEvent(input$add_data, {
    name <- input$name
    age <- input$age
    if (name != "" && age != "") {
      query <- paste0("INSERT INTO data (name, age) VALUES ('", name, "', ", age, ")")
      dbExecute(con, query)
      showModal(modalDialog(
        title = "Data Added",
        "Data has been added to the database."
      ))
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "age", value = "")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter both name and age."
      ))
    }
  })
  
}

# Run app
shinyApp(ui, server, enableBookmark = TRUE, auth = auth)
