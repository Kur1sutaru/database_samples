install.packages("shiny")
install.packages("DBI")
install.packages("RSQLite")
library(shiny)
library(DBI)
library(htmltools)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "testsample.db")
dbExecute(con, "CREATE TABLE IF NOT EXISTS testsample (id INTEGER PRIMARY KEY, specie TEXT, age TEXT, gender TEXT, donor TEXT, tissue TEXT, platform TEXT, library_type TEXT, refgenome TEXT, numreads INTEGER, numcells INTEGER, path_taco TEXT)")

dbExecute(con, "INSERT INTO testsample (specie, age, gender, donor, tissue, platform, library_type, refgenome, numreads, numcells, path_taco) VALUES ('Mus musculus', '8 weeks', 'mixed', 'UT Dallas', 'trigeminal', '10x visium', '3v3', 'mm10', '4548422', '24587', '/storage/chen/data_shared_folder/trigeminal')")

ui <- fluidPage(
  titlePanel("Sample Database"),
  dataTableOutput("table")
)

server <- function(input, output) {
  output$table <- renderDataTable({
    dbGetQuery(con, "SELECT * FROM testsample")
  })
}

shinyApp(ui, server)

deployApp("testsample.db")
