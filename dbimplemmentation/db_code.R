# To generate the header, sidebar, main table body
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyauthr)
library("shinythemes")


ui <- dashboardPage(theme = shinytheme("cerulean"),
  dashboardHeader())
  dashboardSidebar( collapsed = TRUE, 
                    div(htmlOutput("welcome"), style = "padding: 20px"),
                    sidebarMenu(
                      menuItem("View Tables", tabName = "view_table", icon = icon("search")),
                      menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
                      menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
                      menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
                      menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                      menuItem("About", tabName = "about", icon = icon("info-circle")),
                      observe({
                        if(credentials()$user_auth) {
                          shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
                        } else {
                          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
                        }
                      }))),
                      
  
  dashboardBody(  tabItems(
    tabItem(tabName = "view_table", uiOutput("tab1UI")),
    tabItem(tabName = "del_table", uiOutput("tab2UI")),
    tabItem(tabName = "update_table", uiOutput("tab3UI")),
    tabItem(tabName = "create_table", uiOutput("tab4UI")),
    tabItem(tabName = "insert_value", uiOutput("tab5UI")),
    tabItem(tabName = "about", uiOutput("tab6UI"))),
    # Using box as container
    
    output$tab3UI <- renderUI({
      fluidPage(
        fluidRow(
          box(width = 12, collapsible = TRUE, title = "Note:", "")
        ),
        fluidRow(
          box(title = "Rename Table", width = 4, solidHeader = TRUE, status = "primary",
              selectInput(),
              wellPanel(
                textInput(),
                actionButton())
          ),
          box(title = "Rename Column", width = 4, solidHeader = TRUE, status = "primary",
              selectInput(),
              wellPanel()
          ),
          box(title = "Add Column", width = 4, solidHeader = TRUE, status = "primary",
              selectInput(),
              wellPanel()
          )
        )
      )
    }))),


server <- function(input, output) { }

shinyApp(ui, server),

# Sidebar menu items and tabs
# To create icons, use https://shiny.rstudio.com/reference/shiny/0.14/icon.html
# Or use this https://fontawesome.com/icons?from=io
# In the dashboardBody(), the tab items are defined by tabItems(tabItem()). 
# We give each of the tab items an output ID. Therefore, when a user clicks a menu item, 
# it becomes active and its content will be visible in the predefined tab inside the dashboard body.

# To Collapsible sidebar layout



# If the previous code doesn't work, we can try this:

output$tab1UI <- renderUI({
  box(width = NULL, status = "primary",
      sidebarLayout(
        sidebarPanel(
          box(width = 12,
              collapsible = TRUE,
              div(style = "height: 15px; background-color: white;"),
              title = "Database Info:",
              p("")),
          selectInput(),
          textOutput(outputId = "tab_intro"),
          tags$head(tags$style("#tab_intro{font-size: 15px;font-style: italic;}"))
        ),
        mainPanel(
          h4(strong("Table Preview")),
          dataTableOutput(outputId = "sel_table_view")
        )
      )
  )
})

# Authentication - to user log in we need to use shinyauthr package
# The Login form and button is created with shinyauthr::loginUI().

loginUI(id = "login", 
        title = "Please log in", 
        user_title = "User Name",
        pass_title = "Password", 
        login_title = "Log in",
        error_message = "Invalid username or password!",
        additional_ui = NULL)

# To the log out user interface, Once users have logged in, 
# they will see the logout button in the header.
# The Logout button is created with shinyauthr::logoutUI().

logoutUI(id = "logout", 
         label = "Log out", 
         icon = NULL, 
         class = "btn-danger",
         style = "color: white;")


# Login credentials - i will use this example 

user_base <- data.frame(
  username = c("RChenusers", "admin"),
  password = c("rchen1", "rchen2"), 
  password_hash = sapply(c("rchen1", "rchen2"), sodium::password_store), 
  permissions = c("manager", "admin")
)

# logging in and out
# We then put the login and logout UI as well as the user data table in the ui function.
# And we must initiate the package shinyjs with shinyjs::useShinyjs() in our UI code for things to work.
# js code in the source directory.



ui <- dashboardPage(
  dashboardHeader(
    title = "",
    tags$li(),
    tags$li(shinyauthr::logoutUI("logout"))
  ),
  
  dashboardSidebar(),  
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",type="text/javascript"),
              includeScript("returnClick.js")),
    shinyauthr::loginUI("login"),
    uiOutput("user_table")
  )
)


# The script returnClick.js ensures successful login when a user clicks the Enter key.

# To create the reactive output - In the server function, we created login and logout 
# modules that would react to user action and user information.
# In this case, callModule(login) calls the login module. 
# callModule(logout) calls the logout module. Here, callModule() invokes a Shiny module; 
# login() and logout() are Shiny module server functions.

server <- function(input, output, session){
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            reactive(credentials()$user_auth))
  
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = username,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  output$user_table <- renderUI({
    if(credentials()$user_auth) return(NULL)
    fluidRow(column(4,
                    p("Please use the usernames and passwords ...", 
                      class = "text-center", style = "font-size: 15px;"),
                    br(),
                    renderTable({user_base[, -3]}), offset = 4
    )
    )
  })
}


# Dynamic UI reacting to user inputs (UI = User Interface)

user_info <- reactive({credentials()$info})

output$welcome <- renderText({ 
  req(credentials()$user_auth)
  paste("Welcome ","<font color=\"#f3b404\"><b>", {user_info()$permissions}, "</b></font>","!") 
})


# Displaying {text output} based on user selection

table_intro <- list(donors = "Metadata information, like specie, pi, age, gender, left or right side profiled, among others", 
                    samples = "Sample information, donor, tissue, any treatment info, or notes like frozen tissue or others",
                    libraries = "Library preparation, like dissociation technology",
                    sequencing = "Sequencing information, including platform type, bioinformatic info like qc path to server, etc",
                    lab_members = "Email and name of the lab members involved in this project",
                    projects = "Name and description of the project, and members involved",
                    other = "This is a table created by you or other users.")

# To render the tables

output$tab1UI <- renderUI({
  req(credentials()$user_auth)
  box(width = NULL, status = "primary",
      sidebarLayout(
        sidebarPanel(
          box(),
          selectInput(
            inputId = "sel_table_1",
            label = "Tables in Database",
            choices = dbListTables(db),
            selected = "custs"
          ),
          textOutput(outputId = "tab_intro"),
          tags$head(tags$style("#tab_intro{
                               font-size: 15px;
                               font-style: italic;
                               }"))
        ),
        mainPanel()
      )
  )
})

output$sel_table_view <- renderDataTable()

output$tab_intro <- renderText(
  if (input$sel_table_1 %in% c("donors","samples","libraries","sequencing","lab_members", "projects", "other"))
  {table_intro[[input$sel_table_1]]}
  else {table_intro$other}
)

# Rendering {X} number of forms
# The aim is to generate dynamic forms.
# The first group of UI outputs take care of user inputs. 
# We have a text input Table name, a numeric input Number of columns for users to input the number of 
# dynamic forms to be rendered below, and an action button Create table to submit the form. 
# We use box() to house these elements.

output$tab4UI <- renderUI({
  req(credentials()$user_auth)
  box(width = NULL, status = "primary",
      textInput(inputId = "table_name", label = "Table name"),
      numericInput(inputId = "ncols", label = "Number of columns", 1, min = 1),
      uiOutput(outputId = "cols"),
      actionButton(inputId = "create_table", label = "Create table", class = "btn-info", style = "")
  )
})

# For the second group of UI outputs, the dynamic forms will be rendered with uiOutput() and renderUI(). 
# The uiOutput() creates an HTML output element; renderUI() renders reactive HTML.
# For each of these forms, we have a text input Column name and a select input Column type.

output$cols <- renderUI({
  req(input$ncols >= 1)
  cols <- vector("list", input$ncols)
  for (i in seq_len(input$ncols)) {
    cols[[i]] <- box(
      title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
      textInput(inputId = paste0("colName", i), label = "Column name"),
      selectInput(inputId = paste0("colType", i), label = "Column type", 
                  choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN","DATE")
      )
    )
  }
  cols
})

# In the code chunk above, the number of forms to be rendered is defined by the user in the numeric input (id = “ncols”).
# The program then runs through each form cols[[i]] from the first to the last element in the user input ncols.

for (i in seq_len(input$ncols)) {
  cols[[i]] <- box()
}

# The choices in a drop-down menu, or values of a select input, can be dynamically updated in response to user actions.
# For example, in View Tables, the name of the newly created table needs to be added to the drop-down menu Tables in Database, where users select a table to view.

updateSelectInput(session, "sel_table_1", choices = dbListTables(db))

# There are several other select inputs whose values are tables names. 
# These are select inputs for deleting table, renaming column, adding column, renaming table, and inserting values. 
# But in these select inputs, we only allow for newly created tables to be modified. 
# Therefore, in the code block below, we use setdiff() to remove the names of the original tables in the database from the choices in those select inputs.

for (sel_input in c("sel_table_2","sel_table_3","sel_table_3_i","sel_table_3_ii","sel_table_5")){
  updateSelectInput(session, sel_input, choices = setdiff(dbListTables(db),
                                      c("donors","samples","libraries","sequencing","lab_members", "projects")))
}


# When we need to update select inputs of column names, the choices are colnames() instead of dbListTables().
# The example below updates column names in the resulting data extracted from the database, stored in d.

updateSelectInput(session, "sel_col_3", choices = colnames(d))

# Rendering {input} types matched with predefined data types
# This code chunk below generates the input type for users to insert entries that is matched with the predefined data type of a column, 
# and embeds the column type in the title of the form.

output$values <- renderUI({
# UI outputs rendered in the place that has an uiOutput id "values"
  
  req(isTruthy(input$sel_table_5))

  values <- list()
  d <- dbGetQuery(
    conn = db,
    statement = paste0('SELECT * from ',input$sel_table_5)
  )
  typ <- dbGetQuery(
    conn = db, statement = paste0('PRAGMA table_info(',input$sel_table_5,')')
  )
  for (col in colnames(d)) {
    typ_i = typ$type[typ$name==col]
    values[[col]] <- box(
      title = paste0(as.character(col),' (',typ_i,')'), 
      width = 6, solidHeader = TRUE, status = "primary",
      
      if (typ_i == "BOOLEAN") {radioButtons(inputId = paste0("value_", col), label = "Value",
                                            c("TRUE","FALSE") )}
      else if (typ_i == "NUMERIC" | typ_i == "FLOAT" |
               typ_i == "INTEGER" | typ_i == "NUM" ) 
      {numericInput(inputId = paste0("value_", col), label = "Value", value = 0)} 
      else if (typ_i == "DATE") {dateInput(inputId = paste0("value_", col),
                                           label = "Value",
                                           value = "2020-12-01") }
      else {tagList(useShinyFeedback(),
                    textInput(inputId = paste0("value_", col), label = "Value"))}
    )
  }
  values
})

# The PRAGMA table_info statement
# The result from the PRAGMA table_info statement contains one row for each column in the table we are asking about. 
# Columns in the returned data include the column name, data type, whether or not the column can be NULL,
# and the default value for the column. The “pk” column is zero for columns that are not part of the primary key, 
# and is the index of the column in the primary key for columns that are part of the primary key.

################################################################################
#                                                                              #
#                   cid	name	type	notnull	dflt_value	pk                     #
#                     0	id	NUMERIC	0	NA	0                                    #
#                     1	name	VARCHAR(255)	0	NA	0                            #
#                     2	pass	BOOLEAN	0	NA	0                                  #
#                     3	date	DATE	0	NA	0                                    #
#                                                                              #
################################################################################

# embedding column type in the form title
# For each column in the returned data d, we retrieve its data type and add it to the form title.

for (col in colnames(d)) {
  typ_i = typ$type[typ$name==col]
  values[[col]] <- box(
    title = paste0(as.character(col),' (',typ_i,')'), 
    ...
  )
}

if (tolower(input$table_name) %in% tolower(dbListTables(db)) |
    !isTruthy(input$table_name) |
    grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$table_name) == FALSE) {
  showModal(modalDialog(
    title = "Invalid table name",
    "You get this message possibly because:
    1) the table already exists;
    2) the table name is blank;
    or 3) this is an invalid table name.",
    footer = modalButton("OK"), easyClose = TRUE ) )
  return()
}

# make sure there is value in the input 
  if (!isTruthy(input$ncols)) {
    showModal(modalDialog(
      title = "Invalid table name",
      "Please type in the right column number.",
      footer = modalButton("OK"), easyClose = TRUE ) )
  }
  
# make sure the input value of column number is larger than one  
  if (input$ncols < 1) {
    showModal(modalDialog(
      title = "No columns",
      "Each table must have one or more columns.",
      footer = modalButton("OK"), easyClose = TRUE
    )) 
  }

# gather all the colnames into a list
  col_names_list = list()
  for (i in seq_len(input$ncols)) {
    col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
  }
  
# make sure the column name is valid  
  if ( any(col_names_list == "") | 
       sum(duplicated(col_names_list)) > 0 |
       any(grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",col_names_list) == FALSE) |
       any(tolower(col_names_list) %in% sqlite_kw_lo) ) {
    showModal(modalDialog(
      title = "Invalid column name",
      "You get this message possibly because: 
      1) the column name already exists;
      2) the field is blank;
      3) this is an invalid SQLite column name;
      4) the field name conflicts with a SQLite keyword;
      5) Or you are a super human with super powers and we dont known what is happen.",
      footer = modalButton("OK"), easyClose = TRUE
    ))
    return()
  }

# Compiling SQLite queries

#observeEvent(input$create_table, {
# make sure table name is not the same as an existing table in the database, blank, or invalid
#if () {}
# make sure there is value in the input 
#if () {}
# make sure the input value of column number is larger than one  
#else if () {}  
#else {
    # gather all the column names into a list
    # make sure the column name is valid  
#if () {}
      
# compile query
query <- paste0('CREATE TABLE ',input$table_name,' (')
    for (i in seq_len(input$ncols)) { 
      query <- paste0(query,input[[paste0("colName", i)]],' ',input[[paste0("colType", i)]],',')
    }
    query <- paste0(str_sub(query,1,-2),')')
    dbGetQuery(
      conn = db,
      statement = query )
# if successful, update inputs
    updateNumericInput(session, "ncols", value = "1")
    updateTextInput(session, "table_name", value = "")
    for (sel_input in c("sel_table_2","sel_table_3","sel_table_3_i","sel_table_3_ii","sel_table_5")) {
      updateSelectInput(session, sel_input, 
                        choices = setdiff(dbListTables(db),
                                          c("dornos","samples","libraries",
                                            "sequencing","lab_members", "projects")))
    }
    updateSelectInput(session, "sel_table_1", choices = dbListTables(db)) {
    showModal(modalDialog(
      title = "Success",
      "Very nice! The table has been successfully created.",
      footer = modalButton("OK"), easyClose = TRUE ) )
  }


# Some HTML tags
dashboardHeader(
  title = span("RChenLab Database Management Platform", style = "font-size: 20px"),
  titleWidth = 300,
  tags$li(class = "dropdown",tags$a("RChenDB",style = "color: white")),
  tags$li(class = "dropdown",
          tags$a(img(src = "rchenlablogo.png", height = 20),
                 href = "https://rchenlab.github.io/",
                 title = "Check out our lab page!")),
  tags$li(class = "dropdown", style = "padding: 8px;",
          shinyauthr::logoutUI("logout"))
)

# We were also able to add custom CSS class with the tags functions.

textOutput(outputId = "tab_intro")
tags$head(tags$style("#tab_intro{
                   font-size: 15px;
                   font-style: italic;
                   }"))


# We used the modal functions to create modal dialogs.modalDialog() createa a modal dialog UI. showModal() shows a modal dialog.

showModal(modalDialog(
  title = "Invalid column name",
  "You get this message possibly because: 
      1) the column name already exists;
      2) the field is blank;
      3) this is an invalid SQLite column name;
      4) the field name conflicts with a SQLite keyword;
      5) Or you are a super human with super powers and we dont known what is happen.",
  footer = modalButton("OK"), easyClose = TRUE ))




# To make more fancy, we used some js functions  provided by shinyjs package

observe({
  if(credentials()$user_auth) {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  } else {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  }
})




